package http

import akka.actor.{ActorNotFound, ActorRef, ActorRefFactory, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import attestation.Address
import attestation.AddressEncoder.NetworkPrefix
import crypto.AssetCode
import requests.{ApiRoute, Requests, RequestsManager}
import io.circe.Json
import io.circe.syntax._
import keymanager.KeyManager._
import modifier.AssetValue
import settings.RPCApiSettings
import utils.Logging
import wallet.WalletManager.{ConnectToBifrost, DisconnectFromBifrost, GetConnection}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

case class GjallarhornOnlineApiRoute(settings: RPCApiSettings,
                                     keyManager: ActorRef,
                                     walletManager: ActorRef,
                                     requests: Requests)
                                    (implicit val context: ActorRefFactory, system: ActorSystem)
  extends ApiRoute with Logging {

  val namespace: Namespace = OnlineWalletNamespace
  implicit val networkPrefix: NetworkPrefix = _root_.keymanager.networkPrefix

  private var requestsManager: Option[ActorRef] = None

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_connectToBifrost" => connectToBifrost(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_disconnectFromBifrost" => disconnectFromBifrost(id)
    case (method, params, id) if method == s"${namespace.name}_getConnection" => getConnection(id)

    case (method, params, id) if method == s"${namespace.name}_createTransaction" => createTransaction(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_broadcastTx"      => broadcastTx(params.head, id)
  }

  /** #### Summary
    * Connect To Bifrost
    *
    * #### Description
    * Attempts to connect to Bifrost with the given chain provider.
    * ---
    * #### Params
    *
    * | Fields | Data type | Required / Optional | Description |
    * | ---| ---	| --- | --- |
    * | chainProvider | String	| Required | Chain provider corresponding to the akka remote port and hostname that bifrost is running on.|
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - either connectedToBifrost -> true or error message if unable to connect to the given chain provider.
    */
  private def connectToBifrost(params: Json, id: String): Future[Json] = {
    val chainProvider = (params \\ "chainProvider").head.asString.get
    try {
      log.info("gjallarhorn attempting to run in online mode. Trying to connect to Bifrost...")
      val bifrost = Await.result(system.actorSelection(s"akka.tcp://$chainProvider/user/walletConnectionHandler")
        .resolveOne(), 10.seconds)
      log.info(s"${Console.MAGENTA} Bifrst actor ref was found: $bifrost ${Console.RESET}. " +
        s"Now running in online mode.")
      setUpOnlineMode(bifrost)
    } catch {
      case e: ActorNotFound =>
        log.error(s"${Console.MAGENTA} bifrost actor ref not found at: akka.tcp://$chainProvider.${Console.RESET}")
        throw new Exception (s"could not connect to chain provider: $chainProvider. $e")
    }
  }

  /**
    * Helper function when connecting to Bifrost
    * Sets up the walletManager, requestManager, and keyManager
    * @param bifrost - the actor ref for Bifrost
    * @return - Json(connectedToBifrost -> true)
    */
  def setUpOnlineMode(bifrost: ActorRef): Future[Json] = {
    walletManager ! ConnectToBifrost(bifrost)

      val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrost)), name = "RequestsManager")
      requestsManager = Some(requestsManagerRef)
      requests.switchOnlineStatus(requestsManager)
      Future{Map("connectedToBifrost" -> true).asJson}
  }

  /** #### Summary
    * Disconnect From Bifrost
    *
    * #### Description
    * Disconnects from Bifrost and kills the walletManager and requestsManager actors.
    * ---
    * #### Params
    * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    * | ------------------------	| ----------	| --------------------	| -----------------------------------------------------------------------	  |
    * | --None specified--       |           	|                     	|                                                                         |
    *
    * @param id     request identifier
    * @return - status message
    */
  private def disconnectFromBifrost(id: String): Future[Json] = {
    var responseMsg = "Disconnected!"
    walletManager ! DisconnectFromBifrost

    requestsManager match {
      case Some(actor) =>
        requestsManager = None
        actor ! PoisonPill
      case None => responseMsg = "Already disconnected from Bifrost"
    }

    requests.switchOnlineStatus(requestsManager)
    log.info(s"${Console.MAGENTA}Gjallarhorn has been disconnected from Bifrost.${Console.RESET}")
    Future{Map("status" -> responseMsg).asJson}
  }

  /** #### Summary
    * Get Connection
    *
    * #### Description
    * Tells whether Gjallarhorn is currently connected to bifrost or not.
    * ---
    * #### Params
    * | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    * | ------------------------	| ----------	| --------------------	| -----------------------------------------------------------------------	  |
    * | --None specified--       |           	|                     	|                                                                         |
    *
    * @param id     request identifier
    * @return - "connectedToBifrost" -> true or false
    */
  private def getConnection(id: String): Future[Json] = {
    (walletManager ? GetConnection).mapTo[Option[ActorRef]].map {
      case Some(actor) => Map("connectedToBifrost" -> true).asJson
      case None => Map("connectedToBifrost" -> false).asJson
    }
  }

  private def createAssetCode(params: Json): Json = {
    (for {
      rcpts <- (params \\ "recipients").head.as[IndexedSeq[Address]]
      quantity <- (params \\ "amount").head.as[Long]
      issuer <- (params \\ "issuer").head.as[Address]
      shortName <- (params \\ "shortName").head.as[String]
    } yield {
      //TODO: what should assetCode version be?
      Try(AssetCode(1.toByte, issuer, shortName)) match {
        case Success(assetCode) =>
          val recipients: IndexedSeq[(Address, AssetValue)] = rcpts.map(addr =>
            (addr, modifier.AssetValue(quantity, assetCode))
          ).toIndexedSeq
          params.deepMerge(Map("recipients" -> recipients).asJson)
        case Failure(exception) =>
          throw new Exception(s"Unable to generate asset code: $exception")
      }
    }) match {
      case Right(value) => value
      case Left(ex) => throw new Exception(s"error parsing signing keys: $ex")
    }
  }

  /** #### Summary
    * Create Transaction
    *
    * #### Description
    * Either creates a raw transaction if offline (online = false) or creates, signs, and broadcasts a transaction if online = true
    * ---
    * #### Params
    *
    * | Fields | Data type | Required / Optional | Description |
    * | ---| ---	| --- | --- |
    * | method | String	| Required | The method to send request to Bifrost ("topl_raw[Asset, Arbit, or Poly]Transfer")|
    * | params | Json	| Required | The params for the method to send to Bifrost|
    * In the inner params:
    * | online | Boolean	| Required | Defines whether to send online transaction or create raw transaction offline.|
    * | sender | IndexedSeq[Address]	| Required | Array of addresses from which assets should be sent |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return - a response after creating transaction.
    */
  private def createTransaction(params: Json, id: String): Future[Json] = {
    var response = Future{"tx".asJson}
    for {
      method <- (params \\ "method").head.as[String]
      innerParams <- (params \\ "params").head.asArray.get.head.as[Json]
      online <- (innerParams \\ "online").head.as[Boolean]
      sender <- (innerParams \\ "sender").head.as[IndexedSeq[Address]]
    } yield {
      var params = innerParams
      if (method == "topl_rawAssetTransfer") {
        params = createAssetCode(innerParams)
      }
      val tx = requests.transaction(method, params)
      if (online) {
        val txResponse = requests.sendRequest(tx)
        val rawTx = (txResponse \\ "rawTx").head
        val msgToSign = (txResponse \\ "messageToSign").head.asString.get
        val signedTx = Await.result((keyManager ? SignTx(rawTx, sender, msgToSign))
          .mapTo[Json], 10.seconds)
        response = Future{(requests.broadcastTx(signedTx) \\ "result").head}
      } else {
        response = Future {(requests.sendRequest(tx) \\ "result").head}
      }
    }
    response
  }


  /** #### Summary
    * Broadcast transaction
    *
    * #### Description
    * Sends broadcast request to Bifrost to broadcast signed transaction.
    *
    * #### Notes
    *    - Currently only enabled for `AssetCreation` and `AssetTransfer` transactions
    *      ---
    *      #### Params
    *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	      |
    *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	      |
    *      | tx                  	    | object     	| Required            	| A full formatted transaction JSON object (prototype transaction + signatures) |
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def broadcastTx(params: Json, id: String): Future[Json] = {
    requestsManager match {
      case Some(actor) =>
        val signedTx = (params \\ "params").head.asArray.get.head
        Future{(requests.broadcastTx(signedTx) \\ "result").head}
      case None => offlineMessage()
    }
  }

  /**
    * API response for error handling - when request comes in that requires Bifrost but Gjallarhorn is offline.
    * @return - error msg
    */
  private def offlineMessage(): Future[Json] = {
    val msg = "cannot send request because you are offline mode " +
      "or the chain provider provided was incorrect."
    throw new Exception(msg)
  }

}

