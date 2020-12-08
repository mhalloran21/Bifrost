package co.topl.http.api.endpoints

import java.security.SecureRandom

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.http.api.{ApiEndpoint, Namespace, UtilNamespace}
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.concurrent.{ExecutionContext, Future}

case class UtilsApiEndpoint (override val settings: RPCApiSettings, appContext: AppContext)
                            (implicit val  ec: ExecutionContext) extends ApiEndpoint {
  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // the namespace for the endpoints defined in handlers
  val namespace: Namespace = UtilNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_seed"            => seedRoute(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_seedOfLength"    => seedOfLength(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_hashBlake2b256"  => hashBlake2b256(params.head, id)
  }

  private def generateSeed (length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Base58.encode(seed)
  }

  /**  #### Summary
    *    Generates random seed of 32 bytes
    * 
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | --None specified--       |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def seedRoute(params: Json, id: String): Future[Json] = {
    val seedSize = 32 // todo: JAA - read this from a more appropriate place. Bip39 spec or something?
    Future(Map("seed" -> generateSeed(seedSize)).asJson)
  }

  /**  #### Summary
    *    Generates random seed of specified length
    * 
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | length                   | Int        	| Required             	| The number of characters to return                                      |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def seedOfLength(params: Json, id: String): Future[Json] = {
    val length: Int = (params \\ "length").head.asNumber.get.toInt.get
    Future(Map("seed" -> generateSeed(length)).asJson)
  }

  /** 
    *  #### Summary
    *    Returns Blake2b hash of specified message
    * 
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | message                  | String     	| Required             	| The message that will be hashed                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def hashBlake2b256(params: Json, id: String): Future[Json] = {
    val message: String = (params \\ "message").head.asString.get
    Future(Map(
      "message" -> message,
      "hash" -> Base58.encode(Blake2b256(message))
    ).asJson)
  }
}