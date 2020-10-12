package co.topl.modifier.transaction

import co.topl.crypto.{FastCryptographicHash, Signature25519}
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{PublicKeyNoncedBox, TokenBox}
import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base58

abstract class TransferTransaction ( val from              : IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                     val to                : IndexedSeq[(PublicKey25519Proposition, Long)],
                                     val signatures        : Map[PublicKey25519Proposition, Signature25519],
                                     override val fee      : Long,
                                     override val timestamp: Long,
                                     val data              : String
                                   ) extends Transaction {

  override val newBoxes: Traversable[TokenBox]

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  lazy val hashNoNonces: Digest32 = FastCryptographicHash(
    to.flatMap(_._1.pubKeyBytes).toArray ++
      //Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      data.getBytes
    )

  def json ( txType: String ): Json =
    Map(
      "txHash" -> id.toString.asJson,
      "txType" -> txType.asJson,
      "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
      "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
      "from" -> from.map { s =>
        Map(
          "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
          "nonce" -> s._2.toString.asJson
          ).asJson
      }.asJson,
      "to" -> to.map { s =>
        Map(
          "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
          "value" -> s._2.toString.asJson
          ).asJson
      }.asJson,
      "signatures" -> signatures
        .map { s =>
          Map(
            "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
            "signature" -> Base58.encode(s._2.signature).asJson
            ).asJson
        }.asJson,
      "fee" -> fee.asJson,
      "timestamp" -> timestamp.asJson,
      "data" -> data.asJson
      ).asJson


  //YT NOTE - removed timestamp and unlockers since that will be updated after signatures are received
  def commonMessageToSign: Array[Byte] =
    to.flatMap(_._1.pubKeyBytes).toArray ++
      newBoxes.foldLeft(Array[Byte]())(( acc, x ) => acc ++ x.bytes)

  Longs.toByteArray(fee) ++
    boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
    data.getBytes
}