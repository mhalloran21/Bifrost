package co.topl.modifier.block

import co.topl.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block._
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.nodeView.BifrostNodeViewModifier
import co.topl.nodeView.history.History
import co.topl.nodeView.state.box.ArbitBox
import co.topl.utils.serialization.BifrostSerializer
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import scorex.crypto.encode.Base58
import supertagged.@@
// fixme: JAA 0 2020.07.19 - why is protobuf still used here?
import serializer.BloomTopics

import scala.collection.BitSet

/**
 * A block is an atomic piece of data network participates are agreed on.
 *
 * A block has:
 * - transactional data: a sequence of transactions, where a transaction is an atomic state update.
 * Some metadata is possible as well(transactions Merkle tree root, state Merkle tree root etc).
 *
 * - consensus data to check whether block was generated by a right party in a right way. E.g.
 * "baseTarget" & "generatorSignature" fields in the Nxt block structure, nonce & difficulty in the
 * Bitcoin block structure.
 *
 * - a signature(s) of a block generator(s)
 *
 * - additional data: block structure version no, timestamp etc
 */

case class Block ( parentId : BlockId,
                   timestamp: Timestamp,
                   forgerBox: ArbitBox,
                   signature: Signature25519,
                   txs      : Seq[Transaction],
                   version  : Version
                 ) extends BifrostNodeViewModifier {

  type M = Block

  lazy val id: BlockId = ModifierId(FastCryptographicHash(messageToSign))

  lazy val modifierTypeId: ModifierTypeId = Block.modifierTypeId

  lazy val transactions: Option[Seq[Transaction]] = Some(txs)

  lazy val serializer: BifrostSerializer[Block] = BlockSerializer

  lazy val json: Json = Block.jsonEncoder(this)

  lazy val messageToSign: Array[Byte] = {
    val noSigCopy = this.copy(signature = Signature25519(Array.empty[Byte]))
    serializer.toBytes(noSigCopy)
  }
}



object Block {

  type BlockId = ModifierId
  type Timestamp = Long
  type Version = Byte

  val blockIdLength: Int = NodeViewModifier.ModifierIdSize
  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (3: Byte)
  val signatureLength: Int = Signature25519.SignatureSize

  implicit val jsonEncoder: Encoder[Block] = { b: Block ⇒
    Map(
      "id" -> b.id.toString.asJson,
      "parentId" -> b.id.toString.asJson,
      "timestamp" -> b.timestamp.asJson,
      "generatorBox" -> b.forgerBox.asJson,
      "signature" -> b.signature.asJson,
      "txs" -> b.txs.map(_.json).asJson,
      "version" -> b.version.asJson,
      "blockSize" -> b.serializer.toBytes(b).length.asJson
      ).asJson
  }

  implicit val jsonDecoder: Decoder[Block] = (c: HCursor) =>
    for {
      parentId <- c.downField("parentId").as[ModifierId]
      timestamp <- c.downField("timestamp").as[Timestamp]
      generatorBox <- c.downField("generatorBox").as[ArbitBox]
      signature <- c.downField("signature").as[Signature25519]
      txsSeq <- c.downField("txs").as[Seq[Transaction]]
      version <- c.downField("version").as[Byte]
    } yield {
      Block(parentId, timestamp, generatorBox, signature, txsSeq, version)
    }

  /**
   *
   * @param parentId
   * @param timestamp
   * @param txs
   * @param box
   * @param privateKey
   * @param version
   * @return
   */
  def create ( parentId  : BlockId,
               timestamp : Timestamp,
               txs       : Seq[Transaction],
               box       : ArbitBox,
               privateKey: PrivateKey25519,
               version   : Version
             ): Block = {

    assert(box.proposition == privateKey.publicImage)

    // generate block message (block with empty signature) to be signed
    val block = Block(parentId, timestamp, box, Signature25519(Array.empty[Byte]), txs, version)

    // generate signature from the block message and private key
    val signature =
      if (parentId == History.GenesisParentId) Signature25519(Array.empty[Byte]) // genesis block will skip signature check
      else privateKey.sign(block.messageToSign)

    // return a valid block with the signature attached
    block.copy(signature = signature)
  }

  /**
   *
   * @param txs
   * @return
   */
  def createBloom ( txs: Seq[Transaction] ): Array[Byte] = {
    val bloomBitSet = txs.foldLeft(BitSet.empty)(
      ( total, b ) =>
        b.bloomTopics match {
          case Some(e) => total ++ Bloom.calcBloom(e.head, e.tail)
          case None    => total
        }
      ).toSeq
    BloomTopics(bloomBitSet).toByteArray
  }
}
