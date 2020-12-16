package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import co.topl.utils.Identifiable
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class ExecutionBox(override val evidence   : Evidence,
                        override val nonce      : Box.Nonce,
                        override val value      : ProgramId,
                        stateBoxIds             : Seq[ProgramId],
                        codeBoxIds              : Seq[ProgramId]
                        ) extends ProgramBox(evidence, nonce, value)

object ExecutionBox {
  val boxTypePrefix: BoxType = 11: Byte
  val boxTypeString: String = "ExecutionBox"

  implicit val identifier: Identifiable[ExecutionBox] = new Identifiable[ExecutionBox] {
    override def typePrefix: Byte = boxTypePrefix
    override def typeString: String = boxTypeString
  }

  implicit val jsonEncoder: Encoder[ExecutionBox] = { box: ExecutionBox =>
    (Box.jsonEncode[ProgramId, ExecutionBox](box) ++ Map(
      "stateBoxIds" -> box.stateBoxIds.asJson,
      "codeBoxIds" -> box.codeBoxIds.asJson
    )).asJson
  }

  implicit val jsonDecoder: Decoder[ExecutionBox] = ( c: HCursor ) =>
    for {
      b <- Box.jsonDecode[ProgramId](c)
      stateBoxIds <- c.downField("stateBoxIds").as[Seq[ProgramId]]
      codeBoxIds <- c.downField("codeBoxIds").as[Seq[ProgramId]]
    } yield {
      val (evidence, nonce, programId) = b
      ExecutionBox(evidence, nonce, programId, stateBoxIds, codeBoxIds)
    }
}
