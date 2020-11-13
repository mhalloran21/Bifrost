package co.topl

import co.topl.attestation.proof.{Proof, SignatureCurve25519, ThresholdSignatureCurve25519}
import co.topl.attestation.proposition.{Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import io.circe.syntax.EncoderOps
import io.circe.{ACursor, DecodingFailure, Json}

package object attestation {
  def jsonDecoder[P <: Proposition, PR <: Proof[P]] (propType: String, attestation: ACursor): Either[DecodingFailure, Map[P, PR]] = {
    (propType match {
      case "PublicKeyCurve25519" => attestation.as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]]
      case "ThresholdCurve25519" => attestation.as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]]
    }).map {
      case att: Map[P @unchecked, PR @unchecked] => att
    }
  }

  def jsonEncoder[P <: Proposition, PR <: Proof[P]](att: Map[P, PR]): Json =
    att.map {
      case (prop, proof) => prop.toString -> proof.toString
    }.asJson
}
