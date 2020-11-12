package bifrost.modifier.box.serialization

import java.util.UUID

import bifrost.modifier.box.{ExecutionBox, ProgramBox}
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Longs

object ExecutionBoxSerializer extends BifrostSerializer[ExecutionBox] {

  override def serialize(obj: ExecutionBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* stateBoxUUIDs: Seq[UUID], List of uuids of state boxes from ProgramBoxRegistry */
    println(s"\n>>>>>>>>>>>>>>>>>>>>> input: ${obj.stateBoxUUIDs.length}")
    w.putUInt(obj.stateBoxUUIDs.length)
    obj.stateBoxUUIDs.foreach { id =>
      val h1 = Longs.toByteArray(id.getMostSignificantBits)
      val h2 = Longs.toByteArray(id.getLeastSignificantBits)
      w.putBytes(h1 ++ h2)
    }

    /* codeBoxIds: Seq[Array[Byte]] */
    w.putUInt(obj.codeBoxIds.length)
    obj.codeBoxIds.foreach{id =>
      w.putUInt(id.length)
      w.putBytes(id)
    }
  }

  override def parse(r: Reader): ExecutionBox = {
    val programBox: ProgramBox = ProgramBoxSerializer.parse(r)

    /* stateBoxUUIDs: Seq[UUID], List of uuids of state boxes from ProgramBoxRegistry */
    val stateBoxUUIDsLength: Int = r.getUInt().toIntExact
    println(s"\n>>>>>>>>>>>>>>>>>>>>> output: ${stateBoxUUIDsLength}")
    val stateBoxUUIDs: Seq[UUID] = (0 until stateBoxUUIDsLength).map(i => {
      println(s">>>>>>>>>>>>>>>>>>>>> output iter: $i")
      val h1 = Longs.fromByteArray(r.getBytes(8))
      val h2 = Longs.fromByteArray(r.getBytes(8))
      new UUID(h1, h2)
    })

    /* codeBoxIds: Seq[Array[Byte]] */
    val codeBoxIdsLength: Int = r.getUInt().toIntExact

    val codeBoxIds: Seq[Array[Byte]] = (0 until codeBoxIdsLength).map{_ =>
      val idLength: Int = r.getUInt().toIntExact
      r.getBytes(idLength)
    }

    ExecutionBox(programBox.proposition, programBox.nonce, programBox.value, stateBoxUUIDs, codeBoxIds)
  }
}
