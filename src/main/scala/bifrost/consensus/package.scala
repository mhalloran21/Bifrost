package bifrost

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import com.google.common.primitives.Longs

import scala.concurrent.duration._
import scala.math.{max, min}

package object consensus {
  // TODO: JAA - 2020.07.21 - This is the maximum number of Arbits that are issued. It probably shouldn't be
  // TODO: hard-coded
  // these variables are left as vars since they need to be determined at runtime from the network config
  private val MaxTarget: Long = 5000000000L
  private var targetBlockTime: FiniteDuration = FiniteDuration(5, "seconds")

  def setBlockTime(blockTime: FiniteDuration): Unit = {
    this.targetBlockTime = blockTime
  }

  /**
   * Defines how we calculate the test value for determining eligibility to forge
   *
   * @param lastBlock previous block
   * @param box       box to be used for the test value
   * @return the test value to be compared to the adjusted difficulty
   */
  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)

    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  /**
   * Calculates the adjusted difficulty for forging based on the time passed since the previous block
   *
   * @param parent         previous block
   * @param baseDifficulty base difficulty of the parent block
   * @param timestamp      the current timestamp
   * @return the adjusted difficulty
   */
  def calcAdjustedTarget(parent: Block,
                         baseDifficulty: Long,
                         timestamp: Long): BigDecimal = {

    val target: Double = baseDifficulty.toDouble / MaxTarget.toDouble
    val timeDelta = timestamp - parent.timestamp
    require(timeDelta > 0)

    BigDecimal(target * timeDelta.toDouble / targetBlockTime.toUnit(MILLISECONDS))
  }

  /**
    * Calculate the block difficulty according to
    * https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29
    *
    * @param prevDifficulty the previous base difficulty
    * @param prevTimes      sequence of block times to calculate the average and compare to target
    * @return the modified difficulty
    */
  def calcNewBaseDifficulty(prevDifficulty: Long, prevTimes: Seq[Block.Timestamp]): Long = {
    val averageDelay = (prevTimes drop 1, prevTimes).zipped.map(_-_).sum / (prevTimes.length - 1)
    val targetTime = targetBlockTime.toUnit(MILLISECONDS)

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTime) {
      (prevDifficulty * min(averageDelay, targetTime * 1.1) / targetTime).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay, targetTime * 0.9) / targetTime) ))).toLong
    }
  }
}
