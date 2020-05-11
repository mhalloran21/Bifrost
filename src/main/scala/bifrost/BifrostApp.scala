package bifrost

import java.lang.management.ManagementFactory

import akka.actor.{ActorRef, Props}
import bifrost.api.http.{ApiRoute, UtilsApiRoute, _}
import bifrost.crypto.PrivateKey25519
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.modifier.block.Block
import bifrost.modifier.box.BifrostBox
import bifrost.modifier.box.proposition.ProofOfKnowledgeProposition
import bifrost.modifier.transaction.bifrostTransaction.BifrostTransaction
import bifrost.network.{BifrostLocalInterface, BifrostNodeViewSynchronizer, BifrostSyncInfoMessageSpec}
import bifrost.network.message.MessageSpec
import bifrost.scorexMod.GenericApplication
import com.sun.management.HotSpotDiagnosticMXBean
import com.typesafe.config.{Config, ConfigFactory}
import io.circe
import kamon.Kamon

import scala.reflect.runtime.universe._

class BifrostApp(val settingsFilename: String) extends GenericApplication with Runnable {

  override type P = ProofOfKnowledgeProposition[PrivateKey25519]
  override type BX = BifrostBox
  override type TX = BifrostTransaction
  override type PMOD = Block
  override type NVHT = BifrostNodeViewHolder

  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] =
    Seq(BifrostSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new NVHT(settings)), "nodeViewHolder")

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef), "forger")

  override val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings), "localInterface"
  )

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostNodeViewSynchronizer],
      networkController,
      nodeViewHolderRef,
      localInterface,
      BifrostSyncInfoMessageSpec), "nodeViewSynchronizer"
  )

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ProgramApiRoute(settings, nodeViewHolderRef, networkController),
    AssetApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
//    GenericNodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
//    PeersApiRoute(peerManagerRef, networkController, settings),
    NodeViewApiRoute(settings, nodeViewHolderRef)
  )

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute],
                                         typeOf[DebugApiRoute],
                                         typeOf[WalletApiRoute],
                                         typeOf[ProgramApiRoute],
                                         typeOf[AssetApiRoute],
//                                         typeOf[GenericNodeViewApiRoute[P, TX]],
//                                         typeOf[PeersApiRoute],
                                         typeOf[NodeViewApiRoute])



  // Am I running on a JDK that supports JVMCI?
  val vm_version = System.getProperty("java.vm.version")

  System.out.printf("java.vm.version = %s%n", vm_version)

  val bean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
  // Is JVMCI enabled?
  val enableJVMCI = bean.getVMOption("EnableJVMCI")
  System.out.println(enableJVMCI)

  // Is the system using the JVMCI compiler for normal compilations?
  val useJVMCICompiler = bean.getVMOption("UseJVMCICompiler")
  System.out.println(useJVMCICompiler)

  // What compiler is selected?
  val compiler = System.getProperty("jvmci.Compiler")
  System.out.printf("jvmci.Compiler = %s%n", compiler)
}

object BifrostApp extends App {
  private val conf: Config = ConfigFactory.load("application")
  if (conf.getBoolean("kamon.enable"))
    Kamon.init()
  val settingsFilename = args.headOption.getOrElse("testnet-private.json")
  new BifrostApp(settingsFilename).run()
}