package controllers

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{Actor, ActorRef, Props}
import com.oculusvr.capi.OvrLibrary.ovrTrackingCaps._
import com.oculusvr.capi._
import play.api.Play.current
import play.api.libs.EventSource
import play.api.libs.concurrent.Promise
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller, WebSocket}
import scala.concurrent.duration._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._

object EyeSocketController extends Controller with OculusDevice {

  /**
   * Get Tracking data once
   */
  def info = Action.async {
    printProjection()
    Future.successful(Ok(Json.toJson(getSensorData.asArray())))
  }

  def config = Action.async {
    Future.successful(Ok(getConfigData.toString))
  }

  /**
   * Get Tracking data stream using web socket
   */
  def socket = WebSocket.acceptWithActor[String, String] { request => out =>
    SensorActor.props(out)
  }

  /**
   * Get Tracking data stream using server sent events
   */
  def sse = Action {
    request => {
      /** Creates enumerator and channel for Strings through Concurrent factory object
        * for pushing data through the WebSocket */
      val out = Enumerator.generateM {
        val sensorData = getSensorData.toString
        Promise.timeout(Some(sensorData), 10 milliseconds)
      }
      Ok.feed(out &> EventSource()).as("text/event-stream").withHeaders("Access-Control-Allow-Origin" -> "*")
    }
  }

}

object SensorActor {
  def props(out: ActorRef) = Props(classOf[SensorActor], out)

  def props = Props(classOf[SensorActor])
}

class SensorActor(out: ActorRef) extends Actor with OculusDevice {

  import context._


  var sensorData: OVRSensorData = OVRSensorData()

  def receive = {
    case _ =>
      sendHmdConfig()
      become(updating)
  }

  def updating: Receive = {
    case RefreshSensorData =>
      sensorData = getSensorData
      out ! sensorData.toString
  }

  override def preStart() = {
    context.system.scheduler.schedule(1 second, 100 millisecond, self, RefreshSensorData)(context.dispatcher)
  }

  override def postStop() = {
    println(s"$this Socket disconnected!")
  }

  def sendHmdConfig() = {
    out ! new OVRConfigData().toString
  }
}

object OculusDevice {

  def createFirstHmd(): Hmd = {
    Hmd.initialize()
    val hmd: Hmd = Option(Hmd.create(0)).getOrElse(Hmd.createDebug(OvrLibrary.ovrHmdType.ovrHmd_DK2))
    hmd.configureTracking(ovrTrackingCap_Orientation | ovrTrackingCap_MagYawCorrection | ovrTrackingCap_Position, 0)
    hmd
  }

  def destroy(hmd: Hmd) {
    hmd.destroy()
    Hmd.shutdown()
  }

}

trait OculusDevice {

  import controllers.OculusDevice._

  val hmd: Hmd = createFirstHmd()

  def getSensorData: OVRSensorData = {
//    val trackingState: TrackingState = hmd.getSensorState(Hmd.getTimeInSeconds)
    val trackingState: TrackingState = hmd.getSensorState(0)
    val pos: OvrVector3f = trackingState.HeadPose.Pose.Position
    val quat: OvrQuaternionf = trackingState.HeadPose.Pose.Orientation
    val accel: OvrVector3f = trackingState.HeadPose.AngularAcceleration
    OVRSensorData(pos.x, pos.y, pos.z, accel.x, accel.y, accel.z, quat.x, quat.y, quat.z, quat.w)
  }

  def getConfigData: OVRConfigData = {
    val trackingState: TrackingState = hmd.getSensorState(Hmd.getTimeInSeconds)
    val pos: OvrVector3f = trackingState.HeadPose.Pose.Position
    val quat: OvrQuaternionf = trackingState.HeadPose.Pose.Orientation
    val accel: OvrVector3f = trackingState.HeadPose.AngularAcceleration
    println(
      s"""
        | hmd
        | fovport [${hmd.DefaultEyeFov.mkString(",")}]
        | maxEyeFov [${hmd.MaxEyeFov.mkString(",")}]
        | """.stripMargin)
    //OVRConfigData(fov, ipd, lensDistance, eyeToScreen, distortionValues, screenSize, screenResolution)
    new OVRConfigData()
  }

  def printProjection(): Unit = {
    hmd.MaxEyeFov.foreach { fov =>
      val projection: OvrMatrix4f = Hmd.getPerspectiveProjection(fov, 0.01f, 10000.0f, true)
      println(
        f"""
           |projection matrix:
           | [${projection.M(0)}%.12f  ${projection.M(1)}%.12f  ${projection.M(2)}%.12f  ${projection.M(3)}]
           | [${projection.M(4)}%.12f  ${projection.M(5)}%.12f  ${projection.M(6)}%.12f  ${projection.M(7)}]
           | [${projection.M(8)}%.12f  ${projection.M(9)}%.12f  ${projection.M(10)}%.12f  ${projection.M(11)}]
           | [${projection.M(12)}%.12f  ${projection.M(13)}%.12f  ${projection.M(14)}%.12f  ${projection.M(15)}]
         """.stripMargin)
    }
  }
}

case object GetSensorData

case object RefreshSensorData

object OVRSensorData {
  val counter: AtomicLong = new AtomicLong(0)

  def apply(): OVRSensorData = {
    new OVRSensorData(counter.getAndIncrement)
  }

  def apply(px: Double,
            py: Double,
            pz: Double,
            ax: Double,
            ay: Double,
            az: Double,
            qx: Double,
            qy: Double,
            qz: Double,
            qw: Double): OVRSensorData = {
    new OVRSensorData(counter.getAndIncrement, px, py, pz, ax, ay, az, qx, qy, qz, qw)
  }
}

case class OVRSensorData(id: Long = 0,
                         px: Double = 0,
                         py: Double = 0,
                         pz: Double = 0,
                         ax: Double = 0,
                         ay: Double = 0,
                         az: Double = 0,
                         qx: Double = 0,
                         qy: Double = 0,
                         qz: Double = 0,
                         qw: Double = 0) {

  def asArray(): Array[Double] = Array(id, px, py, pz, ax, ay, az, qx, qy, qz, qw)

  override def toString: String = {
    "{ \"m\" : \"update\", \"o\" : [" + List(qw, qx, qy, qz).mkString(",") + "], \"a\" : [" + List(ax, ay, az).mkString(",") + "] }"
  }

}

case class OVRConfigData(fov: Float = 130.7f, //125.871f,
                         ipd: Float = 0.064f,
                         lensDistance: Float = 0.0635f,
                         eyeToScreen: Float = 0.019f,
                         distortionValues: OvrQuaternionf = new OvrQuaternionf(1f, .22f, .24f, 0f),
                         screenSize: OvrVector2f = new OvrVector2f(0.12576f, 0.07074f),
                         screenResolution: OvrSizei = new OvrSizei(1920, 1080)) {

  override def toString: String = "{ \"m\" : \"config\", \"fov\" : " + fov +
    ", \"interpupillaryDistance\" : " + ipd +
    ", \"eyeToScreen\" : " + eyeToScreen +
    ", \"lensDistance\" : " + lensDistance +
    ", \"distortion\" : [" + List(distortionValues.x, distortionValues.y, distortionValues.z, distortionValues.w).mkString(",") +
    "], \"screenSize\" : [" + List(screenSize.x, screenSize.y).mkString(",") +
    "], \"screenResolution\" : [" + List(screenResolution.w, screenResolution.h).mkString(",") + "] }"

}


