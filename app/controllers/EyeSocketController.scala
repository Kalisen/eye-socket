package controllers

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{Actor, ActorRef, Props}
import com.oculusvr.capi.OvrLibrary.ovrTrackingCaps._
import com.oculusvr.capi._
import play.api.Play.current
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller, WebSocket}

import scala.concurrent.Future

object EyeSocketController extends Controller with OculusDevice {

  def socket = WebSocket.acceptWithActor[String, String] { request => out =>
    SensorActor.props(out)
  }

  def index = Action.async {
    var trackingState: TrackingState = null
    trackingState = hmd.getSensorState(Hmd.getTimeInSeconds)

    val pos: OvrVector3f = trackingState.HeadPose.Pose.Position
    val quat: OvrQuaternionf = trackingState.HeadPose.Pose.Orientation

    val latestData = OVRSensorData(pos.x, pos.y, pos.z, 0, 0, 0, quat.x, quat.y, quat.z, quat.w)

    Future.successful(Ok(Json.toJson(latestData.asArray())))
  }

}

object SensorActor {
  def props(out: ActorRef) = Props(classOf[SensorActor], out)
}

class SensorActor(out: ActorRef) extends Actor with OculusDevice {

  import context._

import scala.concurrent.duration._

  var sensorData: OVRSensorData = OVRSensorData()

  def receive = {
    case _ =>
      sendHmdConfig()
      become(updating)
      context.system.scheduler.scheduleOnce(1 second, self, RefreshSensorData)(context.dispatcher)
      self ! GetSensorData
  }

  def updating: Receive = {
    case RefreshSensorData =>
      val trackingState: TrackingState = hmd.getSensorState(Hmd.getTimeInSeconds)
      val pos: OvrVector3f = trackingState.HeadPose.Pose.Position
      val quat: OvrQuaternionf = trackingState.HeadPose.Pose.Orientation
      val accel: OvrVector3f = trackingState.HeadPose.AngularAcceleration
      val latestData = OVRSensorData(pos.x, pos.y, pos.z, accel.x, accel.y, accel.z, quat.x, quat.y, quat.z, quat.w)
      sensorData = latestData
      context.system.scheduler.scheduleOnce(10 milliseconds, self, RefreshSensorData)(context.dispatcher)
    case _ =>
      println(s"Sensor Data: ${sensorData.toString}")
      out ! sensorData.toString
      context.system.scheduler.scheduleOnce(10 milliseconds, self, GetSensorData)(context.dispatcher)
  }

  override def preStart() = {
    context.system.scheduler.scheduleOnce(1 second, self, GetSensorData)(context.dispatcher)
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
    val hmd: Hmd = Option(Hmd.create(0)).getOrElse(Hmd.createDebug(OvrLibrary.ovrHmdType.ovrHmd_DK1))
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
    "{ \"m\" : \"update\", \"o\" : [" + List(qx, qy, qz, qw).mkString(",") + "], \"a\" : [" + List(ax, ay, az).mkString(",") + "] }"
  }

}

case class OVRConfigData( fov: Float = 125.871f,
                          ipd: Float = 0.0635f,
                          lensDistance: Float = 0.063f,
                          eyeToScreen: Float = 0.016f,
                          distortionValues: OvrQuaternionf = new OvrQuaternionf(1f, .22f, .24f, 0f),
                          screenSize: OvrVector2i = new OvrVector2i(1920, 1080),
                          screenResolution: OvrSizei = new OvrSizei(1920, 1080)) {

  override def toString: String = "{ \"m\" : \"config\", \"fov\" : " + fov +
    ", \"interpupillaryDistance\" : " + ipd +
    ", \"eyeToScreen\" : " + eyeToScreen +
    ", \"lensDistance\" : " + lensDistance +
    ", \"distortion\" : [" + List(distortionValues.x, distortionValues.y, distortionValues.z, distortionValues.w).mkString(",") +
    "], \"screenSize\" : [" + List(screenSize.x, screenSize.y).mkString(",") +
    "], \"screenResolution\" : [" + List(screenResolution.w, screenResolution.h).mkString(",") + "] }"

}


