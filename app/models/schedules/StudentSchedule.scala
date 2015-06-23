//package models.schedules
//
//import java.util.UUID
//
//import controllers.crud.JsonSerialisation
//import models._
//import play.api.libs.json.{Json, Reads, Writes}
//
//case class StudentSchedule(id: UUID) extends UniqueEntity
//
//case class StudentScheduleProtocol()
//
//// not included associations: []
//
//object StudentSchedule extends UriGenerator[StudentSchedule] with JsonSerialisation[StudentSchedule, StudentSchedule] {
//
//  override implicit def reads: Reads[StudentSchedule] = Json.reads[StudentSchedule]
//
//  override implicit def writes: Writes[StudentSchedule] = Json.writes[StudentSchedule]
//
//  override def base: String = "studentSchedules"
//}