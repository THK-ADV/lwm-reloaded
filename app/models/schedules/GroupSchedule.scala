//package models.schedules
//
//import java.util.UUID
//
//import controllers.crud.JsonSerialisation
//import models._
//import play.api.libs.json.{Json, Reads, Writes}
//
//case class GroupSchedule(id: UUID) extends UniqueEntity
//
//case class GroupScheduleProtocol()
//
//// not included associations: []
//
//object GroupSchedule extends UriGenerator[GroupSchedule] with JsonSerialisation[GroupSchedule, GroupSchedule]{
//
//  override implicit def reads: Reads[GroupSchedule] = Json.reads[GroupSchedule]
//
//  override implicit def writes: Writes[GroupSchedule] = Json.writes[GroupSchedule]
//
//  override def base: String = "groupSchedules"
//}
