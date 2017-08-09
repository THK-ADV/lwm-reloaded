package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}
import models.LwmDateTime.DateTimeConverter

/**
  * A unary permission.
  *
  * @param value Raw permission label
  */

case class SesamePermission(value: String) {
  override def toString: String = value
}

object SesamePermission extends JsonSerialisation[SesamePermission, SesamePermission, SesamePermission] {

  override implicit def reads: Reads[SesamePermission] = Json.reads[SesamePermission]

  override def writesAtom: Writes[SesamePermission] = writes

  override implicit def writes: Writes[SesamePermission] = Json.writes[SesamePermission]
}

case class PostgresPermission(value: String, description: String, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PermissionDb(value: String, description: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresPermission(value, description, id)
}

case class PostgresPermissionProtocol(value: String, description: String)

object PostgresPermission extends JsonSerialisation[PostgresPermissionProtocol, PostgresPermission, PostgresPermission] {

  override implicit def reads: Reads[PostgresPermissionProtocol] = Json.reads[PostgresPermissionProtocol]

  override implicit def writes: Writes[PostgresPermission] = Json.writes[PostgresPermission]

  override implicit def writesAtom: Writes[PostgresPermission] = writes
}

object PermissionDb {
  def from(protocol: PostgresPermissionProtocol, existingId: Option[UUID]) = {
    PermissionDb(protocol.value, protocol.description, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID()))
  }
}

/*
  missing permissions directly implicate either admin or god and are therefore not modelled
 */
object Permissions {

  lazy val all = room.all ++ degree.all ++ course.all ++
    labwork.all ++ labworkApplication.all ++ authority.all ++
    role.all ++ schedule.all ++ scheduleEntry.all ++
    timetable.all ++ semester.all ++ group.all ++
    user.all ++ blacklist.all ++ entryType.all ++ reportCardEntry.all ++
    reportCardEntryType.all ++ reportCardEvaluation.all ++ assignmentPlan.all ++
    annotation.all

  val god = SesamePermission("with great power comes great responsibility")
  val prime = SesamePermission("Prime")

  object room {
    lazy val all = Set(getAll, get)
    val getAll = SesamePermission("Room:getAll")
    val get = SesamePermission("Room:get")
  }

  object degree {
    lazy val all = Set(getAll, get)
    val getAll = SesamePermission("Degree:getAll")
    val get = SesamePermission("Degree:get")
  }

  object course {
    lazy val all = Set(create, update, getAll, get)
    val create = SesamePermission("Course:create")
    val update = SesamePermission("Course:update")
    val getAll = SesamePermission("Course:getAll")
    val get = SesamePermission("Course:get")
  }

  object labwork {
    lazy val all = Set(create, update, getAll, get)
    val create = SesamePermission("Labwork:create")
    val update = SesamePermission("Labwork:update")
    val getAll = SesamePermission("Labwork:getAll")
    val get = SesamePermission("Labwork:get")
  }

  object labworkApplication {
    lazy val all = Set(create, update, delete, get, getAll)
    val create = SesamePermission("LabworkApplication:create")
    val update = SesamePermission("LabworkApplication:update")
    val delete = SesamePermission("LabworkApplication:delete")
    val get = SesamePermission("LabworkApplication:get")
    val getAll = SesamePermission("LabworkApplication:getAll")
  }

  object authority {
    lazy val all = Set(create, delete, getAll, get)
    val create = SesamePermission("Authority:create")
    val delete = SesamePermission("Authority:delete")
    val getAll = SesamePermission("Authority:getAll")
    val get = SesamePermission("Authority:get")
  }

  object role {
    lazy val all = Set(getAll, get)
    val getAll = SesamePermission("Role:getAll")
    val get = SesamePermission("Role:get")
  }

  object schedule {
    lazy val all = Set(create, delete, get)
    val create = SesamePermission("Schedule:create")
    val delete = SesamePermission("Schedule:delete")
    val get = SesamePermission("Schedule:get")
  }

  object scheduleEntry {
    lazy val all = Set(update, get, getAll)
    val update = SesamePermission("ScheduleEntry:update")
    val get = SesamePermission("ScheduleEntry:get")
    val getAll = SesamePermission("ScheduleEntry:getAll")
  }

  object timetable {
    lazy val all = Set(create, update, delete, get, getAll)
    val create = SesamePermission("Timetable:create")
    val update = SesamePermission("Timetable:update")
    val delete = SesamePermission("Timetable:delete")
    val get = SesamePermission("Timetable:get")
    val getAll = SesamePermission("Timetable:getAll")
  }

  object semester {
    lazy val all = Set(getAll, get)
    val get = SesamePermission("Semester:get")
    val getAll = SesamePermission("Semester:getAll")
  }

  object group {
    lazy val all = Set(create, getAll)
    val create = SesamePermission("Group:create")
    val getAll = SesamePermission("Group:getAll")
  }

  object user {
    lazy val all = Set(get, getAll)
    val get = SesamePermission("User:get")
    val getAll = SesamePermission("User:getAll")
  }

  object blacklist {
    lazy val all = Set(get)
    val get = SesamePermission("Blacklist:get")
  }

  object entryType {
    lazy val all = Set(getAll)
    val getAll = SesamePermission("EntryType:getAll")
  }

  object reportCardEntry {
    lazy val all = Set(create, update, get, getAll)
    val create = SesamePermission("ReportCardEntry:create")
    val update = SesamePermission("ReportCardEntry:update")
    val get = SesamePermission("ReportCardEntry:get")
    val getAll = SesamePermission("ReportCardEntry:getAll")
  }

  object reportCardEntryType {
    lazy val all = Set(update)
    val update = SesamePermission("ReportCardEntryType:update")
  }

  object reportCardEvaluation {
    lazy val all = Set(create, get, getAll)
    val create = SesamePermission("ReportCardEvaluation:create")
    val get = SesamePermission("ReportCardEvaluation:get")
    val getAll = SesamePermission("ReportCardEvaluation:getAll")
  }

  object assignmentPlan {
    lazy val all = Set(create, update, delete, getAll, get)
    val create = SesamePermission("AssignmentPlan:create")
    val update = SesamePermission("AssignmentPlan:update")
    val delete = SesamePermission("AssignmentPlan:delete")
    val getAll = SesamePermission("AssignmentPlan:getAll")
    val get = SesamePermission("AssignmentPlan:get")
  }

  object annotation {
    lazy val all = Set(create, update, delete, getAll, get)
    val create = SesamePermission("Annotation:create")
    val update = SesamePermission("Annotation:update")
    val delete = SesamePermission("Annotation:delete")
    val getAll = SesamePermission("Annotation:getAll")
    val get = SesamePermission("Annotation:get")
  }
}