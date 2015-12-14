package services

import java.util.UUID
import models.schedule.Timetable

trait TimetableServiceLike {

  def merge(current: Timetable, semester: UUID): Vector[Timetable]
}

class TimetableService extends TimetableServiceLike {

  override def merge(current: Timetable, semester: UUID): Vector[Timetable] = Vector.empty[Timetable]
}
