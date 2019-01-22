package models.helper

import java.util.UUID

import org.joda.time.{LocalDate, LocalTime}

case class TimetableDateEntry(weekday: Weekday, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, supervisor: Set[UUID])
