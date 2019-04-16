package dao.helper

import java.sql.{Date, Time}
import java.util.UUID

import database._
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep

trait TableFilter[T <: Table[_]] {
  type TableFilterPredicate = T => Rep[Boolean]
}

object TableFilter {

  def labworkFilter[T <: LabworkIdTable](labwork: UUID): T => Rep[Boolean] = _.labwork === labwork

  def courseFilter[T <: LabworkIdTable](course: UUID): T => Rep[Boolean] = _.memberOfCourse(course)

  def labelFilterEquals[T <: LabelTable](label: String): T => Rep[Boolean] = _.label.toLowerCase === label.toLowerCase

  def labelFilterLike[T <: LabelTable](label: String): T => Rep[Boolean] = _.label.toLowerCase like s"%${label.toLowerCase}%"

  def idFilter[T <: UniqueTable](id: UUID): T => Rep[Boolean] = _.id === id

  def abbreviationFilter[T <: AbbreviationTable](abbreviation: String): T => Rep[Boolean] = _.abbreviation.toLowerCase === abbreviation.toLowerCase

  def userFilter[T <: UserIdTable](user: UUID): T => Rep[Boolean] = _.user === user

  def roomFilter[T <: RoomIdTable](room: UUID): T => Rep[Boolean] = _.room === room

  def entryTypeFilter[T <: EntryTypeTable](entryType: String): T => Rep[Boolean] = _.entryType === entryType

  def reportCardEntryFilter[T <: ReportCardEntryIdTable](reportCardEntry: UUID): T => Rep[Boolean] = _.reportCardEntry === reportCardEntry

  def labworkByReportCardEntryFilter[T <: ReportCardEntryIdTable](labwork: UUID): T => Rep[Boolean] = _.reportCardEntryFk.filter(_.labwork === labwork).exists

  def courseByReportCardEntryFilter[T <: ReportCardEntryIdTable](course: UUID): T => Rep[Boolean] = _.reportCardEntryFk.map(_.memberOfCourse(course)).exists

  def userByReportCardEntryFilter[T <: ReportCardEntryIdTable](user: UUID): T => Rep[Boolean] = _.reportCardEntryFk.filter(_.user === user).exists

  def roomByReportCardEntryFilter[T <: ReportCardEntryIdTable](room: UUID): T => Rep[Boolean] = _.reportCardEntryFk.filter(_.room === room).exists

  def groupFilter[T <: GroupIdTable](group: UUID): T => Rep[Boolean] = _.group === group

  def onDateFilter[T <: DateStartEndTable](date: Date): T => Rep[Boolean] = _.date === date

  def onStartFilter[T <: DateStartEndTable](start: Time): T => Rep[Boolean] = _.start === start

  def onEndFilter[T <: DateStartEndTable](end: Time): T => Rep[Boolean] = _.end === end

  def sinceFilter[T <: DateStartEndTable](since: Date): T => Rep[Boolean] = _.date >= since

  def untilFilter[T <: DateStartEndTable](until: Date): T => Rep[Boolean] = _.date <= until

  def systemIdFilter[T <: UserIdTable](systemId: String): T => Rep[Boolean] = _.userFk.filter(_.systemId.toLowerCase === systemId.toLowerCase).exists // TODO FK comparision should be done this way
}