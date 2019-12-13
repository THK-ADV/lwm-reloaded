package controllers.core

import java.util.UUID

import database._
import slick.lifted.Rep

import scala.concurrent.Future
import scala.util.{Success, Try}

object DBFilterOps {

  implicit class AbstractTableFilter(string: String) {

    import dao.helper.TableFilter._

    def uuid: Try[UUID] = Try(UUID.fromString(string))

    def uuidF: Future[UUID] = Future.fromTry(string.uuid)

    def boolean: Try[Boolean] = Try(string.toBoolean)

    def int: Try[Int] = Try(string.toInt)

    def makeCourseFilter[A <: LabworkIdTable]: Try[A => Rep[Boolean]] = string.uuid map courseFilter

    def makeLabworkFilter[A <: LabworkIdTable]: Try[A => Rep[Boolean]] = string.uuid map labworkFilter

    def makeLabelLikeFilter[A <: LabelTable]: Try[A => Rep[Boolean]] = Success(labelFilterLike(string))

    def makeLabelEqualsFilter[A <: LabelTable]: Try[A => Rep[Boolean]] = Success(labelFilterEquals(string))

    def makeAbbrevFilter[A <: AbbreviationTable]: Try[A => Rep[Boolean]] = Success(abbreviationFilter(string))

    def makeUserFilter[A <: UserIdTable]: Try[A => Rep[Boolean]] = string.uuid map userFilter

    def makeRoomFilter[A <: RoomIdTable]: Try[A => Rep[Boolean]] = string.uuid map roomFilter

    def makeEntryTypeFilter[A <: EntryTypeTable]: Try[A => Rep[Boolean]] = Success(entryTypeFilter(string))

    def makeReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map reportCardEntryFilter

    def makeUserByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map userByReportCardEntryFilter

    def makeLabworkByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map labworkByReportCardEntryFilter

    def makeCourseByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map courseByReportCardEntryFilter

    def makeRoomByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map roomByReportCardEntryFilter

    def makeGroupFilter[A <: GroupIdTable]: Try[A => Rep[Boolean]] = string.uuid map groupFilter
  }
}
