package controllers.helper

import dao.helper.TableFilter
import database.DateStartEndTable
import slick.jdbc.PostgresProfile.api._
import scala.util.{Failure, Try}

trait TimeRangeTableFilter[T <: Table[_] with DateStartEndTable]
  extends TableFilter[T]
    with TimeRangeFilter {

  import dao.helper.TableFilter._
  import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter, StringConverter}

  def makeTimeRangeFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    (attribute, value) match {
      case (`dateAttribute`, d) => d.localDate map (_.sqlDate) map onDateFilter
      case (`startAttribute`, s) => s.localTime map (_.sqlTime) map onStartFilter
      case (`endAttribute`, e) => e.localTime map (_.sqlTime) map onEndFilter
      case (`sinceAttribute`, s) => s.localDate map (_.sqlDate) map sinceFilter
      case (`untilAttribute`, u) => u.localDate map (_.sqlDate) map untilFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }
}
