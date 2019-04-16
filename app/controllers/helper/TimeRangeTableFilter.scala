package controllers.helper

import dao.helper.TableFilter
import database.DateStartEndTable
import slick.jdbc.PostgresProfile.api._
import scala.util.{Failure, Try}

trait TimeRangeTableFilter[T <: Table[_] with DateStartEndTable]
  extends TableFilter[T]
    with TimeRangeFilter {

  import dao.helper.TableFilter._
  import utils.date.DateTimeOps.{LocalDateConverter, StringConverter}

  def makeTimeRangeFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    (attribute, value) match {
      case (`dateAttribute`, d) => d.localDate map (_.sqlDate) map onDateFilter
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }
}
