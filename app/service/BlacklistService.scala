package service

import database.BlacklistDb
import org.joda.time.LocalDate

object BlacklistService {

  import utils.date.DateTimeOps.mapTo

  def blacklistDb(label: String)(date: LocalDate) =
    BlacklistDb.entireDay(label, date, global = true)

  def fromRange(label: String, start: LocalDate, end: LocalDate): List[BlacklistDb] =
    mapTo(start, end, blacklistDb(label)).toList
}
