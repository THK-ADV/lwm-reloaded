package services.blacklist

import org.joda.time.LocalDate

sealed trait BlacklistYear {

  def year: Int = this match {
    case Current => currentYear
    case Next => nextYear
  }

  private def nextYear: Int = LocalDate.now.withYear(currentYear + 1).getYear

  private def currentYear: Int = LocalDate.now.getYear
}

object BlacklistYear {

  /**
    * Constructs [[BlacklistYear]] out of `year` which can either be `current` or `next`.
    *
    * @param year which can be either `current` or `next`
    * @return [[BlacklistYear]] if successful. [[None]] if failed
    */
  def apply(year: String): Option[BlacklistYear] = year match {
    case "current" => Some(Current)
    case "next" => Some(Next)
    case _ => None
  }
}

case object Current extends BlacklistYear

case object Next extends BlacklistYear
