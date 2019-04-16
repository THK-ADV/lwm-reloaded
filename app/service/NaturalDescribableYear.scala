package service

import org.joda.time.LocalDate

sealed trait NaturalDescribableYear {

  import service.NaturalDescribableYear._

  def number: Int = date.getYear

  def shortString: String = date.toString("YY")

  def longString: String = date.toString("YYYY")

  private def date: LocalDate = this match {
    case Current => currentYear
    case Next => nextYear
    case Other(y) => LocalDate.now.withYear(y)
  }
}

object NaturalDescribableYear {

  /**
    * Constructs [[NaturalDescribableYear]] out of `year` which can either be `current` or `next`.
    *
    * @param year which can be either `current` or `next`
    * @return [[NaturalDescribableYear]] if successful. [[None]] if failed
    */
  def apply(year: String): Option[NaturalDescribableYear] = year match {
    case "current" => Some(Current)
    case "next" => Some(Next)
    case _ => None
  }

  /**
    * Constructs [[NaturalDescribableYear]] out of `year`, but allows [[Other]] which can hold arbitrary years
    *
    * @param year which can be any year
    * @return [[NaturalDescribableYear]] which describes best
    */
  def apply(year: Int): NaturalDescribableYear = {
    val current = currentYear.getYear
    val next = nextYear.getYear

    year match {
      case `current` => Current
      case `next` => Next
      case other => Other(other)
    }
  }

  private def nextYear = LocalDate.now.withYear(currentYear.getYear + 1)

  private def currentYear = LocalDate.now
}

case object Current extends NaturalDescribableYear

case object Next extends NaturalDescribableYear

case class Other(year: Int) extends NaturalDescribableYear
