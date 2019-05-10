package service

import org.joda.time.LocalDate

import scala.util.Try

sealed trait NaturalDescribableYear {

  import service.NaturalDescribableYear._

  def year: Int = this match {
    case Current => currentYear
    case Next => nextYear
    case Year(y) => y
  }
}

object NaturalDescribableYear {

  /**
    * Constructs [[NaturalDescribableYear]] out of `year` which can either be `current` or `next`.
    *
    * @param string which can be either `current` or `next`
    * @return [[NaturalDescribableYear]] if successful. [[None]] if failed
    */
  def parse(string: String): Option[NaturalDescribableYear] = string match {
    case "current" => Some(Current)
    case "next" => Some(Next)
    case _ => None
  }

  def apply(value: Int): NaturalDescribableYear = Year(value)

  def apply(year: String): Option[NaturalDescribableYear] = Try(year.toInt).map(apply).toOption

  private def nextYear = currentYear + 1

  private def currentYear = LocalDate.now.getYear

  case object Current extends NaturalDescribableYear

  case object Next extends NaturalDescribableYear

  case class Year(value: Int) extends NaturalDescribableYear

}

