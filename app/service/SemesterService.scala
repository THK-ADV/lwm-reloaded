package service

import java.util.GregorianCalendar

import dao.SemesterDao
import database.SemesterDb
import javax.inject.{Inject, Singleton}
import org.joda.time.LocalDate
import service.actor.NaturalDescribableYear

@Singleton
final class SemesterService @Inject()(private val semesterDao: SemesterDao) {

  def createSemester(year: NaturalDescribableYear) = (semesters _ andThen semesterDao.createManyPartial) (year)

  def semesters(year: NaturalDescribableYear): List[SemesterDb] = List(summerSemester(year), winterSemester(year))

  def summerSemester(year: NaturalDescribableYear) = {
    val start = new LocalDate(year.year, 3, 1)
    val end = new LocalDate(year.year, 8, 31)

    makeSemester(s"Sommersemester ${year.long}", s"SoSe ${year.short}", start, end)
  }

  def winterSemester(year: NaturalDescribableYear) = {
    val nextYear = NaturalDescribableYear(year.year + 1)
    val start = new LocalDate(year.year, 9, 1)
    val maxDayConcerningLeapYear = if (new GregorianCalendar().isLeapYear(nextYear.year)) 29 else 28
    val end = new LocalDate(nextYear.year, 2, maxDayConcerningLeapYear)

    makeSemester(s"Wintersemester ${year.long}/${nextYear.long}", s"WS ${year.short}/${nextYear.short}", start, end)
  }

  private def makeSemester(label: String, abbrev: String, start: LocalDate, end: LocalDate) = {
    import utils.date.DateTimeOps.LocalDateConverter
    SemesterDb(label, abbrev, start.sqlDate, end.sqlDate, end.minusWeeks(examWeekPadding).sqlDate)
  }

  private def examWeekPadding = 2
}
