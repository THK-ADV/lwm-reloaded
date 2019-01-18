package services

import java.util.GregorianCalendar

//import akka.actor.{Actor, ActorLogging, Props}
//import dao.SemesterDao
//import models.SemesterDb
//import org.joda.time.LocalDate
//
//import scala.concurrent.ExecutionContextExecutor
//import scala.util.{Failure, Success}
//
//object SemesterCreationActor {
//  def props(semesterDao: SemesterDao, year: NaturalDescribableYear) = Props(new SemesterCreationActor(semesterDao, year))
//
//  import utils.LwmDateTime._
//
//  def semesters(year: NaturalDescribableYear) = List(summerSemester(year), winterSemester(year))
//
//  def summerSemester(year: NaturalDescribableYear) = {
//    val start = new LocalDate(year.number, 3, 1)
//    val end = new LocalDate(year.number, 8, 31)
//
//    SemesterDb(s"Sommersemester ${year.longString}", s"SS ${year.shortString}", start.sqlDate, end.sqlDate, end.minusWeeks(examWeekPadding).sqlDate)
//  }
//
//  def winterSemester(currentYear: NaturalDescribableYear) = {
//    val nextYear = NaturalDescribableYear(currentYear.number + 1)
//    val start = new LocalDate(currentYear.number, 9, 1)
//    val maxDayConcerningLeapYear = if (new GregorianCalendar().isLeapYear(nextYear.number)) 29 else 28
//    val end = new LocalDate(nextYear.number, 2, maxDayConcerningLeapYear)
//
//    SemesterDb(s"Wintersemester ${currentYear.longString}/${nextYear.longString}", s"WS ${currentYear.shortString}/${nextYear.shortString}", start.sqlDate, end.sqlDate, end.minusWeeks(examWeekPadding).sqlDate)
//  }
//
//  def examWeekPadding = 2
//
//  case object CreationRequest
//
//}
//
//final class SemesterCreationActor(private val semesterDao: SemesterDao, private val year: NaturalDescribableYear) extends Actor with ActorLogging {
//
//  import services.SemesterCreationActor._
//
//  private implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher
//
//  override def receive = {
//    case CreationRequest =>
//      log.info("beginning creation request")
//
//      semesterDao.createManyPartial(semesters(year)) onComplete {
//        case Success(result) =>
//          log.info(s"result of creation request is $result")
//        case Failure(error) =>
//          log.error(s"failed creation request with exception: ${error.getLocalizedMessage}")
//      }
//  }
//}
