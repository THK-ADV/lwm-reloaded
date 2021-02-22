package service.sheet

import models.helper.EvaluationProperty
import models.{Labwork, ReportCardEntryAtom, ReportCardEntryType, Student}
import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.poi.ss.usermodel.IndexedColors
import org.joda.time.LocalDate

import scala.util.Try

object ReportCardEntryExport extends DefaultExportTemplate {

  def createSheet(cards: List[ReportCardEntryAtom], labwork: Labwork): Try[ByteArrayOutputStream] = {
    val sheet = Sheet(s"Detailierte Praktikumsergebnisse für ${labwork.label}", header(labwork), rowHeader(), content(cards), defaultFooter())
    SheetService.createSheet(sheet)(_ => Unit)
  }

  private def today(): String =
    LocalDate.now.toString("dd.MM.yy")

  private def header(labwork: Labwork): SheetHeader =
    SheetHeader(labwork.label, "", today())

  private def rowHeader(): RowHeader = {
    import Fraction._

    RowHeader(
      List(
        Row("#") -> Low,
        Row("GMID") -> Medium,
        Row("Nachname") -> AutoFit,
        Row("Vorname") -> AutoFit,
        Row("MatrikelNr.") -> Medium,
        Row("Anwesenheiten") -> Low,
        Row("Testate") -> Low,
        Row("Bonus") -> Low,
      ),
      IndexedColors.GREY_25_PERCENT,
      repeating = true
    )
  }

  private def content(cards: List[ReportCardEntryAtom]): List[List[Row]] = {
    import service.ReportCardEvaluationService.count
    cards
      .groupBy(_.student.id)
      .values
      .toList
      .sortBy(s => (s.head.student.lastname, s.head.student.firstname))
      .zipWithIndex
      .map {
        case (cards, index) =>
          val student = cards.head.student.asInstanceOf[Student]
          val atts = count(cards, ReportCardEntryType.Attendance.entryType, EvaluationProperty.BoolBased)
          val certs = count(cards, ReportCardEntryType.Certificate.entryType, EvaluationProperty.BoolBased)
          val bonus = count(cards, ReportCardEntryType.Bonus.entryType, EvaluationProperty.IntBased)

          List(
            Row((index + 1).toString),
            Row(student.systemId),
            Row(student.lastname),
            Row(student.firstname),
            Row(student.registrationId),
            Row(atts.toString),
            Row(certs.toString),
            Row(bonus.toString),
          )
      }
  }
}
