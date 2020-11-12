package service.sheet

import java.util.UUID

import models.{LabworkAtom, ReportCardEvaluationAtom, Student}
import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.poi.hssf.usermodel.HSSFSheet
import org.apache.poi.ss.usermodel.{BorderStyle, IndexedColors}
import org.apache.poi.ss.util.{CellRangeAddress, RegionUtil}

import scala.util.Try

object ExaminationOfficeExport extends DefaultExportTemplate {

  def createSheet(evals: Seq[ReportCardEvaluationAtom], labwork: LabworkAtom, firstName: String, lastName: String): Try[ByteArrayOutputStream] = {
    val content = (hasPassed _ andThen merge andThen toContent) (evals)
    val sheet = Sheet(labwork.degree.label, defaultHeader(labwork), rowHeader(), content, defaultFooter())

    SheetService.createSheet(sheet) { sheet0 =>
      createClosing(sheet0, signature(firstName, lastName), sheet.rowContent.size, sheet.rowContent.head.size)
    }
  }

  private def createClosing(sheet: HSSFSheet, signature: Signature, lastRow: Int, maxCol: Int): Unit = {
    val row = sheet.createRow(lastRow + 3)
    val mergeRegion = new CellRangeAddress(row.getRowNum, row.getRowNum, maxCol - 3, maxCol - 1)
    sheet.addMergedRegion(mergeRegion)
    val cell = row.createCell(maxCol - 3)

    RegionUtil.setBorderTop(BorderStyle.MEDIUM, mergeRegion, sheet)
    RegionUtil.setTopBorderColor(IndexedColors.BLACK.index, mergeRegion, sheet)

    cell.setCellValue(signature.text)
  }

  private def rowHeader(): RowHeader = {
    import Fraction._

    RowHeader(
      List(
        Row("#") -> Low,
        Row("Nachname") -> AutoFit,
        Row("Vorname") -> AutoFit,
        Row("MatNr.") -> Medium,
        Row("Datum") -> Medium
      ),
      IndexedColors.GREY_25_PERCENT,
      repeating = true
    )
  }

  private def hasPassed(evals: Seq[ReportCardEvaluationAtom]): Map[UUID, Seq[ReportCardEvaluationAtom]] =
    evals.groupBy(_.student.id).filter(_._2.forall(_.bool))

  private def merge(evals: Map[UUID, Seq[ReportCardEvaluationAtom]]): List[ReportCardEvaluationAtom] = {
    import utils.date.DateTimeOps.dateTimeOrd
    evals.map(t => t._2.maxBy(_.lastModified)).toList
  }

  private def toContent(evals: List[ReportCardEvaluationAtom]): List[List[Row]] = evals
    .sortBy(a => (a.student.lastname, a.student.firstname))
    .zipWithIndex
    .map {
      case (eval, index) => List(
        Row((index + 1).toString),
        Row(eval.student.lastname),
        Row(eval.student.firstname),
        Row(eval.student.asInstanceOf[Student].registrationId.dropRight(2)),
        Row(eval.lastModified.toString("dd.MM.yy")),
      )
    }

  private def signature(firstName: String, lastName: String): Signature =
    Signature(s"Für die Richtigkeit der Angaben: ${firstName.charAt(0)}. ${lastName}")
}
