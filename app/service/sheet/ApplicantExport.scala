package service.sheet

import models.{LabworkApplicationAtom, LabworkAtom}
import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.poi.ss.usermodel.IndexedColors

import scala.util.Try

object ApplicantExport extends DefaultExportTemplate {

  def createSheet(apps: List[LabworkApplicationAtom], labwork: LabworkAtom): Try[ByteArrayOutputStream] = {
    val sheet = Sheet(s"Anmeldungen für ${labwork.label}", defaultHeader(labwork), rowHeader(), content(apps), defaultFooter())
    SheetService.createSheet(sheet)(_ => Unit)
  }

  private def rowHeader(): RowHeader = {
    import Fraction._

    RowHeader(
      List(
        Row("#") -> Low,
        Row("GMID") -> Medium,
        Row("Nachname") -> AutoFit,
        Row("Vorname") -> AutoFit,
        Row("Anmeldezeitpunkt") -> AutoFit
      ),
      IndexedColors.GREY_25_PERCENT,
      repeating = true
    )
  }

  private def content(apps: List[LabworkApplicationAtom]): List[List[Row]] = apps
    .zipWithIndex
    .map {
      case (app, index) => List(
        Row((index + 1).toString),
        Row(app.applicant.systemId),
        Row(app.applicant.lastname),
        Row(app.applicant.firstname),
        Row(app.lastModified.toString("dd.MM.yy")),
      )
    }
}
