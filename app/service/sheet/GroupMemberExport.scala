package service.sheet

import models.{GroupAtom, Labwork}
import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.poi.ss.usermodel.IndexedColors
import org.joda.time.LocalDate

import scala.util.Try

object GroupMemberExport extends DefaultExportTemplate {

  def createSheet(groups: List[GroupAtom], labwork: Labwork): Try[ByteArrayOutputStream] = {
    val sheet = Sheet(s"Gruppen für ${labwork.label}", header(), rowHeader(), content(groups), defaultFooter())
    SheetService.createSheet(sheet)(_ => Unit)
  }

  private def header(): SheetHeader = SheetHeader(
    "",
    "",
    LocalDate.now.toString("dd.MM.yy")
  )

  private def rowHeader(): RowHeader = {
    import Fraction._

    RowHeader(
      List(
        Row("Gruppe") -> Medium,
        Row("GMID") -> Medium,
        Row("Nachname") -> AutoFit,
        Row("Vorname") -> AutoFit,
        Row("Email") -> AutoFit
      ),
      IndexedColors.GREY_25_PERCENT,
      repeating = true
    )
  }

  private def content(groups: List[GroupAtom]): List[List[Row]] =
    for {
      g <- groups sortBy (_.label)
      s <- g.members
    } yield List(
      Row(g.label),
      Row(s.systemId),
      Row(s.lastname),
      Row(s.firstname),
      Row(s.email)
    )
}
