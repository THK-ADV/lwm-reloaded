package service

import java.io.File

import base.TestBaseDefinition
import org.apache.commons.io.FileUtils
import org.apache.poi.ss.usermodel.IndexedColors
import org.joda.time.LocalDate
import org.scalatest.WordSpec
import service.sheet._

class SheetServiceSpec extends WordSpec with TestBaseDefinition {

  "A SheetServiceSpec" should {

    "create rows" in {
      import Fraction._
      val now = LocalDate.now.toString("dd.MM.yy")
      val sheetHeader = SheetHeader("AP2\nKohls", "Medieninformatik", now)

      def col(i: Int) = List(
        Row(i.toString),
        Row("DobryninDobryninDobryninDobryninDobrynin"),
        Row("AlexAlexAlexAlexAlexAlex"),
        Row("88888888"),
        Row(now),
      )

      val rowHeader = RowHeader(
        List(
          Row("#1") -> Low,
          Row("Nachname") -> AutoFit,
          Row("Vorname") -> AutoFit,
          Row("MatNr.") -> Medium,
          Row("Datum") -> Medium
        ),
        IndexedColors.GREY_25_PERCENT,
        repeating = true
      )
      val cols = (0 until 100).map(col).toList

      val footer = SheetFooter("Generiert durch das Praktikumstool (praktikum.gm.fh-koeln.de)", true)

      val sheet = Sheet("A", sheetHeader, rowHeader, cols, Signature("Alex"), footer)
      val res = SheetService.createSheet(sheet)

      val f = new File("foo.xls")
      FileUtils.writeByteArrayToFile(f, res.success.value.toByteArray)
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    FileUtils.forceDelete(new File("foo.xls"))
  }
}
