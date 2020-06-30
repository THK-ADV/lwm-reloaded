package service.sheet

import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.poi.hssf.usermodel._
import org.apache.poi.ss.usermodel.{BorderStyle, FillPatternType, IndexedColors}
import org.apache.poi.ss.util.{CellRangeAddress, RegionUtil}

import scala.util.Try

sealed trait Fraction

object Fraction {

  object Low extends Fraction

  object Medium extends Fraction

  object Large extends Fraction

  object AutoFit extends Fraction

}

case class Row(value: String)

case class SheetHeader(left: String, center: String, right: String)

case class RowHeader(cols: List[(Row, Fraction)], backgroundColor: IndexedColors, repeating: Boolean)

case class Signature(text: String)

case class SheetFooter(left: String, showPageNumbers: Boolean)

case class Sheet(name: String, header: SheetHeader, rowHeader: RowHeader, rowContent: List[List[Row]], signature: Signature, footer: SheetFooter)

object SheetService {

  private def withSheet(name: String)(f: (HSSFSheet, HSSFWorkbook) => Unit): Try[ByteArrayOutputStream] = {
    val book = HSSFWorkbookFactory.createWorkbook()

    for {
      sheet <- Try(book.createSheet(name))
      _ = f(sheet, book)
      stream = new ByteArrayOutputStream()
      _ = book.write(stream)
      _ = stream.close()
      _ = book.close()
    } yield stream
  }

  def createSheet(sheet: Sheet): Try[ByteArrayOutputStream] = withSheet(sheet.name) { (sheet0, book) =>
    val headerStyle = book.createCellStyle()
    val contentStyle = book.createCellStyle()
    val closingStyle = book.createCellStyle()
    val font = book.createFont()

    createHeader(sheet0, sheet.header)
    createFooter(sheet0, sheet.footer)

    val (headerCells, lastRow) = createRowHeader(sheet0, font, headerStyle, sheet.rowHeader)
    applyBorders(headerStyle, headerCells)

    val contentCells = createRows(sheet0, lastRow + 1, sheet.rowContent)
    applyBorders(contentStyle, contentCells)

    createClosing(sheet0, closingStyle, sheet.signature, sheet.rowContent.size, sheet.rowContent.head.size)

    applyWidth(sheet0, sheet)
  }

  private def applyWidth(sheet0: HSSFSheet, sheet: Sheet): Unit = {
    def fractionValue(f: Fraction): Option[Double] = f match {
      case Fraction.Low => Some(0.05)
      case Fraction.Medium => Some(0.15)
      case Fraction.Large => Some(0.25)
      case Fraction.AutoFit => None
    }

    val maxCols = sheet.rowHeader.cols.size
    val colWidth = 16 // TODO this magic number was determined by trial and error
    val totalWidth = maxCols * colWidth

    val cells = sheet.rowHeader.cols.map(t => fractionValue(t._2)).zipWithIndex

    val widthSoFar = cells.filter(_._1.isDefined).foldLeft(0) {
      case (acc, (f, i)) =>
        val width = (totalWidth * f.get).toInt
        sheet0.setColumnWidth(i, width * 256)
        acc + width
    }

    val remainingCells = cells.filter(_._1.isEmpty)
    val remainingWidth = (totalWidth - widthSoFar) / remainingCells.size

    remainingCells.foreach {
      case (_, i) =>
        sheet0.setColumnWidth(i, remainingWidth * 256)
    }
  }

  private def applyBorders(style: HSSFCellStyle, cells: List[HSSFCell]): Unit = {
    val borderColor = IndexedColors.BLACK.getIndex
    val borderStyle = BorderStyle.THIN

    style.setBorderBottom(borderStyle)
    style.setBottomBorderColor(borderColor)
    style.setBorderLeft(borderStyle)
    style.setLeftBorderColor(borderColor)
    style.setBorderRight(borderStyle)
    style.setRightBorderColor(borderColor)
    style.setBorderTop(borderStyle)
    style.setTopBorderColor(borderColor)

    cells.foreach(_.setCellStyle(style))
  }

  private def createRowHeader(sheet: HSSFSheet, font: HSSFFont, style: HSSFCellStyle, header: RowHeader): (List[HSSFCell], Int) = {
    font.setBold(true)
    style.setFont(font)

    style.setFillForegroundColor(header.backgroundColor.index)
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND)

    val row = sheet.createRow(0)
    val cells = header.cols.zipWithIndex.map {
      case (col, index) =>
        val cell = row.createCell(index)
        cell.setCellValue(col._1.value)
        cell.setCellStyle(style)
        cell
    }

    if (header.repeating) {
      sheet.setRepeatingRows(new CellRangeAddress(row.getRowNum, row.getRowNum, row.getRowNum, header.cols.size - 1))
    }

    cells -> row.getRowNum
  }

  private def createHeader(sheet: HSSFSheet, header: SheetHeader): Unit = {
    sheet.getHeader.setLeft(header.left)
    sheet.getHeader.setCenter(header.center)
    sheet.getHeader.setRight(header.right)
  }

  private def createFooter(sheet: HSSFSheet, footer: SheetFooter): Unit = {
    sheet.getFooter.setLeft(footer.left)

    if (footer.showPageNumbers) {
      sheet.getFooter.setRight(s"${HeaderFooter.page} / ${HeaderFooter.numPages}")
    }
  }

  private def createClosing(sheet: HSSFSheet, style: HSSFCellStyle, signature: Signature, lastRow: Int, maxCol: Int): Unit = {
    val row = sheet.createRow(lastRow + 3)
    val mergeRegion = new CellRangeAddress(row.getRowNum, row.getRowNum, maxCol - 3, maxCol - 1)
    sheet.addMergedRegion(mergeRegion)
    val cell = row.createCell(maxCol - 3)

    RegionUtil.setBorderTop(BorderStyle.MEDIUM, mergeRegion, sheet)
    RegionUtil.setTopBorderColor(IndexedColors.BLACK.index, mergeRegion, sheet)

    cell.setCellValue(signature.text)
  }

  private def createRows(sheet: HSSFSheet, nextRow: Int, rows: List[List[Row]]): List[HSSFCell] = {
    rows.zipWithIndex.flatMap {
      case (rows, rowIndex) =>
        val row = sheet.createRow(rowIndex + nextRow)

        rows.zipWithIndex.map {
          case (col, colIndex) =>
            val cell = row.createCell(colIndex)
            cell.setCellValue(col.value)
            cell
        }
    }
  }
}