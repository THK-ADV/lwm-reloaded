package service.sheet

import models.LabworkAtom
import org.joda.time.LocalDate

trait DefaultExportTemplate {

  def defaultFooter(): SheetFooter =
    SheetFooter("Generiert durch das Praktikumstool (https://praktikum.gm.fh-koeln.de)", showPageNumbers = true)

  def defaultHeader(labwork: LabworkAtom): SheetHeader = SheetHeader(
    s"${labwork.course.label}\n${labwork.course.lecturer.firstname} ${labwork.course.lecturer.lastname}",
    labwork.degree.label,
    LocalDate.now.toString("dd.MM.yy")
  )
}
