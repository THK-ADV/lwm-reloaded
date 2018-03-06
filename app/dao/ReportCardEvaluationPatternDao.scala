package dao

import java.util.UUID

import models.{ReportCardEvaluationPattern, ReportCardEvaluationPatternDb}
import slick.driver.PostgresDriver
import slick.lifted.{Rep, TableQuery}
import store.{ReportCardEvaluationPatternTable, TableFilter}
import slick.driver.PostgresDriver.api._

case class EvaluationPatternPropertyFilter(value: String) extends TableFilter[ReportCardEvaluationPatternTable] {
  override def predicate = _.property.toLowerCase === value.toLowerCase
}

case class EvaluationPatternEntryTypeFilter(value: String) extends TableFilter[ReportCardEvaluationPatternTable] {
  override def predicate = _.entryType.toLowerCase === value.toLowerCase
}

case class EvaluationPatternLabworkFilter(value: String) extends TableFilter[ReportCardEvaluationPatternTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

case class EvaluationPatternCourseFilter(value: String) extends TableFilter[ReportCardEvaluationPatternTable] {
  override def predicate = _.memberOfCourse(value)
}

trait ReportCardEvaluationPatternDao extends AbstractDao[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, ReportCardEvaluationPattern] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[ReportCardEvaluationPatternTable]

  override protected def toAtomic(query: Query[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, Seq]) = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, Seq]) = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }

  override protected def existsQuery(entity: ReportCardEvaluationPatternDb): Query[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, Seq] = {
    filterBy(List(
      EvaluationPatternPropertyFilter(entity.property),
      EvaluationPatternEntryTypeFilter(entity.entryType),
      EvaluationPatternLabworkFilter(entity.labwork.toString)
    ))
  }

  override protected def shouldUpdate(existing: ReportCardEvaluationPatternDb, toUpdate: ReportCardEvaluationPatternDb): Boolean = {
    (existing.min != toUpdate.min) &&
      (existing.property == toUpdate.property && existing.entryType == toUpdate.entryType && existing.labwork == toUpdate.labwork)
  }
}

final class ReportCardEvaluationPatternDaoImpl(val db: PostgresDriver.backend.Database) extends ReportCardEvaluationPatternDao