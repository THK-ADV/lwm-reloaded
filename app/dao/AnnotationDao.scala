package dao

import database.{AnnotationDb, AnnotationTable}
import models.AnnotationLike
import models.AnnotationLike.{Annotation, AnnotationAtom}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

trait AnnotationDao extends AbstractDao[AnnotationTable, AnnotationDb, AnnotationLike]

final class AnnotationDaoImpl @Inject()(
  val db: Database,
  val profile: JdbcProfile,
  implicit val executionContext: ExecutionContext,
) extends AnnotationDao {

  import dao.helper.TableFilter.{reportCardEntryFilter, userFilter}
  import profile.api._
  import utils.date.DateTimeOps.SqlTimestampConverter

  override protected def tableQuery = TableQuery[AnnotationTable]

  override protected def shouldUpdate(existing: AnnotationDb, toUpdate: AnnotationDb) =
    existing.reportCardEntry == toUpdate.reportCardEntry &&
      existing.author == toUpdate.author

  override protected def existsQuery(entity: AnnotationDb) =
    filterBy(List(userFilter(entity.author), reportCardEntryFilter(entity.reportCardEntry)))

  override protected def toAtomic(query: Query[AnnotationTable, AnnotationDb, Seq]): Future[Seq[AnnotationAtom]] = {
    val join = for {
      q <- query
      r <- q.reportCardEntryFk
      u <- q.userFk
    } yield (q, r, u)

    val action = join.result.map(_.map {
      case (annotation, reportCardEntry, author) =>
        AnnotationAtom(
          reportCardEntry.toUniqueEntity,
          author.toUniqueEntity,
          annotation.message,
          annotation.lastModified.dateTime,
          annotation.id
        )
    })

    db.run(action)
  }

  override protected def toUniqueEntity(query: Query[AnnotationTable, AnnotationDb, Seq]): Future[Seq[Annotation]] =
    db.run(query.result.map(_.map(_.toUniqueEntity)))
}
