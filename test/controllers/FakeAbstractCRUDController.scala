package controllers

import java.sql.Timestamp
import java.util.UUID

import dao.{AbstractDao, AuthorityDao}
import database.UniqueTable
import javax.inject.Inject
import models.{UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

import scala.concurrent.ExecutionContext

case class FakeProtocol(string: String, int: Int)

case class FakeModel(string: String, int: Int, id: UUID) extends UniqueEntity

class FakeTable(tag: Tag) extends Table[FakeDb](tag, "FAKE") with UniqueTable {
  def string = column[String]("STRING")

  def int = column[Int]("INT")

  override def * = (string, int, lastModified, invalidated, id) <> ((FakeDb.apply _).tupled, FakeDb.unapply)
}

case class FakeDb(string: String, int: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = FakeModel(string, int, id)
}

class FakeAbstractDao @Inject()(val db: Database, implicit val executionContext: ExecutionContext) extends AbstractDao[FakeTable, FakeDb, FakeModel] {
  override protected val tableQuery = TableQuery[FakeTable]

  override protected def toAtomic(query: PostgresProfile.api.Query[FakeTable, FakeDb, Seq]) = toUniqueEntity(query)

  override protected def toUniqueEntity(query: PostgresProfile.api.Query[FakeTable, FakeDb, Seq]) = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: FakeDb): PostgresProfile.api.Query[FakeTable, FakeDb, Seq] = {
    filterValidOnly(_.string === entity.string)
  }

  override protected def shouldUpdate(existing: FakeDb, toUpdate: FakeDb): Boolean = {
    existing.int != toUpdate.int && existing.string == toUpdate.string
  }
}

class FakeAbstractCRUDController @Inject()(
  val cc: ControllerComponents,
  val abstractDao: FakeAbstractDao,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain
) extends AbstractCRUDController[FakeProtocol, FakeTable, FakeDb, FakeModel](cc) {
  override implicit protected def writes: Writes[FakeModel] = Json.writes[FakeModel]

  override implicit protected def reads: Reads[FakeProtocol] = Json.reads[FakeProtocol]

  override protected def toDbModel(protocol: FakeProtocol, existingId: Option[UUID]): FakeDb = {
    FakeDb(protocol.string, protocol.int, id = existingId getOrElse UUID.randomUUID)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => NonSecureBlock
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => NonSecureBlock
  }
}
