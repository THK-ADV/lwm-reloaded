package dao

import database.{DegreeDb, DegreeTable}
import models.Degree
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

final class DegreeDaoSpec extends AbstractDaoSpec[DegreeTable, DegreeDb, Degree] {
  import AbstractDaoSpec.degrees

  override protected val dao: DegreeDao = app.injector.instanceOf(classOf[DegreeDao])

  override protected def name: String = "degree"

  override protected val dbEntity: DegreeDb = DegreeDb("label", "abbrev")

  override protected val invalidDuplicateOfDbEntity: DegreeDb = DegreeDb(dbEntity.label, dbEntity.abbreviation)

  override protected val invalidUpdateOfDbEntity: DegreeDb = dbEntity.copy("new label", "new abbrev")

  override protected val validUpdateOnDbEntity: DegreeDb = dbEntity.copy("new label")

  override protected val dbEntities: List[DegreeDb] = degrees

  override protected val lwmAtom: Degree = dbEntity.toUniqueEntity

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()
}
