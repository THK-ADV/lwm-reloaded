package dao

import models.Degree
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._
import store.{DegreeDb, DegreeTable}

final class DegreeDaoSpec extends AbstractDaoSpec[DegreeTable, DegreeDb, Degree] with DegreeDao {
  import dao.AbstractDaoSpec._

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "degree"

  override protected val dbEntity: DegreeDb = DegreeDb("label", "abbrev")

  override protected val invalidDuplicateOfDbEntity: DegreeDb = DegreeDb(dbEntity.label, dbEntity.abbreviation)

  override protected val invalidUpdateOfDbEntity: DegreeDb = DegreeDb("new label", "new abbrev", lastModified, dbEntity.invalidated, dbEntity.id)

  override protected val validUpdateOnDbEntity: DegreeDb = DegreeDb("new label", dbEntity.abbreviation, lastModified, dbEntity.invalidated, dbEntity.id)

  override protected val dbEntities: List[DegreeDb] = degrees

  override protected val lwmEntity: Degree = dbEntity.toUniqueEntity

  override protected val lwmAtom: Degree = lwmEntity
}
