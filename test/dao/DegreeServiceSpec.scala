package dao

import models.{DegreeDb, PostgresDegree}
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store.DegreeTable

final class DegreeServiceSpec extends AbstractDaoSpec[DegreeTable, DegreeDb, PostgresDegree] with DegreeService {
  import dao.AbstractDaoSpec._

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "degree"

  override protected val dbEntity: DegreeDb = DegreeDb("label", "abbrev")

  override protected val invalidDuplicateOfDbEntity: DegreeDb = DegreeDb(dbEntity.label, dbEntity.abbreviation)

  override protected val invalidUpdateOfDbEntity: DegreeDb = DegreeDb("new label", "new abbrev", lastModified, dbEntity.invalidated, dbEntity.id)

  override protected val validUpdateOnDbEntity: DegreeDb = DegreeDb("new label", dbEntity.abbreviation, lastModified, dbEntity.invalidated, dbEntity.id)

  override protected val dbEntities: List[DegreeDb] = degrees

  override protected val lwmEntity: PostgresDegree = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresDegree = lwmEntity
}
