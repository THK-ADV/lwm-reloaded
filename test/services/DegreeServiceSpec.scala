package services

import models.{DegreeDb, PostgresDegree}
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store.DegreeTable

final class DegreeServiceSpec extends AbstractDaoSpec[DegreeTable, DegreeDb, PostgresDegree] with DegreeService {

  val maxDegrees = 10

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "degree"

  override protected val entity: DegreeDb = DegreeDb("label", "abbrev")

  override protected val invalidDuplicateOfEntity: DegreeDb = DegreeDb(entity.label, entity.abbreviation, entity.invalidated, entity.id)

  override protected val invalidUpdateOfEntity: DegreeDb = DegreeDb("new label", "new abbrev", entity.invalidated, entity.id)

  override protected val validUpdateOnEntity: DegreeDb = DegreeDb("new label", entity.abbreviation, entity.invalidated, entity.id)

  override protected val entities: List[DegreeDb] = (0 until maxDegrees).map(i => DegreeDb(i.toString, i.toString)).toList
}
