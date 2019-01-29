package dao

import base.{LwmFakeApplication, PostgresDbSpec, TestBaseDefinition}
import org.scalatest.WordSpec
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

final class DegreeDaoSpec extends PostgresDbSpec {

  val dao = app.injector.instanceOf(classOf[DegreeDao])

  "A DegreeDaoSpec" should {
    "fake" in {
      val res = dao.get()
      whenReady(res, timeout(Span(5, Seconds)))(println)
    }
  }

//  override protected def name: String = "degree"
//
//  override protected val dbEntity: DegreeDb = DegreeDb("label", "abbrev")
//
//  override protected val invalidDuplicateOfDbEntity: DegreeDb = DegreeDb(dbEntity.label, dbEntity.abbreviation)
//
//  override protected val invalidUpdateOfDbEntity: DegreeDb = DegreeDb("new label", "new abbrev", lastModified, dbEntity.invalidated, dbEntity.id)
//
//  override protected val validUpdateOnDbEntity: DegreeDb = DegreeDb("new label", dbEntity.abbreviation, lastModified, dbEntity.invalidated, dbEntity.id)
//
//  override protected val dbEntities: List[DegreeDb] = degrees
//
//  override protected val lwmEntity: Degree = dbEntity.toUniqueEntity
//
//  override protected val lwmAtom: Degree = lwmEntity

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()
}
