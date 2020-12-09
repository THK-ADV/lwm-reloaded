package dao

import java.util.UUID

import base.DateGenerator
import dao.helper.ModelAlreadyExists
import database.{BlacklistDb, BlacklistTable}
import models.Blacklist
import org.joda.time.{LocalDate, LocalTime}
import play.api.inject.guice.GuiceableModule
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext

final class BlacklistDaoSpec extends AbstractDaoSpec[BlacklistTable, BlacklistDb, Blacklist] with DateGenerator {

  import AbstractDaoSpec._
  import utils.date.DateTimeOps._

  val dao = app.injector.instanceOf(classOf[BlacklistDao])
  implicit val ctx = app.injector.instanceOf(classOf[ExecutionContext])

  override protected def name: String = "blacklist"

  override protected val dbEntity: BlacklistDb =
    BlacklistDb.entireDay("label", LocalDate.now.minusDays(1).sqlDate, global = true)

  override protected val invalidDuplicateOfDbEntity: BlacklistDb =
    dbEntity.copy(label = "new", id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: BlacklistDb =
    dbEntity.copy("label 2", global = !dbEntity.global)

  override protected val validUpdateOnDbEntity: BlacklistDb =
    dbEntity.copy("label 2", start = LocalTime.now.minusHours(1).sqlTime)

  override protected val dbEntities: List[BlacklistDb] = blacklists

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq()

  override protected val lwmAtom: Blacklist = dbEntity.toUniqueEntity

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  private def removeAll() = for {
    all <- dao.get()
    _ <- dao.invalidateMany(all.map(_.id).toList)
  } yield ()

  "A BlacklistDaoSpec also" should {

    "create a blacklist" in {
      val bl1 = BlacklistDb.entireDay("Global", localDate(2020, 1, 1), true)
      val bl2 = BlacklistDb.entireDay("Local", localDate(2020, 1, 2), false)
      val bl3 = BlacklistDb.entireDay("Local2", localDate(2020, 1, 2), false)
      val bl4 = BlacklistDb.entireDay("Global2", localDate(2020, 1, 2), true)

      val f = for {
        _ <- removeAll()
        _ <- dao.create(bl1)
        _ <- dao.create(bl2)
        _ <- dao.create(bl3)
        _ <- dao.create(bl4)
      } yield Unit

      async(f)(_ shouldBe Unit)
    }

    "not create a local Blacklist if a global one exists on the same day" in {
      val existing = BlacklistDb.entireDay("Global", localDate(2020, 1, 1), true)
      val toCreate = BlacklistDb.entireDay("Local", localDate(2020, 1, 1), false)

      val f = for {
        _ <- removeAll()
        _ <- dao.create(existing)
        _ <- dao.create(toCreate)
      } yield ()

      async(f.failed)(_ shouldBe ModelAlreadyExists(toCreate, List(existing)))
    }

    "not allow to have the same local blacklist twice" in {
      val toCreate = BlacklistDb.entireDay("Local", localDate(2020, 1, 1), false)
      val existing = BlacklistDb.entireDay("Local", localDate(2020, 1, 1), false)

      val f = for {
        _ <- removeAll()
        _ <- dao.create(existing)
        _ <- dao.create(toCreate)
      } yield ()

      async(f.failed)(_ shouldBe ModelAlreadyExists(toCreate, List(existing)))
    }

    "not allow to have the same global blacklist twice" in {
      val toCreate = BlacklistDb.entireDay("Global 1", localDate(2020, 1, 1), true)
      val existing = BlacklistDb.entireDay("Global 2", localDate(2020, 1, 1), true)

      val f = for {
        _ <- removeAll()
        _ <- dao.create(existing)
        _ <- dao.create(toCreate)
      } yield ()

      async(f.failed)(_ shouldBe ModelAlreadyExists(toCreate, List(existing)))
    }
  }
}
