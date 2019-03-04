package dao

import database.{BlacklistDb, BlacklistTable}
import models.Blacklist
import org.joda.time.LocalDate
import play.api.inject.guice.GuiceableModule
import slick.jdbc.PostgresProfile.api._

final class BlacklistDaoSpec extends AbstractDaoSpec[BlacklistTable, BlacklistDb, Blacklist] {

  import AbstractDaoSpec._
  import Blacklist._
  import utils.LwmDateTime._
  import scala.concurrent.ExecutionContext.Implicits.global

  val dao = app.injector.instanceOf(classOf[BlacklistDao])

  "A BlacklistService2Spec also" should {

    "filter properly" in {
      val (since, until) = {
        val chosen = takeSomeOf(dbEntities.
          filter(b => !b.start.localTime.isEqual(startOfDay))).
          toList.
          sortWith((a, b) => a.date.localDate.isBefore(b.date.localDate))

        (chosen.head.date, takeOneOf(chosen.tail).date)
      }

      runAsyncSequence(
        dao.filterBy(List(BlacklistGlobalFilter(true.toString))).result map { blacklists =>
          blacklists shouldBe dbEntities.filter(_.global)
          blacklists.forall(b => b.start.localTime.isEqual(startOfDay) && b.end.localTime.isEqual(endOfDay)) shouldBe true
        },
        dao.filterBy(List(BlacklistDateFilter(randomBlacklist.date.stringMillis))).result map { blacklists =>
          blacklists shouldBe dbEntities.filter(b => blacklists.exists(_.date.localDate.isEqual(b.date.localDate)))
        },
        dao.filterBy(List(BlacklistStartFilter(randomBlacklist.start.stringMillis))).result map { blacklists =>
          blacklists shouldBe dbEntities.filter(b => blacklists.exists(_.start.localTime.isEqual(b.start.localTime)))
        },
        dao.filterBy(List(BlacklistEndFilter(randomBlacklist.end.stringMillis))).result map { blacklists =>
          blacklists shouldBe dbEntities.filter(b => blacklists.exists(_.end.localTime.isEqual(b.end.localTime)))
        },
        dao.filterBy(List(BlacklistSinceFilter(since.stringMillis), BlacklistUntilFilter(until.stringMillis))).result map { blacklists =>
          blacklists.forall { b =>
            val date = b.date.localDate
            val lower = since.localDate
            val upper = until.localDate

            (date.isAfter(lower) || date.isEqual(lower)) && (date.isBefore(upper) || date.isEqual(upper))
          } shouldBe true
        }
      )
    }
  }

  override protected def name: String = "blacklist"

  override protected val dbEntity: BlacklistDb = BlacklistDb.entireDay("label", LocalDate.now.sqlDate, global = true)

  override protected val invalidDuplicateOfDbEntity: BlacklistDb = dbEntity

  override protected val invalidUpdateOfDbEntity: BlacklistDb = dbEntity.copy("label 2", global = !dbEntity.global)

  override protected val validUpdateOnDbEntity: BlacklistDb = dbEntity.copy("label 2")

  override protected val dbEntities: List[BlacklistDb] = blacklists

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq()

  override protected val lwmAtom: Blacklist = dbEntity.toUniqueEntity

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
