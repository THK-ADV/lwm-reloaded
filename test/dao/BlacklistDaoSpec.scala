package dao

import models.{BlacklistDb, PostgresBlacklist}
import org.joda.time.LocalDate
import slick.jdbc.PostgresProfile.api._
import store.BlacklistTable

final class BlacklistDaoSpec extends AbstractDaoSpec[BlacklistTable, BlacklistDb, PostgresBlacklist] with BlacklistDao {
  import dao.AbstractDaoSpec._
  import utils.LwmDateTime._
  import models.PostgresBlacklist._

  "A BlacklistService2Spec also" should {

    // TODO ADD FETCHING SPEC FROM BLACKLISTSERVICESPEC

    "filter properly" in {
      val (since, until) = {
        val chosen = takeSomeOf(dbEntities.
          filter(b => !b.start.localTime.isEqual(startOfDay))).
          toList.
          sortWith((a, b) => a.date.localDate.isBefore(b.date.localDate))

        (chosen.head.date, takeOneOf(chosen.tail).date)
      }

      run(DBIO.seq(
        filterBy(List(BlacklistGlobalFilter(true.toString))).result.map { blacklists =>
          blacklists shouldBe dbEntities.filter(_.global)
          blacklists.forall(b => b.start.localTime.isEqual(startOfDay) && b.end.localTime.isEqual(endOfDay)) shouldBe true
        },
        filterBy(List(BlacklistDateFilter(randomBlacklist.date.stringMillis))).result.map { blacklists =>
          blacklists shouldBe dbEntities.filter(b => blacklists.exists(_.date.localDate.isEqual(b.date.localDate)))
        },
        filterBy(List(BlacklistStartFilter(randomBlacklist.start.stringMillis))).result.map { blacklists =>
          blacklists shouldBe dbEntities.filter(b => blacklists.exists(_.start.localTime.isEqual(b.start.localTime)))
        },
        filterBy(List(BlacklistEndFilter(randomBlacklist.end.stringMillis))).result.map { blacklists =>
          blacklists shouldBe dbEntities.filter(b => blacklists.exists(_.end.localTime.isEqual(b.end.localTime)))
        },
        filterBy(List(BlacklistSinceFilter(since.stringMillis), BlacklistUntilFilter(until.stringMillis))).result.map { blacklists =>
          blacklists.forall { b =>
            val date = b.date.localDate
            val lower = since.localDate
            val upper = until.localDate

            (date.isAfter(lower) || date.isEqual(lower)) && (date.isBefore(upper) || date.isEqual(upper))
          } shouldBe true
        }
      ))
    }
  }

  override protected def name: String = "blacklist"

  override protected val dbEntity: BlacklistDb = BlacklistDb.entireDay("label", LocalDate.now.sqlDate, global = true)

  override protected val invalidDuplicateOfDbEntity: BlacklistDb = dbEntity

  override protected val invalidUpdateOfDbEntity: BlacklistDb = dbEntity.copy("label 2", global = !dbEntity.global)

  override protected val validUpdateOnDbEntity: BlacklistDb = dbEntity.copy("label 2")

  override protected val dbEntities: List[BlacklistDb] = blacklists

  override protected val lwmEntity: PostgresBlacklist = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresBlacklist = lwmEntity

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq()
}
