package dao

import database.{BlacklistDb, BlacklistTable}
import models.Blacklist
import org.joda.time.LocalDate
import play.api.inject.guice.GuiceableModule
import slick.jdbc.PostgresProfile.api._

final class BlacklistDaoSpec extends AbstractDaoSpec[BlacklistTable, BlacklistDb, Blacklist] {

  import AbstractDaoSpec._
  import utils.LwmDateTime._

  val dao = app.injector.instanceOf(classOf[BlacklistDao])

  "A BlacklistService2Spec also" should { // TODO not sure which strings needs to be passed to those filters... plain string date or time, currentMillis, localized or not? this needs to be discussed first

    /*    "filter by global blacklists" in {
          runAsync(dao.filterBy(List(BlacklistGlobalFilter(true.toString))).result) { blacklists =>
            blacklists shouldBe dbEntities.filter(_.global)
          }
        }

        "filter by date" in {
          val dateFilter = randomBlacklist.date
          runAsync(dao.filterBy(List(BlacklistDateFilter(dateFilter.stringMillis))).result) { blacklists =>
            blacklists shouldBe dbEntities.filter(_.date.localDate == dateFilter.localDate)
          }
        }

        "filter by start time" in {
          val startFilter = randomBlacklist.start.localTime
          println(s"startFilter $startFilter")
          runAsync(dao.filterBy(List(BlacklistStartFilter(startFilter.stringMillis))).result) { blacklists =>
            blacklists shouldBe dbEntities.filter(_.start.localTime == startFilter)
          }
        }

        "filter by end time" in {
          val endFilter = randomBlacklist.end.localTime
          println(s"endFilter $endFilter")
          runAsync(dao.filterBy(List(BlacklistEndFilter(endFilter.stringMillis))).result) { blacklists =>
            blacklists shouldBe dbEntities.filter(_.end.localTime == endFilter)
          }
        }

        "filter by range" in {
          val e = dbEntities.sortBy(_.date.localDate).slice(3, 8)
          val since = e.head.date
          val until = e.last.date

          runAsync(dao.filterBy(List(BlacklistSinceFilter(since.stringMillis), BlacklistUntilFilter(until.stringMillis))).result) { blacklists =>
            blacklists.forall { b =>
              val date = b.date.localDate
              val lower = since.localDate
              val upper = until.localDate

              (date.isAfter(lower) || date.isEqual(lower)) && (date.isBefore(upper) || date.isEqual(upper))
            } shouldBe true
          }
        }*/
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
