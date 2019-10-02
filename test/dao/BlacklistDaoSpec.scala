package dao

import java.util.UUID

import database.{BlacklistDb, BlacklistTable}
import models.Blacklist
import org.joda.time.{LocalDate, LocalTime}
import play.api.inject.guice.GuiceableModule
import slick.jdbc.PostgresProfile.api._

final class BlacklistDaoSpec extends AbstractDaoSpec[BlacklistTable, BlacklistDb, Blacklist] {

  import AbstractDaoSpec._
  import utils.date.DateTimeOps._

  val dao = app.injector.instanceOf(classOf[BlacklistDao])

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
}
