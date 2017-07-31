package dao

import models.{PermissionDb, Permissions, PostgresPermission}
import store.PermissionTable

class PermissionDaoSpec extends AbstractDaoSpec[PermissionTable, PermissionDb, PostgresPermission] with PermissionDao{
  import dao.AbstractDaoSpec._
  import slick.driver.PostgresDriver.api._

  "A PermissionServiceSpec" should {
    "filter by prefix" in {
      val toCheck = (Permissions.labwork.all ++ Permissions.labworkApplication.all).map(_.value)

      addAllAndThen{
        filterBy(List(PermissionPrefixFilter("abw"))).result.map { perms =>
          perms.forall(p => toCheck.contains(p.value)) shouldBe true
        }
      }
    }

    "filter by suffix" in {
      val getPerms = Permissions.all.filter(p => p.value.split(":")(1).contains("get")).map(_.value).toSeq

      addAllAndThen {
        filterBy(List(PermissionSuffixFilter("get"))).result.map { perms =>
          perms.forall(p => getPerms.contains(p.value)) shouldBe true
        }
      }
    }
  }

  private def addAllAndThen(method: DBIOAction[Unit, NoStream, Effect.Read]) = {
    val allPerms = Permissions.all.map(p => PermissionDb(p.value, "")).toSeq
    await(db.run(DBIO.seq(
      createManyQuery(allPerms)
    ).andThen(
      method
    )))
  }

  override protected def name: String = "permission"

  override protected def dependencies: DBIOAction[Unit, NoStream, Effect] = DBIO.seq()

  override protected val dbEntity: PermissionDb = PermissionDb("value", "description")

  override protected val invalidDuplicateOfDbEntity: PermissionDb = PermissionDb(dbEntity.value, "description")

  override protected val invalidUpdateOfDbEntity: PermissionDb = dbEntity.copy("value2", dbEntity.description)

  override protected val validUpdateOnDbEntity: PermissionDb = dbEntity.copy(dbEntity.value, "updateDescription")

  override protected val dbEntities: List[PermissionDb] = permissions

  override protected val lwmEntity: PostgresPermission = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresPermission = dbEntity.toLwmModel
}
