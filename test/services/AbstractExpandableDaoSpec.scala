package services

import models.{UniqueDbEntity, UniqueEntity}
import slick.dbio.{DBIOAction, NoStream}
import slick.driver.PostgresDriver.api._
import store.UniqueTable

abstract class AbstractExpandableDaoSpec[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity]
  extends AbstractDaoSpec[T, DbModel, LwmModel] {

  protected final def zipAndCompare[Db, Lwm <: UniqueEntity](lhs: Traversable[Db], rhs: Traversable[Lwm]): Boolean = {
    if (lhs.isEmpty) false else (lhs.map(_.asInstanceOf[Lwm]) ++ rhs).groupBy(_.id).forall {
      case (id, pairs) => pairs.size == 2 && pairs.forall(_.id == id) && pairs.head == pairs.last
    }
  }

  private def assertEverythingOf(dbModel: List[DbModel], isDefined: Boolean): Unit = {
    val ids = dbModel.map(_.id)
    val nonAtomic = dbModel.map(_.toLwmModel)
    val atomic = dbModel.map(atom)

    zipAndCompare(await(getMany(ids, atomic = false)), nonAtomic) shouldBe isDefined
    zipAndCompare(await(getMany(ids)), atomic) shouldBe isDefined

    dbModel foreach (c => run(expanderSpecs(c, isDefined)))
  }

  protected def toAdd: List[DbModel]
  protected def numberOfUpdates: Int
  protected def numberOfDeletions: Int

  protected def update(toUpdate: List[DbModel]): List[DbModel]
  protected def atom(dbModel: DbModel): LwmModel
  protected def expanderSpecs(dbModel: DbModel, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read]

  s"A AbstractExpandableDaoSpec with $name " should {

    s"create $name's by expanding" in {
      await(createMany(toAdd)) shouldBe toAdd
      assertEverythingOf(toAdd, isDefined =  true)
    }

    s"update few $name's by expanding" in {
      val updated = update(toAdd.take(numberOfUpdates))

      await(updateMany(updated)) shouldBe updated.map(Some(_))
      assertEverythingOf(updated, isDefined =  true)
    }

    s"delete few $name's by expanding" in {
      val toDelete = toAdd.slice(numberOfUpdates, numberOfUpdates + numberOfDeletions)
      val remainingAfterDelete = toAdd.drop(numberOfUpdates + numberOfDeletions)

      await(deleteManyEntities(toDelete)).flatten.size shouldBe toDelete.size

      assertEverythingOf(toDelete, isDefined =  false)
      assertEverythingOf(remainingAfterDelete, isDefined =  true)
    }
  }
}
