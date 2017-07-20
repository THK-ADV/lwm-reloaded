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

    val start1 = System.currentTimeMillis()
    val a = await(getMany(ids, atomic = false))
    println(s"await(getMany(ids, atomic = false)) ${System.currentTimeMillis() - start1}")

    val start2 = System.currentTimeMillis()
    val b = await(getMany(ids))
    println(s"await(getMany(ids)) ${System.currentTimeMillis() - start2}")

    val start3 = System.currentTimeMillis()
    zipAndCompare(a, nonAtomic) shouldBe isDefined
    println(s"zipAndCompare(a, nonAtomic) shouldBe isDefined ${System.currentTimeMillis() - start3}")

    val start4 = System.currentTimeMillis()
    zipAndCompare(b, atomic) shouldBe isDefined
    println(s"zipAndCompare(b, atomic) shouldBe isDefined ${System.currentTimeMillis() - start4}")

    val start5 = System.currentTimeMillis()
    dbModel foreach (c => run(expanderSpecs(c, isDefined)))
    println(s"dbModel foreach (c => run(expanderSpecs(c, isDefined))) ${System.currentTimeMillis() - start5}")
  }

  protected def toAdd: List[DbModel]
  protected def numberOfUpdates: Int
  protected def numberOfDeletions: Int

  protected def update(toUpdate: List[DbModel]): List[DbModel]
  protected def atom(dbModel: DbModel): LwmModel
  protected def expanderSpecs(dbModel: DbModel, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read]

  s"A AbstractExpandableDaoSpec with $name " should {

    s"create $name's by expanding" in {
      val start1 = System.currentTimeMillis()
      await(createMany(toAdd)) shouldBe toAdd
      println(s"await(createMany(toAdd)) ${System.currentTimeMillis() - start1}")

      val start2 = System.currentTimeMillis()
      assertEverythingOf(toAdd, isDefined =  true)
      println(s"assertEverythingOf(toAdd, isDefined =  true) ${System.currentTimeMillis() - start2}")
    }

    s"update few $name's by expanding" in {
      val updated = update(toAdd.take(numberOfUpdates))

      val start1 = System.currentTimeMillis()
      await(updateMany(updated)) shouldBe updated.map(Some(_))
      println(s"await(updateMany(updated)) shouldBe updated.map(Some(_)) ${System.currentTimeMillis() - start1}")

      val start2 = System.currentTimeMillis()
      assertEverythingOf(updated, isDefined =  true)
      println(s"assertEverythingOf(updated, isDefined =  true) ${System.currentTimeMillis() - start2}")
    }

    s"delete few $name's by expanding" in {
      val toDelete = toAdd.slice(numberOfUpdates, numberOfUpdates + numberOfDeletions)
      val remainingAfterDelete = toAdd.drop(numberOfUpdates + numberOfDeletions)

      val start1 = System.currentTimeMillis()
      await(deleteManyEntities(toDelete)).flatten.size shouldBe toDelete.size
      println(s"await(deleteManyEntities(toDelete)).flatten.size ${System.currentTimeMillis() - start1}")

      val start2 = System.currentTimeMillis()
      assertEverythingOf(toDelete, isDefined =  false)
      println(s"assertEverythingOf(toDelete, isDefined =  false) ${System.currentTimeMillis() - start2}")

      val start3 = System.currentTimeMillis()
      assertEverythingOf(remainingAfterDelete, isDefined =  true)
      println(s"assertEverythingOf(remainingAfterDelete, isDefined =  true) ${System.currentTimeMillis() - start3}")
    }
  }
}
