package dao

import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import slick.dbio.{DBIOAction, NoStream}
import slick.jdbc.PostgresProfile.api._

abstract class AbstractExpandableDaoSpec[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity]
  extends AbstractDaoSpec[T, DbModel, LwmModel] {

  import scala.concurrent.ExecutionContext.Implicits.global

  protected final def zipAndCompare[Db, Lwm <: UniqueEntity](lhs: Traversable[Db], rhs: Traversable[Lwm]): Boolean = {
    if (lhs.isEmpty) false else (lhs.map(_.asInstanceOf[Lwm]) ++ rhs).groupBy(_.id).forall {
      case (_, pairs) => pairs.size == 2 && pairs.head == pairs.last
    }
  }

  private def assertEverythingOf(dbModel: List[DbModel], isDefined: Boolean): Unit = {
    val ids = dbModel.map(_.id)
    val nonAtomic = dbModel.map(_.toUniqueEntity)
    val atomic = dbModel.map(atom)

    val future = for {
      a <- dao.getMany(ids, atomic = false) if a.nonEmpty == isDefined
      b <- dao.getMany(ids) if b.nonEmpty == isDefined
    } yield (zipAndCompare(a, nonAtomic), zipAndCompare(b, atomic))

    async(future) { b =>
      b._1 shouldBe isDefined
      b._2 shouldBe isDefined
    }

    runAsyncSequence(dbModel map (c => expanderSpecs(c, isDefined)): _*)
  }

  protected def toAdd: List[DbModel]

  protected def numberOfUpdates: Int

  protected def numberOfDeletions: Int

  protected def update(toUpdate: List[DbModel]): List[DbModel]

  protected def atom(dbModel: DbModel): LwmModel

  protected def expanderSpecs(dbModel: DbModel, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read]

  s"A AbstractExpandableDaoSpec with $name " should {

    s"create $name's by expanding" in {
      async(dao.createMany(toAdd))(_ shouldBe toAdd)
      assertEverythingOf(toAdd, isDefined = true)
    }

    s"update few $name's by expanding" in {
      val updated = update(toAdd.take(numberOfUpdates))

      async(dao.updateMany(updated))(_ shouldBe updated)
      assertEverythingOf(updated, isDefined = true)
    }

    s"delete few $name's by expanding" in {
      val toDelete = toAdd.slice(numberOfUpdates, numberOfUpdates + numberOfDeletions)
      val remainingAfterDelete = toAdd.drop(numberOfUpdates + numberOfDeletions)

      async(dao.deleteManyEntities(toDelete))(_.size shouldBe toDelete.size)
      assertEverythingOf(toDelete, isDefined = false)
      assertEverythingOf(remainingAfterDelete, isDefined = true)
    }
  }
}
