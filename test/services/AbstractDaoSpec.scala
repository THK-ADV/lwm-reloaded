package services

import base.PostgresDbSpec
import models.UniqueEntity
import slick.dbio.Effect.Write
import store.UniqueTable
import slick.driver.PostgresDriver.api._

abstract class AbstractDaoSpec[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueEntity, LwmModel <: UniqueEntity]
  extends PostgresDbSpec with AbstractDao[T, DbModel, LwmModel] {

  protected def name: String
  protected def entity: DbModel
  protected def invalidDuplicateOfEntity: DbModel
  protected def invalidUpdateOfEntity: DbModel
  protected def validUpdateOnEntity: DbModel
  protected def entities: List[DbModel]

  override protected def dependencies: DBIOAction[Unit, NoStream, Write]

  s"A AbstractDaoSpec with $name " should {

    s"create a $name" in {
      await(create(entity)) shouldBe entity
    }

    s"not create a $name because model already exists" in {
      await(create(invalidDuplicateOfEntity).failed) shouldBe ModelAlreadyExists(Seq(entity))
    }

    s"not update a $name because model already exists" in {
      await(update(invalidUpdateOfEntity).failed) shouldBe ModelAlreadyExists(entity)
    }

    s"update a $name properly" in {
      await(update(validUpdateOnEntity)) shouldBe Some(validUpdateOnEntity)
    }

    s"create many $name" in {
      await(createMany(entities)) shouldBe entities
    }

    s"delete a $name by invalidating it" in {
      await(delete(entity)) shouldBe defined
    }
  }
}
