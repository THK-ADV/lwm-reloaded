package services

import base.PostgresDbSpec
import models.UniqueEntity
import slick.dbio.Effect.Write
import store.UniqueTable
import slick.driver.PostgresDriver.api._

abstract class AbstractDaoSpec[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueEntity, LwmModel <: UniqueEntity]
  extends PostgresDbSpec with AbstractDao[T, DbModel, LwmModel] {

  protected def name: String
  protected def entityToDelete: DbModel
  protected def entities: List[DbModel]

  override protected def customFill: DBIOAction[Unit, NoStream, Write]

  s"A AbstractDaoSpec with $name " should {

    s"create a $name" in {
      await(create(entityToDelete)) shouldBe entityToDelete
    }

    s"create many $name" in {
      await(createMany(entities)) shouldBe entities
    }

    s"delete a $name by invalidating it" in {
      await(delete(entityToDelete)) shouldBe defined
    }
  }
}
