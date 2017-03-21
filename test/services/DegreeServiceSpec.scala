package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._

class DegreeServiceSpec extends PostgresDbSpec with DegreeService {
  override protected def customFill: DBIOAction[Unit, NoStream, Write] = ???
}
