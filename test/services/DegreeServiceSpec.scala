package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write

class DegreeServiceSpec extends PostgresDbSpec with DegreeService {
  override protected def customFill: _root_.slick.driver.PostgresDriver.api.DBIOAction[Unit, _root_.slick.driver.PostgresDriver.api.NoStream, Write] = ???
}
