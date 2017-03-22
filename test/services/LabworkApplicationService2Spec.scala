package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._

final class LabworkApplicationService2Spec extends PostgresDbSpec with LabworkApplicationService2 {
  override protected def customFill: DBIOAction[Unit, NoStream, Write] = ???

  override protected def labworkApplicationFriendService: LabworkApplicationFriendService = new LabworkApplicationFriendServiceSpec()
}

final class LabworkApplicationFriendServiceSpec extends PostgresDbSpec with LabworkApplicationFriendService {
  override protected def customFill: DBIOAction[Unit, NoStream, Write] = ???
}