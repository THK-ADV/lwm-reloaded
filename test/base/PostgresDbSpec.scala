package base

import database.helper.DatabaseTables
import slick.jdbc.JdbcProfile

abstract class PostgresDbSpec extends DatabaseSpec with DatabaseTables {
  override implicit val profile: JdbcProfile = _root_.slick.jdbc.PostgresProfile

  import profile.api._

  protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write]

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    createSchemas()
  }


  override protected def afterAll(): Unit = {
    super.afterAll()

    dropSchemas()
    db.close()
  }

  protected def createSchemas(): Unit = runAsyncSequence(createAction(), dependencies)

  protected def dropSchemas(): Unit = runAsyncSequence(dropAction())

  protected def clearTables(): Unit = runAsyncSequence(tables.reverseMap(_.delete): _*)
}
