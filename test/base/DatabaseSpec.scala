package base

import org.scalatest.WordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile

trait DatabaseSpec extends WordSpec with TestBaseDefinition with GuiceOneAppPerSuite with LwmFakeApplication with AsyncSpec {
  val db = app.injector.instanceOf(classOf[Database])

  implicit val profile: JdbcProfile

  import profile.api._

  protected final def runAsync[R](action: DBIOAction[R, NoStream, Nothing])(assert: R => Unit): Unit = async(db.run(action))(assert)

  protected final def runAsyncSequence[R](args: DBIO[R]*): Unit = async(db.run(DBIO.seq(args: _*).transactionally))(_ => Unit)
}
