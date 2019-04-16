package base

import org.scalatest.WordSpec
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile

import scala.concurrent.Future

trait DatabaseSpec extends WordSpec with TestBaseDefinition with GuiceOneAppPerSuite with LwmFakeApplication {
  val db = app.injector.instanceOf(classOf[Database])

  val profile: JdbcProfile = _root_.slick.jdbc.PostgresProfile
  import profile.api._

  protected final def async[R](future: Future[R])(assert: R => Unit): Unit = whenReady(future, timeout(Span(5, Seconds)))(assert)

  protected final def runAsync[R](action: DBIOAction[R, NoStream, Nothing])(assert: R => Unit): Unit = async(db.run(action))(assert)

  protected final def runAsyncSequence[R](args: DBIO[R]*): Unit = async(db.run(DBIO.seq(args: _*).transactionally))(_ => Unit)
}
