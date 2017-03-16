package base

import org.scalatest._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.w3.banana.sesame.{Sesame, SesameModule}
import org.w3.banana.{RDF, RDFModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store._

import scala.concurrent.{Await, Future}

abstract class PostgresDbSpec extends WordSpec with TestBaseDefinition with PostgresDatabase {
  import slick.driver.PostgresDriver.api._
  import scala.concurrent.duration._

  override lazy val db = Database.forConfig("database_test")
  implicit lazy val executionContext = scala.concurrent.ExecutionContext.Implicits.global

  final def await[T](future: Future[T]) = Await.result(future, Duration.Inf)
}

abstract class SesameDbSpec extends DbSpec[Sesame] with SesameModule {
  val lwm = LWMPrefix[Sesame]
  implicit val namespace = Namespace(s"http://testDB/${this.getClass.getSimpleName}")

  val bindings = Bindings[Sesame](namespace)

  lazy val repo = SesameRepository(namespace)

  override def initDB(): Unit = {}

  override def prepareDB(): Unit = {
    initDB()
  }

  override def destroyDB(): Unit = {
    repo.close()
  }
}


trait DbSpec[R <: RDF] extends WordSpec with RDFModule with TestBaseDefinition {
  def repo: SemanticRepository

  def initDB(): Unit

  def prepareDB(): Unit

  def destroyDB(): Unit

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    prepareDB()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    destroyDB()
  }
}

trait TestBaseDefinition extends BeforeAndAfterAll with BeforeAndAfterEach with Matchers with ScalaFutures with OptionValues with EitherValues {
  this: Suite =>
}
