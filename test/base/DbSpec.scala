package base

import org.scalatest._
import org.w3.banana.sesame.{Sesame, SesameModule}
import org.w3.banana.{RDF, RDFModule}
import store.Prefixes.LWMPrefix
import store.{SemanticRepository, SesameRepository}

abstract class SesameDbSpec extends DbSpec[Sesame] with SesameModule {
  val lwm = LWMPrefix[Sesame]
  lazy val repo = SesameRepository(s"http://testDB/${this.getClass.getSimpleName}")

  override def initDB(): Unit = {}

  override def prepareDB(): Unit = {
    initDB()
  }

  override def destroyDB(): Unit = {
    repo.close()
  }
}


trait DbSpec[R <: RDF] extends WordSpec with RDFModule with TestBaseDefinition {
  def repo: SemanticRepository[R, R#URI, R#Graph]

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

trait TestBaseDefinition extends BeforeAndAfterAll with BeforeAndAfterEach with Matchers with OptionValues with EitherValues {
  this: Suite =>
}
