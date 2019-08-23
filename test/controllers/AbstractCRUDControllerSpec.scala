package controllers

import java.sql.Timestamp
import java.util.UUID

import base.DatabaseSpec
import dao.{AbstractDao, AuthorityDao}
import database.UniqueTable
import javax.inject.Inject
import models.{UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import org.scalatest.mockito.MockitoSugar
import play.api.inject.guice.GuiceableModule
import play.api.libs.json._
import play.api.mvc._
import play.api.test.Helpers._
import play.api.test._
import security.SecurityActionChain
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

import scala.concurrent.{ExecutionContext, Future}

case class FakeProtocol(string: String, int: Int)

case class FakeModel(string: String, int: Int, id: UUID) extends UniqueEntity

class FakeTable(tag: Tag) extends Table[FakeDb](tag, "FAKE") with UniqueTable {
  def string = column[String]("STRING")

  def int = column[Int]("INT")

  override def * = (string, int, lastModified, invalidated, id) <> ((FakeDb.apply _).tupled, FakeDb.unapply)
}

case class FakeDb(string: String, int: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = FakeModel(string, int, id)
}

class FakeAbstractDao @Inject()(val db: Database, implicit val executionContext: ExecutionContext) extends AbstractDao[FakeTable, FakeDb, FakeModel] {
  override protected val tableQuery = TableQuery[FakeTable]

  override protected def toAtomic(query: PostgresProfile.api.Query[FakeTable, FakeDb, Seq]) = toUniqueEntity(query)

  override protected def toUniqueEntity(query: PostgresProfile.api.Query[FakeTable, FakeDb, Seq]) = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: FakeDb): PostgresProfile.api.Query[FakeTable, FakeDb, Seq] = {
    filterValidOnly(_.string === entity.string)
  }

  override protected def shouldUpdate(existing: FakeDb, toUpdate: FakeDb): Boolean = {
    existing.int != toUpdate.int && existing.string == toUpdate.string
  }
}

class FakeAbstractCRUDController @Inject()(
  val cc: ControllerComponents,
  val abstractDao: FakeAbstractDao,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain
) extends AbstractCRUDController[FakeProtocol, FakeTable, FakeDb, FakeModel](cc) {
  override implicit protected def writes: Writes[FakeModel] = Json.writes[FakeModel]

  override implicit protected def reads: Reads[FakeProtocol] = Json.reads[FakeProtocol]

  override protected def toDbModel(protocol: FakeProtocol, existingId: Option[UUID]): FakeDb = {
    FakeDb(protocol.string, protocol.int, id = existingId getOrElse UUID.randomUUID)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => NonSecureBlock
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => NonSecureBlock
  }
}

class AbstractCRUDControllerSpec extends DatabaseSpec with MockitoSugar with Results {

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  val dao = new FakeAbstractDao(
    db,
    app.injector.instanceOf(classOf[ExecutionContext])
  )

  val controller = new FakeAbstractCRUDController(
    app.injector.instanceOf(classOf[ControllerComponents]),
    dao,
    app.injector.instanceOf(classOf[AuthorityDao]),
    app.injector.instanceOf(classOf[SecurityActionChain]),
  )

  val items = (0 until 10).map(i => FakeDb(i.toString, i)).toList

  private def contentAsJsonMessage(result: Future[Result]) = {
    contentAsJson(result).\("message").get
  }

  implicit val writesP: OWrites[FakeProtocol] = Json.writes[FakeProtocol]

  implicit val writesM: OWrites[FakeModel] = Json.writes[FakeModel]

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    async(dao.createSchema)(_ => Unit)
    async(dao.createMany(items))(_ => Unit)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    async(dao.dropSchema)(_ => Unit)
  }

  "A AbstractCRUDControllerSpec" should {

    "return all values" in {
      val request = FakeRequest()
      val result = controller.all().apply(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe a[JsArray]
    }

    "fail a get request if filter are bad" in {
      val request = FakeRequest("GET", "?foo=bar")
      val result = controller.all().apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result) shouldBe JsString("no filter for foo and bar")
    }

    "return get a single value" in {
      val request = FakeRequest()
      val result = controller.get(items.head.id.toString).apply(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe a[JsObject]
    }

    "return no value if not found" in {
      val request = FakeRequest()
      val id = UUID.randomUUID.toString
      val result = controller.get(id).apply(request)

      status(result) shouldBe NOT_FOUND
      contentAsJsonMessage(result) shouldBe JsString(s"No such element for $id")
    }

    "fail to get a single value if id is broken" in {
      val request = FakeRequest()
      val result = controller.get("broken").apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result) shouldBe JsString("Invalid UUID string: broken")
    }

    "create a new value" in {
      val json = JsArray(Seq(Json.toJson(FakeProtocol("fake", 0))))
      val request = FakeRequest().withJsonBody(json)
      val result = controller.create().apply(request)

      status(result) shouldBe OK

      val createdJson = contentAsJson(result).\("created").as[JsArray].\(0)
      createdJson.\("string").get shouldBe JsString("fake")
      createdJson.\("int").get shouldBe JsNumber(0)
      createdJson.\("id").get shouldBe a[JsString]
    }

    "fail creation if body is bad json" in {
      val request = FakeRequest().withJsonBody(JsArray(Seq(JsString(""))))
      val result = controller.create().apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result).toString() should include("error.expected")
    }

    "fail creation if there is no body " in {
      val request = FakeRequest()
      val result = controller.create().apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result) shouldBe JsString("json body is required")
    }

    "update a value" in {
      val first = items.head
      val body = FakeProtocol(first.string, 3201)
      val request = FakeRequest().withJsonBody(Json.toJson(body))
      val result = controller.update(first.id.toString).apply(request)

      status(result) shouldBe OK
      contentAsJson(result).\("updated").get shouldBe Json.toJson(FakeModel(body.string, body.int, first.id))
    }

    "fail update if model already exists" in {
      val first = items.head
      val body = FakeProtocol("can't update that", -1)
      val request = FakeRequest().withJsonBody(Json.toJson(body))
      val result = controller.update(first.id.toString).apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result).toString should include("model already exists")
    }

    "fail update if json is bad" in {
      val first = items.head
      val request = FakeRequest().withJsonBody(JsString(""))
      val result = controller.update(first.id.toString).apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result).toString should include("error.expected")
    }

    "fail update if there is no body" in {
      val first = items.head
      val request = FakeRequest()
      val result = controller.update(first.id.toString).apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result).toString should include("json body is required")
    }

    "fail update if value can't be found" in {
      val first = items.head
      val body = FakeProtocol(first.string, 3201)
      val request = FakeRequest().withJsonBody(Json.toJson(body))
      val result = controller.update(UUID.randomUUID.toString).apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result) shouldBe JsString("Invoker.first") // TODO super wrong
    }

    "fail update if id is broken" in {
      val first = items.head
      val body = FakeProtocol(first.string, 3201)
      val request = FakeRequest().withJsonBody(Json.toJson(body))
      val result = controller.update("broken").apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result) shouldBe JsString("Invalid UUID string: broken")
    }

    "delete a value" in {
      val request = FakeRequest()
      val item = items(1)
      val result = controller.delete(item.id.toString).apply(request)

      status(result) shouldBe OK
      contentAsJson(result).\("deleted").get shouldBe Json.toJson(item.toUniqueEntity)
    }

    "fail delete if value is not found" in {
      val request = FakeRequest()
      val result = controller.delete(UUID.randomUUID.toString).apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR // TODO this is wrong
    }

    "fail delete if id is bad" in {
      val request = FakeRequest()
      val result = controller.delete("broken").apply(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJsonMessage(result) shouldBe JsString("Invalid UUID string: broken")
    }
  }
}
