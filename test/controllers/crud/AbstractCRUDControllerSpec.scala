package controllers.crud

import base.TestBaseDefinition
import models._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.Value
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.{Sesame, SesameModule}
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{GroupService, RoleService, SessionHandlingService, TimetableService}
import store.bind.Bindings
import store.sparql.{Initial, QueryEngine, QueryExecutor, SelectClause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Success}

abstract class AbstractCRUDControllerSpec[I, O <: UniqueEntity] extends WordSpec with TestBaseDefinition with SesameModule { self =>

  val factory = ValueFactoryImpl.getInstance()
  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val groupService = mock[GroupService]
  val timetableService = mock[TimetableService]
  val sessionService = mock[SessionHandlingService]
  val qe = mock[QueryExecutor[SelectClause]]
  val bindings: Bindings[Sesame] = Bindings[Sesame](namespace)
  val query = QueryEngine.empty(qe)

  def pointedGraph: PointedGraph[Rdf]

  def entityToPass: O

  def entityToFail: O

  def mimeType: LwmMimeType

  def controller: AbstractCRUDController[I, O]

  def entityTypeName: String

  val inputJson: JsValue

  val updateJson: JsValue

  implicit def jsonWrites: Writes[O]

  def namespace: Namespace = Namespace("http://testNamespace/")

  def plural(s: String): String = if(s.endsWith("y")) s.take(s.length - 1) + "ies" else s + "s"

  when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
  when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

  s"A ${entityTypeName}CRUDController " should {
    s"successfully create a new $entityTypeName" in {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(repository.add(anyObject())(anyObject())).thenReturn(Success(pointedGraph))
      when(repository.getAll[O](anyObject())).thenReturn(Success(Set.empty[O]))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.create()(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldEqual Json.toJson(entityToPass)
    }

    s"not create a new $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant create $entityTypeName for some reason"
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(repository.add(anyObject())(anyObject())).thenReturn(Failure(new Exception(errorMessage)))
      when(repository.getAll[O](anyObject())).thenReturn(Success(Set.empty[O]))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.create()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"fail while validating a $entityTypeName with invalid json data" in {
      val json = Json.toJson("no valid data")

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )
      val result = controller.create()(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) should include("KO")
      contentAsString(result) should include("errors")
    }

    s"not get a single $entityTypeName when its not found" in {
      when(repository.get[O](anyString())(anyObject())).thenReturn(Success(None))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToFail.id}"
      )
      val result = controller.get(entityToFail.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    s"not get a single $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"
      when(repository.get[O](anyString())(anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToFail.id}"
      )
      val result = controller.get(entityToFail.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"successfully get a single $entityTypeName" in {
      when(repository.get[O](anyString())(anyObject())).thenReturn(Success(Some(entityToPass)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.get(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entityToPass)
    }

    s"successfully get all ${plural(entityTypeName)}" in {
      val allEntities = Set(entityToPass, entityToFail)
      when(repository.getAll[O](anyObject())).thenReturn(Success(allEntities))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(allEntities)
    }

    s"not get all ${plural(entityTypeName)} when there is an exception" in {
      val errorMessage = s"Oops, cant get all ${entityTypeName}s for some reason"
      when(repository.getAll[O](anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"successfully delete an existing $entityTypeName" in {
      when(repository.delete(anyString())(anyObject())).thenReturn(Success(()))

      val expectedPassModel = s"""{"status":"OK","id":"${namespace.base}${if(entityTypeName.endsWith("y")) entityTypeName.take(entityTypeName.length - 1) + "ie" else entityTypeName}s/${entityToPass.id}"}"""
      val request = FakeRequest(
        DELETE,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.delete(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsString(result).split(" ") contains expectedPassModel
    }

    s"not delete an existing $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant delete the desired $entityTypeName for some reason"
      when(repository.delete(anyString())(anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val request = FakeRequest(
        DELETE,
        s"/${entityTypeName}s/${entityToFail.id}"
      )
      val result = controller.delete(entityToFail.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"successfully update an existing $entityTypeName" in {
      when(repository.get[O](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(pointedGraph))

      val request = FakeRequest(
        PUT,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        updateJson
      )
      val result = controller.update(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entityToPass)
    }

    s"create instead of update an existing $entityTypeName when resource does not exists" in {
      when(repository.get[O](anyObject())(anyObject())).thenReturn(Success(None))
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(repository.add(anyObject())(anyObject())).thenReturn(Success(pointedGraph))

      val request = FakeRequest(
        PUT,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        updateJson
      )
      val result = controller.update(entityToPass.id.toString)(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entityToPass)
    }

    s"not update an existing $entityTypeName when a duplicate occur" in {
      when(repository.get[O](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))

      val request = FakeRequest(
        PUT,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.update(entityToPass.id.toString)(request)

      status(result) shouldBe ACCEPTED
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> entityToPass.id
      )
    }

    s"not update an existing $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant update the desired $entityTypeName for some reason"
      when(repository.get[O](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val request = FakeRequest(
        PUT,
        s"/${entityTypeName}s/${entityToFail.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        updateJson
      )
      val result = controller.update(entityToFail.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"not update an existing $entityTypeName with invalid json data" in {
      when(repository.get[O](anyString())(anyObject())).thenReturn(Success(Some(entityToPass)))

      val json = Json.toJson("no valid data")
      val request = FakeRequest(
        PUT,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )
      val result = controller.update(entityToPass.id.toString)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) should include("KO")
      contentAsString(result) should include("errors")
    }

    s"return the expected content type for $entityTypeName" in {
      val request = FakeRequest(
        HEAD,
        s"/${entityTypeName}s"
      )

      val result = controller.header()(request)

      status(result) shouldBe NO_CONTENT
      contentType(result) shouldBe Some[String](mimeType)
      contentAsString(result) shouldBe empty
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    repository.connect { conn =>
      repository.rdfStore.removeGraph(conn, repository.ns)
    }
  }
}
