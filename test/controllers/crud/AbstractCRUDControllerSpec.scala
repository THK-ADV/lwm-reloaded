package controllers.crud

import base.TestBaseDefinition
import models._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.{Sesame, SesameModule}
import play.api.ApplicationLoader.Context
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest, WithApplicationLoader}
import play.api.{Application, ApplicationLoader}
import services.RoleService
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.{DefaultLwmApplication, LWMMimeType}

import scala.util.{Failure, Success}

abstract class AbstractCRUDControllerSpec[I, O <: UniqueEntity] extends WordSpec with TestBaseDefinition with SesameModule {

  val factory = ValueFactoryImpl.getInstance()
  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]

  val bindings: Bindings[Sesame] = Bindings[Sesame](namespace)

  def pointedGraph: PointedGraph[Rdf]

  def entityToPass: O

  def entityToFail: O

  def mimeType: LWMMimeType

  def controller: AbstractCRUDController[I, O]

  def entityTypeName: String

  val inputJson: JsValue

  implicit def jsonWrites: Writes[O]

  def namespace: Namespace = Namespace("http://testNamespace/")

  def fgrammar(s: String): String = if(s.endsWith("y")) s.take(s.length - 1) + "ies" else s + "s"

  class FakeApplication extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context).application
  })

  s"A ${entityTypeName}CRUDController " should {
    s"successfully create a new $entityTypeName" in {
      when(repository.add(anyObject())(anyObject())).thenReturn(Success(pointedGraph))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.create()(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entityToPass)
    }

    s"not create a new $entityTypeName when there is an invalid mimeType" in new FakeApplication {
      val result = route(FakeRequest(
        POST,
        s"/${if(entityTypeName.endsWith("y")) entityTypeName.take(entityTypeName.length - 1) + "ie" else entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        inputJson
      )).get

      status(result) shouldBe UNSUPPORTED_MEDIA_TYPE
      contentType(result) shouldBe Some("text/html")
      contentAsString(result) should include (s"Expecting ${mimeType.value} body")
    }

    s"not create a new $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant create $entityTypeName for some reason"
      when(repository.add(anyObject())(anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val expectedErrorMessage = s"""{"status":"KO","errors":"$errorMessage"}"""
      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.create()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
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

      val expectedErrorMessage = s"""{"status":"KO","message":"No such element..."}"""
      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToFail.id}"
      )
      val result = controller.get(entityToFail.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
    }

    s"not get a single $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"
      when(repository.get[O](anyString())(anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val expectedErrorMessage = s"""{"status":"KO","errors":"$errorMessage"}"""
      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToFail.id}"
      )
      val result = controller.get(entityToFail.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
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
      contentAsString(result) shouldBe Json.toJson(entityToPass).toString()
    }

    s"successfully get all ${fgrammar(entityTypeName)}" in {
      val allEntities = Set(entityToPass, entityToFail)
      when(repository.get[O](anyObject(), anyObject())).thenReturn(Success(allEntities))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsString(result) shouldBe Json.toJson(allEntities).toString()
    }

    s"not get all ${fgrammar(entityTypeName)} when there is an exception" in {
      val errorMessage = s"Oops, cant get all ${entityTypeName}s for some reason"
      when(repository.get[O](anyObject(), anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val expectedErrorMessage = s"""{"status":"KO","errors":"$errorMessage"}"""
      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
    }

    s"successfully delete an existing $entityTypeName" in {
      when(repository.delete(anyString())).thenReturn(Success(pointedGraph.graph))
      val expectedPassModel = s"""{"status":"OK","id":"${namespace.base}${if(entityTypeName.endsWith("y")) entityTypeName.take(entityTypeName.length - 1) + "ie" else entityTypeName}s/${entityToPass.id}"}"""
      val request = FakeRequest(
        DELETE,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.delete(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedPassModel
    }

    s"not delete an existing $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant delete the desired $entityTypeName for some reason"
      when(repository.delete(anyString())).thenReturn(Failure(new Exception(errorMessage)))

      val expectedErrorMessage = s"""{"status":"KO","errors":"$errorMessage"}"""
      val request = FakeRequest(
        DELETE,
        s"/${entityTypeName}s/${entityToFail.id}"
      )
      val result = controller.delete(entityToFail.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
    }

    s"successfully update an existing $entityTypeName" in {
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(pointedGraph))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.update(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entityToPass)
    }

    s"not update an existing $entityTypeName when there is an exception" in {
      val errorMessage = s"Oops, cant update the desired $entityTypeName for some reason"
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val expectedErrorMessage = s"""{"status":"KO","errors":"$errorMessage"}"""
      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s/${entityToFail.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.update(entityToFail.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
    }

    s"not update an existing $entityTypeName when its not found" in {
      when(repository.get[O](anyString())(anyObject())).thenReturn(Success(None))

      val expectedErrorMessage = s"""{"status":"KO","message":"No such element..."}"""
      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.update(entityToPass.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsString(result) shouldBe expectedErrorMessage
    }

    s"not update an existing $entityTypeName with invalid json data" in {
      when(repository.get[O](anyString())(anyObject())).thenReturn(Success(Some(entityToPass)))

      val json = Json.toJson("no valid data")
      val request = FakeRequest(
        POST,
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

    s"should return the expected content type for $entityTypeName" in {
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
}
