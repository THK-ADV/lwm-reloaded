package controllers

import base.TestBaseDefinition
import models._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.impl.{LinkedHashModel, ValueFactoryImpl}
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import play.api.http.HeaderNames
import play.api.libs.json.{Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import store.{Namespace, SesameRepository}

import scala.util.{Failure, Success}

abstract class AbstractCRUDControllerSpec[T <: UniqueEntity] extends WordSpec with TestBaseDefinition {
  val factory = ValueFactoryImpl.getInstance()


  lazy val passModel: LinkedHashModel = {
    val m = new LinkedHashModel()
    m.add(factory.createURI(s"http://${namespace.base}${entityTypeName.toLowerCase}s/${entityToPass.id}"), factory.createURI("http://somePredicate"), factory.createLiteral(""))
    m
  }


  val repository = mock[SesameRepository]

  def entityToPass: T

  def entityToFail: T


  def controller: AbstractCRUDController[T]

  def entityTypeName: String

  implicit def jsonWrites: Writes[T]


  def namespace: Namespace = Namespace("http://testNamespace/")


  s"A ${entityTypeName}CRUDController " should {
    s"successfully create a new $entityTypeName" in {
      when(repository.add(anyObject())(anyObject())).thenReturn(Success(passModel))

      val json = Json.toJson(entityToPass)

      val expected = s"""{"status":"OK","id":"http://${namespace.base}${entityTypeName.toLowerCase}s/${entityToPass.id}"}"""

      val request = FakeRequest(
        POST,
        s"/${entityTypeName.toLowerCase}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      )
      val result = controller.create()(request)

      status(result) shouldBe CREATED
      contentAsString(result) shouldBe expected

    }
    s"not create a new $entityTypeName when there is an exception" in {
      when(repository.add(anyObject())(anyObject())).thenReturn(Failure(new Exception("mimimimi")))

      val json = Json.toJson(entityToFail)

      val request = FakeRequest(
        POST,
        s"/${entityTypeName.toLowerCase}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> "application/json")),
        json
      )
      val result = controller.create()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsString(result) should include ("KO")
    }
  }
}
