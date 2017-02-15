package controllers

import models.{PostgresDegree$, DegreeProtocol}
import org.mockito.Matchers
import org.mockito.Matchers.anyObject
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.util.Success

class DegreeCRUDControllerSpec extends AbstractCRUDControllerSpec[DegreeProtocol, Degree, Degree] {
  override val entityToPass: Degree = PostgresDegree("label to pass", "abbreviation to pass")

  override val entityToFail: Degree = PostgresDegree("label to fail", "abbreviation to fail")

  override implicit val jsonWrites: Writes[Degree] = PostgresDegree.writes

  override val atomizedEntityToPass: Degree = entityToPass

  override val atomizedEntityToFail: Degree = entityToFail

  override val jsonWritesAtom: Writes[Degree] = jsonWrites

  override val controller: DegreeCRUDController = new DegreeCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: DegreeProtocol, existing: Option[Degree]): Degree = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val mimeType: LwmMimeType = LwmMimeType.degreeV1Json

  override def entityTypeName: String = "degree"

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "abbreviation" -> entityToPass.abbreviation
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> s"${entityToPass.label} updated",
    "abbreviation" -> s"${entityToPass.abbreviation} updated"
  )

  import bindings.DegreeDescriptor
  import ops._

  implicit val degreeBinder = DegreeDescriptor.binder

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A DegreeCRUDControllerSpec also " should {

    s"handle this model issue when creating a new $entityTypeName which already exists" in {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "s" -> List(factory.createLiteral(PostgresDegree.generateUri(entityToPass)))
      )))
      when(repository.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        inputJson
      )
      val result = controller.create()(request)

      status(result) shouldBe ACCEPTED
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> entityToPass.id
      )
    }

    s"neither create or update an existing $entityTypeName when resource does not exists although body would lead to duplication" in {
      doReturn(Success(None)).doReturn(Success(Some(entityToPass))).when(repository).get(anyObject())(anyObject())
      when(repository.prepareQuery(Matchers.anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "s" -> List(factory.createLiteral(PostgresDegree.generateUri(entityToPass)))
      )))

      val request = FakeRequest(
        PUT,
        s"/${entityTypeName}s/${entityToPass.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        updateJson
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
  }
}