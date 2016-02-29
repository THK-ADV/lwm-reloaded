package controllers.crud

import java.util.UUID

import models.{Degree, DegreeProtocol}
import org.mockito.{Matchers, Mockito}
import org.mockito.Matchers.anyObject
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType
import play.api.test.Helpers._

import scala.util.Success

class DegreeCRUDControllerSpec extends AbstractCRUDControllerSpec[DegreeProtocol, Degree] {
  override val entityToPass: Degree = Degree("label to pass", "abbreviation to pass", Degree.randomUUID)

  override val controller: AbstractCRUDController[DegreeProtocol, Degree] = new DegreeCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: DegreeProtocol, id: Option[UUID]): Degree = entityToPass
  }

  override val entityToFail: Degree = Degree("label to fail", "abbreviation to fail", Degree.randomUUID)

  override implicit val jsonWrites: Writes[Degree] = Degree.writes

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

  import bindings.DegreeBinding.degreeBinder
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A DegreeCRUDControllerSpec also " should {

    s"handle this model issue when creating a new $entityTypeName which already exists" in {
      Mockito.when(repository.prepareQuery(anyObject())).thenReturn(query)
      Mockito.when(qe.execute(anyObject())).thenReturn(Success(Map(
        "id" -> List(factory.createLiteral(entityToPass.id.toString))
      )))

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
      when(repository.get[Degree](anyObject())(anyObject())).thenReturn(Success(None))
      when(repository.prepareQuery(Matchers.anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "id" -> List(factory.createLiteral(entityToPass.id.toString))
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