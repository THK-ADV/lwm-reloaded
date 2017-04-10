package controllers

import models.{SesameRoom, SesameRoomProtocol}
import org.mockito.Matchers
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.util.Success

class RoomCRUDControllerSpec extends AbstractCRUDControllerSpec[SesameRoomProtocol, SesameRoom, SesameRoom] {
  override val entityToPass: SesameRoom = SesameRoom("label to pass", "description to pass")

  override val entityToFail: SesameRoom = SesameRoom("label to fail", "description to fail")

  override implicit val jsonWrites: Writes[SesameRoom] = SesameRoom.writes

  override val atomizedEntityToPass: SesameRoom = entityToPass

  override val atomizedEntityToFail: SesameRoom = entityToFail

  override val jsonWritesAtom: Writes[SesameRoom] = jsonWrites

  override val controller: RoomCRUDController = new RoomCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: SesameRoomProtocol, existing: Option[SesameRoom]): SesameRoom = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override def entityTypeName: String = "room"


  override val mimeType: LwmMimeType = LwmMimeType.roomV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "description" -> entityToPass.description
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> s"${entityToPass.label} updated",
    "description" -> s"${entityToPass.description} updated"
  )

  import bindings.RoomDescriptor
  import ops._

  implicit val roomBinder = RoomDescriptor.binder
  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  "A RoomCRUDControllerSpec also " should {

    s"handle this model issue when creating a new $entityTypeName which already exists" in {
      when(repository.prepareQuery(Matchers.anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map(
        "s" -> List(factory.createLiteral(SesameRoom.generateUri(entityToPass)))
      )))
      when(repository.get[SesameRoom](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))

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
        "s" -> List(factory.createLiteral(SesameRoom.generateUri(entityToPass)))
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
