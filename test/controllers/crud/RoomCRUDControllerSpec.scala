package controllers.crud

import models.{Room, RoomProtocol}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.mockito.Matchers
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType
import play.api.test.Helpers._
import scala.util.Success

class RoomCRUDControllerSpec extends AbstractCRUDControllerSpec[RoomProtocol, Room, Room] {
  override val entityToPass: Room = Room("label to pass", "description to pass")

  override val entityToFail: Room = Room("label to fail", "description to fail")

  override implicit val jsonWrites: Writes[Room] = Room.writes

  override val atomizedEntityToPass: Room = entityToPass

  override val atomizedEntityToFail: Room = entityToFail

  override val jsonWritesAtom: Writes[Room] = jsonWrites

  override val controller: RoomCRUDController = new RoomCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: RoomProtocol, existing: Option[Room]): Room = entityToPass

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
        "s" -> List(factory.createLiteral(Room.generateUri(entityToPass)))
      )))
      when(repository.get[Room](anyObject())(anyObject())).thenReturn(Success(Some(entityToPass)))

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
        "s" -> List(factory.createLiteral(Room.generateUri(entityToPass)))
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
