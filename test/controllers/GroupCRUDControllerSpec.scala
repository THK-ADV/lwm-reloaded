package controllers

import java.util.UUID

import controllers.GroupCRUDController._
import models._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{Count, Range}
import utils.LwmMimeType

import scala.util.Success

class GroupCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupProtocol, Group, GroupAtom] {

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  val studentsToPass = Set(
    Student("systemId1 to pass", "last name 1 to pass", "first name 1 to pass", "email1 to pass", "regId1 to pass", UUID.randomUUID()),
    Student("systemId2 to pass", "last name 2 to pass", "first name 2 to pass", "email2 to pass", "regId2 to pass", UUID.randomUUID())
  )
  val studentsToFail = Set(
    Student("systemId1 to fail", "last name 1 to fail", "first name 1 to fail", "email1 to fail", "regId1 to fail", UUID.randomUUID()),
    Student("systemId2 to fail", "last name 2 to fail", "first name 2 to fail", "email2 to fail", "regId2 to fail", UUID.randomUUID())
  )

  override val entityToPass: Group = Group("label to pass", labworkToPass.id, studentsToPass.map(_.id))

  override val controller: GroupCRUDController = new GroupCRUDController(repository, sessionService, namespace, roleService, groupService) {

    override protected def fromInput(input: GroupProtocol, existing: Option[Group]): Group = entityToPass

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Group = Group("label to fail", labworkToFail.id, studentsToFail.map(_.id))

  override implicit val jsonWrites: Writes[Group] = Group.writes

  override implicit def jsonWritesAtom: Writes[GroupAtom] = Group.writesAtom

  override val mimeType: LwmMimeType = LwmMimeType.groupV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "labwork" -> entityToPass.labwork,
    "members" -> entityToPass.members
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> s"${entityToPass.label} updated",
    "labwork" -> entityToPass.labwork,
    "members" -> (entityToPass.members + User.randomUUID)
  )

  override val atomizedEntityToPass = GroupAtom(entityToPass.label, labworkToPass, studentsToPass, entityToPass.invalidated, entityToPass.id)
  override val atomizedEntityToFail = GroupAtom(entityToFail.label, labworkToFail, studentsToFail, entityToPass.invalidated, entityToFail.id)

  override def entityTypeName: String = "group"

  import bindings.GroupDescriptor
  import ops._

  implicit val groupBinder = GroupDescriptor.binder

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  def groups(labwork: UUID, amount: Int) = {
    def students = (0 until 20).map(_ => UUID.randomUUID).toVector

    (0 until amount).map(_ => Group("label", labwork, students.take(15).toSet)).toSet
  }

  implicit val groupReads = Json.reads[Group]


  "A GroupCRUDController also" should {

    /*"return the corresponding group for a given labwork" in {
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val first = Group("first", Labwork.randomUUID, Set.empty[UUID])
      val second = Group("second", labwork.id, Set.empty[UUID])
      val third = Group("third", Labwork.randomUUID, Set.empty[UUID])
      val fourth = Group("fourth", Labwork.randomUUID, Set.empty[UUID])

      val groups = Set(first, second, third, fourth)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${GroupCRUDController.labworkAttribute}=${labwork.id.toString}"
      )

      val result = controller.all()(request)
      val expected = Set(Json.toJson(second))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe expected
    }

    "return all corresponding groups for a given labwork" in {
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val first = Group("first", Labwork.randomUUID, Set.empty[UUID])
      val second = Group("second", Labwork.randomUUID, Set.empty[UUID])
      val third = Group("third", labwork.id, Set.empty[UUID])
      val fourth = Group("fourth", labwork.id, Set.empty[UUID])

      val groups = Set(first, second, third, fourth)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${GroupCRUDController.labworkAttribute}=${labwork.id.toString}"
      )

      val result = controller.all()(request)
      val expected = Set(Json.toJson(third), Json.toJson(fourth))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentAsString(result) shouldBe expected.mkString("")
    }

    "return all corresponding groups for a given student" in {
      val student = User.randomUUID

      val first = Group("first", Labwork.randomUUID, Set(User.randomUUID, User.randomUUID))
      val second = Group("second", Labwork.randomUUID, Set(User.randomUUID))
      val third = Group("third", Labwork.randomUUID, Set(student))
      val fourth = Group("fourth", Labwork.randomUUID, Set(User.randomUUID))

      val groups = Set(first, second, third, fourth)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${GroupCRUDController.studentAttribute}=$student"
      )

      val result = controller.all()(request)
      val expected = Set(Json.toJson(third))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentAsString(result) shouldBe expected.mkString("")
    }

    "return all corresponding groups for a given group label" in {
      val label = "fourth"

      val first = Group("first", Labwork.randomUUID, Set(User.randomUUID, User.randomUUID))
      val second = Group("second", Labwork.randomUUID, Set(User.randomUUID))
      val third = Group("third", Labwork.randomUUID, Set(User.randomUUID))
      val fourth = Group("fourth", Labwork.randomUUID, Set(User.randomUUID))

      val groups = Set(first, second, third, fourth)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${GroupCRUDController.labelAttribute}=$label"
      )

      val result = controller.all()(request)
      val expected = Set(Json.toJson(fourth))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentAsString(result) shouldBe expected.mkString("")
    }

    "not return groups for a labwork when there is no match" in {
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val first = Group("first", Labwork.randomUUID, Set.empty[UUID])
      val second = Group("second", Labwork.randomUUID, Set.empty[UUID])
      val third = Group("third", Labwork.randomUUID, Set.empty[UUID])
      val fourth = Group("fourth", Labwork.randomUUID, Set.empty[UUID])

      val groups = Set(first, second, third, fourth)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${GroupCRUDController.labworkAttribute}=${labwork.id.toString}"
      )

      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentAsString(result) shouldBe ""
    }

    "not return groups when there is an invalid query attribute" in {
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

      val first = Group("first", Labwork.randomUUID, Set.empty[UUID])
      val second = Group("second", Labwork.randomUUID, Set.empty[UUID])
      val third = Group("third", Labwork.randomUUID, Set.empty[UUID])
      val fourth = Group("fourth", Labwork.randomUUID, Set.empty[UUID])

      val groups = Set(first, second, third, fourth)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalidAttribute=${labwork.id.toString}"
      )

      val result = controller.all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "not return groups when there is an invalid query parameter value" in {
      val invalidParameter = "invalidParameterValue"

      val first = Group("first", Labwork.randomUUID, Set.empty[UUID])
      val second = Group("second", Labwork.randomUUID, Set.empty[UUID])
      val third = Group("third", Labwork.randomUUID, Set.empty[UUID])
      val fourth = Group("fourth", Labwork.randomUUID, Set.empty[UUID])

      val groups = Set(first, second, third, fourth)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}invalidParameter?${GroupCRUDController.labworkAttribute}=$invalidParameter"
      )

      val result = controller.all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> s"Invalid UUID string: $invalidParameter"
      )
    }*/

    "infer strategy from query string" in {
      val countValue = "8"
      val minValue = "15"
      val maxValue = "20"

      val count = Map(countAttribute -> Seq(countValue))
      val range = Map(minAttribute -> Seq(minValue), maxAttribute -> Seq(maxValue))
      val broken = Map("wrongAttribute" -> Seq("broken"))

      strategyFrom(count) match {
        case Some(c: Count) => c.value shouldBe countValue
        case Some(_) => fail("strategy type should be count but was strategy")
        case None => fail("there should be at least one attribute")
      }

      strategyFrom(range) match {
        case Some(r: Range) =>
          r.min shouldBe minValue
          r.max shouldBe maxValue
        case Some(_) => fail("strategy type should be range but was count")
        case None => fail("there should be at least one attribute")
      }

      strategyFrom(broken) should not be defined
    }

    "preview groups given some arbitrary group size and range" in {
      import models.Group.protocolWrites

      val labwork = Labwork.randomUUID
      val course = Course.randomUUID
      val groupSize = 8
      val min = 13
      val max = 15
      val gs = groups(labwork, groupSize)

      when(groupService.groupBy(anyObject(), anyObject())).thenReturn(Success(gs))

      val countRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/preview?$countAttribute=$groupSize",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val rangeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/preview?$minAttribute=$min&$maxAttribute=$max",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val countResult = controller.preview(course.toString, labwork.toString)(countRequest).run
      val rangeResult = controller.preview(course.toString, labwork.toString)(rangeRequest).run

      status(countResult) shouldBe OK
      contentAsJson(countResult) shouldBe Json.toJson(gs.map(g => GroupProtocol(g.label, g.labwork, g.members)))

      status(rangeResult) shouldBe OK
      contentAsJson(rangeResult) shouldBe Json.toJson(gs.map(g => GroupProtocol(g.label, g.labwork, g.members)))
    }
  }
}
