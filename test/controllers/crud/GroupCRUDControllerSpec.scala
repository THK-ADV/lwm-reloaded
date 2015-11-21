package controllers.crud

import java.util.UUID

import models.applications.LabworkApplication
import models.users.{User, Student}
import models._
import org.mockito.Matchers._
import org.mockito.Mockito.when
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType
import play.api.test.Helpers._
import scala.util.{Failure, Try}

class GroupCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupProtocol, Group] {
  override val entityToPass: Group = Group("label to pass", Labwork.randomUUID, Set(Student.randomUUID), Group.randomUUID)
  override val controller: GroupCRUDController = new GroupCRUDController(repository, namespace, roleService, groupService) {

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def fromInput(input: GroupProtocol, id: Option[UUID]): Group = entityToPass
  }
  override val entityToFail: Group = Group("label to fail", Labwork.randomUUID, Set(Student.randomUUID), Group.randomUUID)
  override implicit val jsonWrites: Writes[Group] = Group.writes
  override val mimeType: LwmMimeType = LwmMimeType.groupV1Json
  override val inputJson: JsValue = Json.obj(
    "label" -> "label input",
    "labwork" -> entityToPass.labwork,
    "members" -> entityToPass.members
  )

  override def entityTypeName: String = "group"

  import bindings.GroupBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  def applicationIds = Stream.continually(LabworkApplication.randomUUID)
  def applications(labwork: UUID) = Stream.continually(LabworkApplication(labwork, User.randomUUID, Set.empty[UUID]))

  implicit val groupReads = Json.reads[Group]


  "A GroupCRUDController also" should {

    "create groups given some arbitrary group size" in {
      val labwork = Labwork.randomUUID
      val groupSize = 12
      val applicantsAmount = 100

      implicit val groupProtWrites = Json.writes[GroupCountProtocol]

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toVector

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Some(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Try(Vector.empty[PointedGraph[Sesame]]))

      val json = Json.toJson(GroupCountProtocol(labwork, groupSize))

      val fakeRequest = FakeRequest(
        POST,
        s"/labworks/$labwork/groups/count",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )

      val result = controller.createWithCount(labwork.toString)(fakeRequest)

      status(result) shouldBe OK

      val resultValue = contentAsJson(result).as[JsArray]
      val deserialised = resultValue.value map (_.as[Group])
      val allmembers = deserialised.map(_.members).reduce(_ ++ _)

      deserialised.size shouldBe groupSize
      deserialised.map(_.labwork).toSet shouldBe Set(labwork)
      allmembers.size shouldBe concreteApplicationIds.size
      concreteApplicationIds forall allmembers.contains
    }

    "create groups given some arbitrary group range" in {
      val labwork = Labwork.randomUUID
      val min = 12
      val max = 20
      val applicantsAmount = 100
      val calculatedGroupSize = (min to max).reduce { (l, r) =>
        if(l % applicantsAmount < r % applicantsAmount) r
        else l
      } + 1

      val predictedGroupsNumber = (applicantsAmount / calculatedGroupSize) + 1

      implicit val groupProtWrites = Json.writes[GroupRangeProtocol]

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toVector

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Some(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Try(Vector.empty[PointedGraph[Sesame]]))

      val json = Json.toJson(GroupRangeProtocol(labwork, min, max))

      val fakeRequest = FakeRequest(
        POST,
        s"/labworks/$labwork/groups/range",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )

      val result = controller.createWithRange(labwork.toString)(fakeRequest)
      status(result) shouldBe OK

      val resultValue = contentAsJson(result).as[JsArray]
      val deserialised = resultValue.value map (_.as[Group])
      val allmembers = deserialised.map(_.members).reduce(_ ++ _)

      deserialised.size shouldBe predictedGroupsNumber
      deserialised.map(_.labwork).toSet shouldBe Set(labwork)
      allmembers.size shouldBe concreteApplicationIds.size
      concreteApplicationIds forall allmembers.contains
    }

    "stop creating groups when no applications have been found" in {
      val labwork = Labwork.randomUUID
      val groupSize = 12

      val expectedResult = Json.obj(
        "status" -> "KO",
        "errors" -> s"Error while creating groups for labwork $labwork"
      )

      when(groupService.sortApplicantsFor(labwork)).thenReturn(None)

      implicit val groupProtWrites = Json.writes[GroupCountProtocol]
      val json = Json.toJson(GroupCountProtocol(labwork, groupSize))

      val fakeRequest = FakeRequest(
        POST,
        s"/labworks/$labwork/groups/count",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )

      val result = controller.createWithCount(labwork.toString)(fakeRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldBe expectedResult
    }

    "stop creating groups when models cannot be added to database" in {
      val labwork = Labwork.randomUUID
      val min = 12
      val max = 20
      val applicantsAmount = 100

      implicit val groupProtWrites = Json.writes[GroupRangeProtocol]

      val expectedResult = Json.obj(
        "status" -> "KO",
        "errors" -> s"Error while creating groups for labwork $labwork"
      )

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toVector

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Some(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Failure(new Throwable("could not add to graph")))


      val json = Json.toJson(GroupRangeProtocol(labwork, min, max))

      val fakeRequest = FakeRequest(
        POST,
        s"/labworks/$labwork/groups/range",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        json
      )

      val result = controller.createWithRange(labwork.toString)(fakeRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldBe expectedResult
    }
  }
}
