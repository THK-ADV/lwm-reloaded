package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDControllerSpec
import models._
import models.labwork._
import models.semester.Semester
import models.users.{Student, User}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.util.{Failure, Success, Try}

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

  override val entityToPass: Group = Group("label to pass", labworkToPass.id, studentsToPass.map(_.id), Group.randomUUID)

  override val controller: GroupCRUDController = new GroupCRUDController(repository, sessionService, namespace, roleService, groupService) {

    override protected def fromInput(input: GroupProtocol, existing: Option[Group]): Group = entityToPass

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Group = Group("label to fail", labworkToFail.id, studentsToFail.map(_.id), Group.randomUUID)

  override implicit val jsonWrites: Writes[Group] = Group.writes

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

  val atomizedEntityToPass = GroupAtom(entityToPass.label, labworkToPass, studentsToPass, entityToPass.id)
  val atomizedEntityToFail = GroupAtom(entityToFail.label, labworkToFail, studentsToFail, entityToFail.id)

  override def entityTypeName: String = "group"

  import bindings.GroupDescriptor
  import ops._

  implicit val groupBinder = GroupDescriptor.binder

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  def applicationIds = Stream.continually(LabworkApplication.randomUUID)

  def applications(labwork: UUID) = Stream.continually(LabworkApplication(labwork, User.randomUUID, Set.empty[UUID]))

  implicit val groupReads = Json.reads[Group]


  "A GroupCRUDController also" should {

    "return the corresponding group for a given labwork" in {
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

      val result = controller.asInstanceOf[GroupCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(second))
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

      val result = controller.asInstanceOf[GroupCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(third, fourth))
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

      val result = controller.asInstanceOf[GroupCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(third))
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

      val result = controller.asInstanceOf[GroupCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(fourth))
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

      val result = controller.asInstanceOf[GroupCRUDController].all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Group])
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

      val result = controller.asInstanceOf[GroupCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }

    "not return groups when there is an invalid query parameter value" in {
      val invalidParameter = "invalidParameterValue"

      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)

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

      val result = controller.asInstanceOf[GroupCRUDController].all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> s"Invalid UUID string: $invalidParameter"
      )
    }

    "create groups given some arbitrary group size" in {
      val labwork = Labwork.randomUUID
      val course = Course.randomUUID
      val groupSize = 12
      val applicantsAmount = 100

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toSet

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Success(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Try(Set.empty[PointedGraph[Sesame]]))

      val fakeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/count?value=$groupSize",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val result = controller.createWithCount(course.toString, labwork.toString)(fakeRequest).run

      status(result) shouldBe CREATED

      val resultValue = contentAsJson(result).as[JsArray]
      val deserialised = resultValue.value map (_.as[Group])
      val allmembers = deserialised.map(_.members).reduce(_ ++ _)

      deserialised.size shouldBe groupSize
      deserialised.map(_.labwork).toSet shouldBe Set(labwork)
      allmembers.size shouldBe concreteApplicationIds.size
      concreteApplicationIds forall allmembers.contains
    }

    "create groups given some arbitrary group range" in {
      val course = Course.randomUUID
      val labwork = Labwork.randomUUID
      val min = 12
      val max = 20
      val applicantsAmount = 100
      val calculatedGroupSize = (min to max).reduce { (l, r) =>
        if (l % applicantsAmount < r % applicantsAmount) r else l
      } + 1

      val predictedGroupsNumber = (applicantsAmount / calculatedGroupSize) + 1

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toSet

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Success(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Try(Set.empty[PointedGraph[Sesame]]))

      val fakeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/range?min=$min&max=$max",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val result = controller.createWithRange(course.toString, labwork.toString)(fakeRequest).run
      status(result) shouldBe CREATED

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
      val course = Course.randomUUID
      val groupSize = 12

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Success(Vector.empty[UUID]))

      val fakeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/count?value=$groupSize",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val result = controller.createWithCount(course.toString, labwork.toString)(fakeRequest).run

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> s"Error while creating groups for labwork: Predicate does not hold for Vector()"
      )
    }

    "stop creating groups when models cannot be added to database" in {
      val labwork = Labwork.randomUUID
      val course = Course.randomUUID
      val min = 12
      val max = 20
      val applicantsAmount = 100

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toSet

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Success(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Failure(new Throwable("could not add to graph")))

      val fakeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/range?min=$min&max=$max",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val result = controller.createWithRange(course.toString, labwork.toString)(fakeRequest).run

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> s"Error while creating groups for labwork: could not add to graph"
      )
    }

    "stop creating groups when there are invalid attributes" in {
      val labwork = Labwork.randomUUID
      val course = Course.randomUUID
      val applicantsAmount = 100

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toSet

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Success(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Failure(new Throwable("could not add to graph")))

      val fakeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/range?invalid=1",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val result = controller.createWithRange(course.toString, labwork.toString)(fakeRequest).run

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> "Error while creating groups for labwork: key not found: min"
      )
    }

    "stop creating groups when there are invalid values for attribute" in {
      val labwork = Labwork.randomUUID
      val course = Course.randomUUID
      val applicantsAmount = 100

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toSet

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Success(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Failure(new Throwable("could not add to graph")))

      val fakeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/count?value=invalid",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val result = controller.createWithCount(course.toString, labwork.toString)(fakeRequest).run

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

    "stop creating groups when max is lower than min" in {
      val labwork = Labwork.randomUUID
      val course = Course.randomUUID
      val min = 20
      val max = 15
      val applicantsAmount = 100

      val concreteApplicationIds = applicationIds.take(applicantsAmount).toVector
      val concreteApplications = applications(labwork).take(applicantsAmount).toSet

      when(groupService.sortApplicantsFor(labwork)).thenReturn(Success(concreteApplicationIds))
      when(groupService.alphabeticalOrdering(anyInt())).thenReturn(('A' to 'Z').map(_.toString).toList)
      when(repository.getMany[LabworkApplication](anyObject())(anyObject())).thenReturn(Try(concreteApplications))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Failure(new Throwable("could not add to graph")))

      val fakeRequest = FakeRequest(
        POST,
        s"/${entityTypeName}s/$course/groups/range?min=$min&max=$max",
        FakeHeaders(Seq.empty),
        Json.obj("" -> "")
      )

      val result = controller.createWithRange(course.toString, labwork.toString)(fakeRequest).run

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> s"Error while creating groups for labwork: Predicate does not hold for $max"
      )
    }

    s"successfully get a single $entityTypeName atomized" in {
      import Group.writesAtom

      doReturn(Success(Some(entityToPass))).
        doReturn(Success(Some(atomizedEntityToPass))).
        when(repository).get(anyObject())(anyObject())
      when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(studentsToPass))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
    }

    s"not get a single $entityTypeName atomized when one of the atomized models is not found" in {
      doReturn(Success(Some(entityToPass))).
        doReturn(Success(None)).
        when(repository).get(anyObject())(anyObject())
      when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(studentsToPass))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    s"not get a single $entityTypeName atomized when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      doReturn(Success(Some(entityToPass))).
        doReturn(Failure(new Exception(errorMessage))).
        when(repository).get(anyObject())(anyObject())
      when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(studentsToPass))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"successfully get all ${plural(entityTypeName)} atomized" in {
      import Group.writesAtom

      val groups = Set(entityToPass, entityToFail)
      val labworks = Set(labworkToPass, labworkToFail)

      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      doReturn(Success(Some(atomizedEntityToPass))).
        doReturn(Success(Some(atomizedEntityToFail))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(atomizedEntityToPass, atomizedEntityToFail))
    }

    s"not get all ${plural(entityTypeName)} atomized when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.getAll[Group](anyObject())).thenReturn(Success(Set(entityToPass, entityToFail)))

      doReturn(Failure(new Exception(errorMessage))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
