package controllers.crud.schedule

import java.util.UUID
import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models._
import models.schedule._
import models.semester.{Blacklist, Semester}
import models.users.{User, Employee}
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services._
import utils.{Evaluation, Gen, LwmMimeType}

import scala.util.{Failure, Success}

class ScheduleCRUDControllerSpec extends AbstractCRUDControllerSpec[ScheduleProtocol, Schedule] {

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  val roomToPass = Room("room to pass", "desc to pass")
  val roomToFail = Room("room to fail", "desc to fail")

  val supervisorToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass")
  val supervisorToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "stauts to fail")

  val groupToPass = Group("group to pass", labworkToPass.id, Set(UUID.randomUUID(), UUID.randomUUID()))
  val groupToFail = Group("group to fail", labworkToFail.id, Set(UUID.randomUUID(), UUID.randomUUID()))

  val entriesToPass = (0 until 10).map (n =>
    ScheduleEntry(
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n),
      LocalDate.now.plusWeeks(n),
      roomToPass.id,
      supervisorToPass.id,
      groupToPass.id
    )
  ).toSet
  val entriesToFail = (0 until 10).map (n =>
    ScheduleEntry(
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n),
      LocalDate.now.plusWeeks(n),
      roomToFail.id,
      supervisorToFail.id,
      groupToFail.id
    )
  ).toSet

  override val entityToFail: Schedule = Schedule(labworkToFail.id, entriesToFail)

  override val entityToPass: Schedule = Schedule(labworkToPass.id, entriesToPass)

  import ops._
  import bindings.ScheduleBinding.scheduleBinder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override def entityTypeName: String = "schedule"

  val scheduleService = org.scalatest.mock.MockitoSugar.mock[ScheduleService]
  val reportCardService = org.scalatest.mock.MockitoSugar.mock[ReportCardService]

  override val controller: AbstractCRUDController[ScheduleProtocol, Schedule] = new ScheduleCRUDController(repository, namespace, roleService, scheduleService, reportCardService) {

    override protected def fromInput(input: ScheduleProtocol, existing: Option[Schedule]): Schedule = entityToPass

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override implicit val jsonWrites: Writes[Schedule] = Schedule.writes

  override val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries,
    "published" -> entityToPass.published
  )

  override val updateJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "entries" -> (
      entityToPass.entries +
        ScheduleEntry(LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      ),
    "published" -> true
  )

  private def toScheduleEntryAtom(entries: Set[ScheduleEntry])(room: Room, supervisor: Employee, group: Group): Set[ScheduleEntryAtom] = {
    entries.map(e => ScheduleEntryAtom(e.start, e.end, e.date, room, supervisor, group))
  }

  val atomizedEntityToPass = ScheduleAtom(
    labworkToPass,
    toScheduleEntryAtom(entriesToPass)(roomToPass, supervisorToPass, groupToPass),
    entityToPass.published,
    entityToPass.id
  )

  val atomizedEntityToFail = ScheduleAtom(
    labworkToFail,
    toScheduleEntryAtom(entriesToFail)(roomToFail, supervisorToFail, groupToFail),
    entityToPass.published,
    entityToFail.id
  )

  val emptyVector = Vector.empty[ScheduleEntryG]

  "A ScheduleCRUDController also" should {

    "return empty list of scheduleG's when there are no competitive schedules" in {
      val semester = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val course = Course("label", "desc", "abbreviation", User.randomUUID, 1, Course.randomUUID)
      val labwork = Labwork("label", "description", semester.id, course.id, Degree.randomUUID)

      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set.empty[Schedule]))

      val result = ScheduleCRUDController.competitive(labwork.id, repository)

      result match {
        case Success(s) => s shouldBe empty
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

    "return scheduleG's when there are competitive schedules" in {
      val course1 = Course("label", "desc", "abbreviation", User.randomUUID, 1, Course.randomUUID)
      val course2 = Course("label", "desc", "abbreviation", User.randomUUID, 1, Course.randomUUID)
      val course3 = Course("label", "desc", "abbreviation", User.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val labwork1 = Labwork("label", "description", semester1.id, course1.id, Degree.randomUUID)
      val labwork2 = Labwork("label", "description", semester1.id, course2.id, Degree.randomUUID)
      val labwork3 = Labwork("label", "description", semester1.id, course3.id, Degree.randomUUID)

      val groups = (0 until 3).map(n => Group(n.toString, Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet

      val first = Schedule(labwork1.id, Set.empty[ScheduleEntry])
      val second = Schedule(labwork2.id, Set.empty[ScheduleEntry])
      val third = Schedule(labwork3.id, Set.empty[ScheduleEntry])

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set(first, second)))
      when(repository.get[Group](anyObject(), anyObject())).thenReturn(Success(groups))

      val result = ScheduleCRUDController.competitive(labwork3.id, repository)

      result match {
        case Success(s) =>
          s should not be empty
          s.size shouldBe 2

          val schedules = s.map(sg => Schedule(sg.labwork, Set.empty[ScheduleEntry], published = false, sg.id))
          schedules.contains(first) shouldBe true
          schedules.contains(second) shouldBe true
          schedules.contains(third) shouldBe false
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

    "preview a schedule successfully when there are no competitive schedules" in {
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
        Schedule(gen.elem.labwork, entries, published = false, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).doReturn(Success(Set(plan))).when(repository).get(anyObject(), anyObject())
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].previewFrom(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully although there are competitive schedules" in {
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
        Schedule(gen.elem.labwork, entries, published = false, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).doReturn(Success(Set(plan))).doReturn(Success(Set.empty[Group])).when(repository).get(anyObject(), anyObject())
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set.empty[Schedule]))
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].previewFrom(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully where conflicts are found" in {
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List(
          Conflict(
            ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), UUID.randomUUID(), groups.head),
            groups.head.members.toVector.take(1),
            groups.head
          )
        ), 1)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
        Schedule(gen.elem.labwork, entries, published = false, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).doReturn(Success(Set(plan))).doReturn(Success(Set.empty[Group])).when(repository).get(anyObject(), anyObject())
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set.empty[Schedule]))
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].previewFrom(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "not preview a schedule when assignment plan is empty" in {
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).doReturn(Success(Set(AssignmentPlan.empty))).when(repository).get(anyObject(), anyObject())
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].previewFrom(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when timetable is empty" in {
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set.empty[TimetableEntry], LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).doReturn(Success(Set(plan))).when(repository).get(anyObject(), anyObject())
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].previewFrom(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when groups are empty" in {
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(Set.empty[Group])).doReturn(Success(Set(timetable))).doReturn(Success(Set(plan))).when(repository).get(anyObject(), anyObject())
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].previewFrom(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when db errors occur" in {
      val course = Course.randomUUID
      val labwork = Labwork.randomUUID

      val request = FakeRequest(
        GET,
        s"/labworks/$labwork/${entityTypeName}s/preview"
      )

      val exception = new Exception("Oops, something went wrong")
      doReturn(Failure(exception)).doReturn(Failure(exception)).doReturn(Success(Set(AssignmentPlan.empty))).when(repository).get(anyObject(), anyObject())
      when(repository.prepareQuery(anyObject())).thenReturn(query)

      val result = controller.asInstanceOf[ScheduleCRUDController].previewFrom(course.toString, labwork.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> exception.getMessage
      )
    }

    s"successfully get a single $entityTypeName atomized" in {
      import Schedule.atomicWrites

      doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(labworkToPass))).
      when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set(roomToPass))).
      doReturn(Success(Set(supervisorToPass))).
      doReturn(Success(Set(groupToPass))).
      when(repository).getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
    }

    s"not get a single $entityTypeName atomized when one of the atomic models is not found" in {
      doReturn(Success(Some(entityToPass))).
      doReturn(Success(None)).
      when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set(roomToPass))).
      doReturn(Success(Set(supervisorToPass))).
      doReturn(Success(Set(groupToPass))).
      when(repository).getMany(anyObject())(anyObject())

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
      doReturn(Success(Some(labworkToPass))).
      when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set(roomToPass))).
      doReturn(Failure(new Exception(errorMessage))).
      doReturn(Success(Set(groupToPass))).
      when(repository).getMany(anyObject())(anyObject())

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

    s"successfully get all ${fgrammar(entityTypeName)} atomized" in {
      import Schedule._

      val schedules = Set(entityToPass, entityToFail)

      when(repository.get[Schedule](anyObject(), anyObject())).thenReturn(Success(schedules))

      doReturn(Success(Some(labworkToPass))).
        doReturn(Success(Some(labworkToFail))).
        when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set(roomToPass))).
        doReturn(Success(Set(supervisorToPass))).
        doReturn(Success(Set(groupToPass))).
        doReturn(Success(Set(roomToFail))).
        doReturn(Success(Set(supervisorToFail))).
        doReturn(Success(Set(groupToFail))).
        when(repository).getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(atomizedEntityToPass, atomizedEntityToFail))
    }

    s"not get all ${fgrammar(entityTypeName)} atomized when there is an exception" in {
      val schedules = Set(entityToPass, entityToFail)
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.get[Schedule](anyObject(), anyObject())).thenReturn(Success(schedules))
      doReturn(Failure(new Exception(errorMessage))).
      when(repository).getMany(anyObject())(anyObject())

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

    "successfully publish a schedule and create dedicated report cards" in {
      val courseId = Course.randomUUID
      val scheduleId = Schedule.randomUUID

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(
        Map("plan" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      ))
      doReturn(Success(Some(AssignmentPlan.empty))).doReturn(Success(Some(entityToPass))).doReturn(Success(Some(groupToPass))).when(repository).get(anyObject())(anyObject())
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(pointedGraph))
      when(reportCardService.reportCards(anyObject(), anyObject())).thenReturn(Success(Set.empty[ReportCard]))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Success(Set(pointedGraph)))

      val request = FakeRequest(
        POST,
        s"/courses/$courseId/${entityTypeName}s/$scheduleId/publish"
      )
      val result = controller.asInstanceOf[ScheduleCRUDController].publish(courseId.toString, scheduleId.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "message" -> "Published"
      )
    }

    "fail publishing a schedule when there are no students for report cards" in {
      val courseId = Course.randomUUID
      val scheduleId = Schedule.randomUUID

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(
        Map("plan" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      ))
      doReturn(Success(Some(AssignmentPlan.empty))).doReturn(Success(Some(entityToPass))).doReturn(Success(Some(groupToPass))).when(repository).get(anyObject())(anyObject())
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(pointedGraph))
      when(reportCardService.reportCards(anyObject(), anyObject())).thenReturn(Success(Set.empty[ReportCard]))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Success(Set.empty[PointedGraph[Sesame]]))

      val request = FakeRequest(
        POST,
        s"/courses/$courseId/${entityTypeName}s/$scheduleId/publish"
      )
      val result = controller.asInstanceOf[ScheduleCRUDController].publish(courseId.toString, scheduleId.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Error while creating report cards"
      )
    }

    "fail publishing a schedule when there is a exception" in {
      val courseId = Course.randomUUID
      val scheduleId = Schedule.randomUUID
      val errorMessage = "Oops, something went wrong"

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(
        Map("plan" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      ))
      doReturn(Success(Some(AssignmentPlan.empty))).doReturn(Success(Some(entityToPass))).doReturn(Success(Some(groupToPass))).when(repository).get(anyObject())(anyObject())
      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Failure(new Throwable(errorMessage)))
      when(reportCardService.reportCards(anyObject(), anyObject())).thenReturn(Success(Set.empty[ReportCard]))
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Success(Set.empty[PointedGraph[Sesame]]))

      val request = FakeRequest(
        POST,
        s"/courses/$courseId/${entityTypeName}s/$scheduleId/publish"
      )
      val result = controller.asInstanceOf[ScheduleCRUDController].publish(courseId.toString, scheduleId.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
