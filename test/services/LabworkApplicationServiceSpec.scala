package services

import base.TestBaseDefinition
import models._
import models.applications.LabworkApplication
import models.semester.Semester
import models.users.User
import org.joda.time.DateTime
import org.openrdf.model.Value
import org.scalatest.WordSpec
import org.w3.banana.sesame.SesameModule
import store.{Namespace, SesameRepository}
import store.bind.Bindings
import store.sparql.select._

import scala.util.{Failure, Success}

class LabworkApplicationServiceSpec extends WordSpec with TestBaseDefinition with SesameModule {


  val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val repository = SesameRepository(ns)

  val bindings = Bindings(ns)

  val applicationService = new LabworkApplicationService(repository)

  import bindings._
  import bindings.LabworkApplicationBinding._
  import bindings.LabworkBinding._
  import bindings.AssignmentPlanBinding._
  import bindings.jodaDateTimeBinder

  val mandatoryT = EntryType("Mandatory")
  val optionalT = EntryType("Optional")
  val assignmentPlan = AssignmentPlan(2, Set(
    AssignmentEntry(0, Set(mandatoryT, optionalT)),
    AssignmentEntry(1, Set(optionalT))
  ))

  "An application service" should {

    "return applications for a given labwork" in {
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan)
      val applications = List(
        LabworkApplication(labwork.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty)
      )

      repository.add[Labwork](labwork)
      repository.addMany[LabworkApplication](applications)

      val resApplications = applicationService.applicationsFor(labwork.id)

      resApplications match {
        case Success(v) if v.nonEmpty=>
          v.size shouldBe applications.size
          v.forall(applications.contains) shouldBe true
        case Success(v) => fail("LabworkApplications should exist")
        case Failure(e) => fail(s"failed while retrieving data ${e.getMessage}")
      }
    }

    "return only the applications of a specific labwork, even though more are present" in {
      val labwork1 = Labwork("label1", "description1", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan)
      val labwork2 = Labwork("label2", "description2", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan)
      val applicationList1 = List(
        LabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork1.id, User.randomUUID, Set.empty)
      )

      val applicationList2 = List(
        LabworkApplication(labwork2.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork2.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork2.id, User.randomUUID, Set.empty),
        LabworkApplication(labwork2.id, User.randomUUID, Set.empty)
      )
      repository.add[Labwork](labwork1)
      repository.add[Labwork](labwork2)
      repository.addMany[LabworkApplication](applicationList1)
      repository.addMany[LabworkApplication](applicationList2)

      val resApplications = applicationService.applicationsFor(labwork2.id)

      resApplications match {
        case Success(v) if v.nonEmpty =>
          v.size shouldBe applicationList2.size
          v.forall(applicationList2.contains) shouldBe true
          v.forall(applicationList1.contains) shouldBe false
        case Success(v) => fail("LabworkApplicaitons should exist")
        case Failure(e) => fail(s"failed while retrieving data ${e.getMessage}")
      }

    }

    "return None when no applications are found" in {
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan)

      repository.add[Labwork](labwork)

      val resApplications = applicationService.applicationsFor(labwork.id)

      resApplications match {
        case Success(v) if v.nonEmpty => fail("Should not return any LabworkApplication")
        case Success(v) =>
        case Failure(e) => fail(s"failed while retrieving data ${e.getMessage}")
      }
    }

    "return applications for a given labwork ordered by timestamp" in {
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan)
      val applications = List(
        LabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now()),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusDays(1)),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusHours(4)),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusMinutes(50)),
        LabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusMinutes(10))
      )
      implicit val dateTimeOrdering = new Ordering[DateTime] {
        override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
      }

      repository.add[Labwork](labwork)
      repository.addMany[LabworkApplication](applications)

      val resApplications = applicationService.applicationsFor(labwork.id)

      resApplications match {
        case Success(set) if set.nonEmpty =>
          set.size shouldBe applications.size
          set.forall(applications.contains) shouldBe true
          applications.sortBy(_.timestamp).map(_.applicant).toSet shouldBe set.map(_.applicant)
        case Success(set) => fail("LabworkApplicaitons should exist")
        case Failure(e) => fail(s"failed while retrieving data ${e.getMessage}")
      }
    }
  }

}
