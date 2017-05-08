package services

import java.util.UUID

import base.TestBaseDefinition
import models._
import org.joda.time.DateTime
import org.scalatest.WordSpec
import org.w3.banana.sesame.SesameModule
import store.{Namespace, SesameRepository}
import store.bind.Bindings

import scala.util.{Failure, Success}

class LabworkApplicationServiceSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val repository = SesameRepository(ns)

  val bindings = Bindings(ns)

  val applicationService = LabworkApplicationService(repository)

  import bindings.{
  LabworkApplicationDescriptor,
  LabworkDescriptor,
  AssignmentPlanDescriptor,
  dateTimeBinder
  }

  val emptyPlan = SesameAssignmentPlan.empty

  "An application service" should {

    "return applications for a given labwork" in {
      val labwork = SesameLabwork("label", "description", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)
      val applications = List(
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty)
      )

      repository.add[SesameLabwork](labwork)
      repository.addMany[SesameLabworkApplication](applications)

      val resApplications = applicationService.applicationsFor(labwork.id)

      resApplications match {
        case Success(v) if v.nonEmpty =>
          v.size shouldBe applications.size
          v.forall(applications.contains) shouldBe true
        case Success(v) => fail("LabworkApplications should exist")
        case Failure(e) => fail(s"failed while retrieving data ${e.getMessage}")
      }
    }

    "return only the applications of a specific labwork, even though more are present" in {
      val labwork1 = SesameLabwork("label1", "description1", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)
      val labwork2 = SesameLabwork("label2", "description2", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)
      val applicationList1 = List(
        SesameLabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork1.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork1.id, User.randomUUID, Set.empty)
      )

      val applicationList2 = List(
        SesameLabworkApplication(labwork2.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork2.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork2.id, User.randomUUID, Set.empty),
        SesameLabworkApplication(labwork2.id, User.randomUUID, Set.empty)
      )
      repository.add[SesameLabwork](labwork1)
      repository.add[SesameLabwork](labwork2)
      repository.addMany[SesameLabworkApplication](applicationList1)
      repository.addMany[SesameLabworkApplication](applicationList2)

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
      val labwork = SesameLabwork("label", "description", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)

      repository.add[SesameLabwork](labwork)

      val resApplications = applicationService.applicationsFor(labwork.id)

      resApplications match {
        case Success(v) if v.nonEmpty => fail("Should not return any LabworkApplication")
        case Success(v) =>
        case Failure(e) => fail(s"failed while retrieving data ${e.getMessage}")
      }
    }

    "return applications for a given labwork ordered by timestamp" in {
      val labwork = SesameLabwork("label", "description", SesameSemester.randomUUID, SesameCourse.randomUUID, UUID.randomUUID)
      val applications = List(
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now()),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusDays(1)),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusHours(4)),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusMinutes(50)),
        SesameLabworkApplication(labwork.id, User.randomUUID, Set.empty, DateTime.now().plusMinutes(10))
      )
      implicit val dateTimeOrdering = new Ordering[DateTime] {
        override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
      }

      repository.add[SesameLabwork](labwork)
      repository.addMany[SesameLabworkApplication](applications)

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
