package bind

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.LocalDate
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class LabworkApplicationBindingSpec extends SesameDbSpec {

  import bindings.{LabworkApplicationDescriptor, dateTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val labworkApplicationBinder = LabworkApplicationDescriptor.binder

  val student = User.randomUUID
  val friend1 = User.randomUUID
  val friend2 = User.randomUUID
  val friend3 = User.randomUUID
  val application = LabworkApplication(Labwork.randomUUID, student, Set(friend1, friend2))
  val applicationGraph = URI(LabworkApplication.generateUri(application)).a(lwm.LabworkApplication)
    .--(lwm.labwork).->-(application.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.applicant).->-(application.applicant)(ops, uuidRefBinder(User.splitter))
    .--(lwm.timestamp).->-(application.timestamp)
    .--(lwm.friends).->-(application.friends)(ops, uuidRefBinder(User.splitter))
    .--(lwm.invalidated).->-(application.invalidated)
    .--(lwm.id).->-(application.id).graph

  "A LabworkApplicationBinding" should {
    "return a RDF graph representation of a labwork application" in {
      val graph = application.toPG.graph

      graph isIsomorphicWith applicationGraph shouldBe true
    }

    "return an labwork application based on an RDF representation" in {
      val expectedLabworkApplication = PointedGraph[Rdf](URI(LabworkApplication.generateUri(application)), applicationGraph).as[LabworkApplication]

      expectedLabworkApplication match {
        case Success(s) =>
          s.applicant shouldEqual application.applicant
          s.id shouldEqual application.id
          s.friends shouldEqual application.friends
          s.labwork shouldEqual application.labwork
          s.timestamp.isEqual(application.timestamp) shouldBe true
        case Failure(e) =>
          fail(s"Unable to deserialise labwork application graph: $e")
      }
    }

    "return an atomic labwork application based on an RDF representation" in {
      import bindings.{LabworkApplicationAtomDescriptor, LabworkAtomDescriptor, StudentDescriptor}

      val employee = SesameEmployee("systemId", "last", "first", "mail", "status")
      val courseAtom = SesameCourseAtom("label", "desc", "abbrev", employee, 1, None, UUID.randomUUID)
      val degree = PostgresDegree("label", "abbrev")
      val semester = Semester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      val labwork = LabworkAtom("Label", "Description", semester, courseAtom, degree, false, false, None, UUID.randomUUID)
      val student1 = SesameStudent("systemid1", "lastname1", "firstname1", "email1", "registrationId1", UUID.randomUUID())
      val student2 = SesameStudent("systemid2", "lastname2", "firstname2", "email2", "registrationId2", UUID.randomUUID())
      val application = LabworkApplication(labwork.id, student1.id, Set(student2.id))

      val applicationAtom = LabworkApplicationAtom(labwork, student1, Set(student2), application.timestamp, application.invalidated, application.id)

      repo.add[LabworkAtom](labwork)
      repo.add[SesameStudent](student1)
      repo.add[SesameStudent](student2)
      repo.add[LabworkApplication](application)

      repo.get[LabworkApplicationAtom](LabworkApplication.generateUri(application.id)) match {
        case Success(Some(dapp)) =>
          dapp.labwork shouldEqual applicationAtom.labwork
          dapp.applicant shouldEqual applicationAtom.applicant
          dapp.friends shouldEqual applicationAtom.friends
          dapp.id shouldEqual applicationAtom.id
          dapp.timestamp isEqual applicationAtom.timestamp shouldBe true
        case Success(None) =>
          fail("No LabworkApplications have been found")
        case Failure(e) =>
          fail(s"LabworkApplication could not be deserialised: $e")
      }
    }

  }
}
