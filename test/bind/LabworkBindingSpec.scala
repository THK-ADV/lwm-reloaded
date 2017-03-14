package bind

import base.SesameDbSpec
import models._
import org.joda.time.LocalDate
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class LabworkBindingSpec extends SesameDbSpec {

  import bindings.{LabworkDescriptor, dateTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val labworkBinder = LabworkDescriptor.binder
  val labwork = SesameLabwork("AP Praktikum", "AP Praktikum", SesameSemester.randomUUID, SesameCourse.randomUUID, PostgresDegree.randomUUID)
  val labworkGraph = URI(SesameLabwork.generateUri(labwork)).a(lwm.Labwork)
    .--(lwm.label).->-(labwork.label)
    .--(lwm.description).->-(labwork.description)
    .--(lwm.semester).->-(labwork.semester)(ops, uuidRefBinder(SesameSemester.splitter))
    .--(lwm.course).->-(labwork.course)(ops, uuidRefBinder(SesameCourse.splitter))
    .--(lwm.degree).->-(labwork.degree)(ops, uuidRefBinder(PostgresDegree.splitter))
    .--(lwm.subscribable).->-(labwork.subscribable)
    .--(lwm.published).->-(labwork.published)
    .--(lwm.invalidated).->-(labwork.invalidated)
    .--(lwm.id).->-(labwork.id).graph

  "A LabworkBinding" should {
    "return a RDF graph representation of a labwork" in {
      val graph = labwork.toPG.graph

      graph isIsomorphicWith labworkGraph shouldBe true
    }

    "return a labwork based on a RDF representation" in {
      val expectedLabwork = PointedGraph[Rdf](URI(SesameLabwork.generateUri(labwork)), labworkGraph).as[SesameLabwork]

      expectedLabwork match {
        case Success(s) =>
          s shouldEqual labwork
        case Failure(e) =>
          fail(s"Unable to deserialise labwork graph: $e")
      }
    }

    "return a labwork atom based on an RDF representation" in {
      import bindings.{CourseDescriptor, DegreeDescriptor, EmployeeDescriptor, LabworkAtomDescriptor, LabworkDescriptor, SemesterDescriptor}

      val semester = SesameSemester("semester", "abr", LocalDate.now, LocalDate.now, LocalDate.now)
      val employee = SesameEmployee("systemid", "lastname", "firstname", "email", "status")
      val course = SesameCourse("course", "description", "abbr", employee.id, 1)
      val degree = PostgresDegree("degree", "abbr")
      val labwork = SesameLabwork("labwork", "description", semester.id, course.id, degree.id, subscribable = false, published = false)

      val courseAtom = SesameCourseAtom("course", "description", "abbr", employee, 1, course.invalidated, course.id)
      val labworkAtom = SesameLabworkAtom("labwork", "description", semester, courseAtom, degree, labwork.subscribable, labwork.published, labwork.invalidated, labwork.id)

      repo add semester
      repo add employee
      repo add course
      repo add degree
      repo add labwork

      repo.get[SesameLabworkAtom](SesameLabwork.generateUri(labwork)) match {
        case Success(Some(atom)) =>
          atom shouldEqual labworkAtom
        case Success(None) =>
          fail("There should exist one labwork")
        case Failure(e) =>
          fail(s"LabworkAtom could not be deserialised: $e")
      }
    }
  }
}
