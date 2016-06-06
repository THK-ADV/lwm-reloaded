package bind.labwork

import base.SesameDbSpec
import models.labwork.{Labwork, LabworkAtom}
import models.semester.Semester
import models.users.Employee
import models.{Course, CourseAtom, Degree}
import org.joda.time.LocalDate
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class LabworkBindingSpec extends SesameDbSpec {

  import ops._

  val bindings = Bindings[Sesame](namespace)

  import bindings.{LabworkDescriptor, uuidBinder, uuidRefBinder}

  implicit val labworkBinder = LabworkDescriptor.binder
  val labwork = Labwork("AP Praktikum", "AP Praktikum", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
  val labworkGraph = URI(Labwork.generateUri(labwork)).a(lwm.Labwork)
    .--(lwm.label).->-(labwork.label)
    .--(lwm.description).->-(labwork.description)
    .--(lwm.semester).->-(labwork.semester)(ops, uuidRefBinder(Semester.splitter))
    .--(lwm.course).->-(labwork.course)(ops, uuidRefBinder(Course.splitter))
    .--(lwm.degree).->-(labwork.degree)(ops, uuidRefBinder(Degree.splitter))
    .--(lwm.subscribable).->-(labwork.subscribable)
    .--(lwm.published).->-(labwork.published)
    .--(lwm.id).->-(labwork.id).graph

  "A LabworkBinding" should {
    "return a RDF graph representation of a labwork" in {
      val graph = labwork.toPG.graph

      graph isIsomorphicWith labworkGraph shouldBe true
    }

    "return a labwork based on a RDF representation" in {
      val expectedLabwork = PointedGraph[Rdf](URI(Labwork.generateUri(labwork)), labworkGraph).as[Labwork]

      expectedLabwork match {
        case Success(s) =>
          s shouldEqual labwork
        case Failure(e) =>
          fail(s"Unable to deserialise labwork graph: $e")
      }
    }

    "return a labwork atom based on an RDF representation" in {
      import bindings.{
      SemesterDescriptor,
      EmployeeDescriptor,
      CourseDescriptor,
      DegreeDescriptor,
      LabworkDescriptor,
      LabworkAtomDescriptor
      }

      val semester = Semester("semester", "abr", LocalDate.now, LocalDate.now, LocalDate.now)
      val employee = Employee("systemid", "lastname", "firstname", "email", "status")
      val course = Course("course", "description", "abbr", employee.id, 1)
      val degree = Degree("degree", "abbr")
      val labwork = Labwork("labwork", "description", semester.id, course.id, degree.id, false, false)

      val courseAtom = CourseAtom("course", "description", "abbr", employee, 1, course.id)
      val labworkAtom = LabworkAtom("labwork", "description", semester, courseAtom, degree, labwork.subscribable, labwork.published, labwork.id)

      repo add semester
      repo add employee
      repo add course
      repo add degree
      repo add labwork

      repo.get[LabworkAtom](Labwork.generateUri(labwork)) match {
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
