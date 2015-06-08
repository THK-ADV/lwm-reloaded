package store

import base.TestBaseDefinition
import models.users.Student
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import org.scalatest.WordSpec
import org.w3.banana.sesame.{Sesame, SesameModule}
import scala.util.{Failure, Success}

class SesameRepositorySpec extends WordSpec with TestBaseDefinition with SesameModule {

  import ops._

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  val lwm = LWMPrefix[Sesame]

  import bindings.StudentBinding._
  import bindings.uuidBinder

  lazy val repo = SesameRepository(ns)

  "Sesame Repository" should {
    "add a student" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de")

      val g = repo.add(student)

      val expectedGraph = (
        URI(Student.generateUri(student)).a(lwm.Student)
          -- lwm.systemId ->- student.systemId
          -- lwm.firstname ->- student.firstname
          -- lwm.lastname ->- student.lastname
          -- lwm.registrationId ->- student.registrationId
          -- lwm.email ->- student.email
          -- lwm.id ->- student.id
        ).graph

      g match {
        case Success(graph) =>
          graph.isIsomorphicWith(expectedGraph) shouldBe true
        case Failure(e) =>
          fail(s"Addition not successful: $e")
      }
    }
    "delete a student" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de")

      val repoSize = repo.size
      val graph = repo delete Student.generateUri(student)

      graph match {
        case Success(s) =>
          repo get Student.generateUri(student) map (o => o.isEmpty shouldBe true)
        case Failure(e) =>
          fail("repo could not delete the given entity")
      }
    }
    "delete stuff" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de")
      repo.add(student)
      val studentUri = Student.generateUri(student)

      repo.contains(studentUri) shouldBe true
      repo.delete(studentUri)

      repo.contains(studentUri) shouldBe false
      repo should have size 0
    }
    "get list of stuff" in {
      val student1 = Student("mi1111", "Carl", "A", "117272", "mi1111@gm.fh-koeln.de")
      val student2 = Student("mi1112", "Claus", "B", "117272", "mi1111@gm.fh-koeln.de")
      val student3 = Student("mi1113", "Tom", "C", "117272", "mi1111@gm.fh-koeln.de")
      val student4 = Student("mi1114", "Bob", "D", "117272", "mi1111@gm.fh-koeln.de")

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)
      repo.add(student4)

      repo.get[Student] match {
        case Success(students) =>
          students should contain theSameElementsAs Set(student1, student2, student3, student4)
        case Failure(e) =>
          fail(s"Could not get list of students: $e")
      }
    }
    "get an explicit entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de")

      val explicitStudent = repo get Student.generateUri(student)

      explicitStudent match {
        case Success(s) =>
          s.foreach(ss => ss shouldEqual student)
        case Failure(e) =>
          fail("repo could not return explicit entity")
      }

    }
    "update a student" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de")
      val studentUpdated = Student("mi1111", "Carlo", "Heinz", "117272", "mi1111@gm.fh-koeln.de")

      val g = repo.add(student)

      val expectedGraph = (
        URI(Student.generateUri(student)).a(lwm.Student)
          -- lwm.systemId ->- student.systemId
          -- lwm.firstname ->- student.firstname
          -- lwm.lastname ->- student.lastname
          -- lwm.registrationId ->- student.registrationId
          -- lwm.email ->- student.email
          -- lwm.id ->- student.id
        ).graph

      val expectedGraphUpdated = (
        URI(Student.generateUri(studentUpdated)).a(lwm.Student)
          -- lwm.systemId ->- studentUpdated.systemId
          -- lwm.firstname ->- studentUpdated.firstname
          -- lwm.lastname ->- studentUpdated.lastname
          -- lwm.registrationId ->- studentUpdated.registrationId
          -- lwm.email ->- studentUpdated.email
          -- lwm.id ->- studentUpdated.id
        ).graph

      g match {
        case Success(graph) =>
          graph.isIsomorphicWith(expectedGraph) shouldBe true
          implicit val generator = Student
          val updated = repo.update(studentUpdated)
          updated match {
            case Success(updatedGraph) =>
              updatedGraph.isIsomorphicWith(expectedGraphUpdated) shouldBe true
            case Failure(e) =>
              fail(s"Could not update student: $e")
          }

        case Failure(e) =>
          fail(s"Student could not be added to graph: $e")
      }
    }
    "contains a student" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de")
      val anotherStudent = Student("mi1112", "Carlo", "Heinz", "117273", "mi1112@gm.fh-koeln.de")

      repo add student

      val didContainStudent = repo contains Student.generateUri(student)
      val didContainAnotherStudent = repo contains Student.generateUri(anotherStudent)

      didContainStudent shouldBe true
      didContainAnotherStudent shouldBe false
    }
  }


  override protected def beforeEach(): Unit = {
    repo.reset().foreach(r => assert(repo.size == 0))
  }

  override protected def beforeAll(): Unit = {
    repo.reset().foreach(r => assert(repo.size == 0))
  }
}