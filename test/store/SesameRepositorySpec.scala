package store

import java.util.UUID

import base.TestBaseDefinition
import models._
import models.applications.LabworkApplication
import models.security._
import models.users.Student
import org.scalatest.WordSpec
import org.w3.banana.sesame.{Sesame, SesameModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import security.Permissions
import scala.util.{Failure, Success}

class SesameRepositorySpec extends WordSpec with TestBaseDefinition with SesameModule {

  import ops._

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  val lwm = LWMPrefix[Sesame]

  import bindings.StudentBinding._
  import bindings.uuidBinder
  import bindings.uuidRefBinder

  lazy val repo = SesameRepository(ns)

  "Sesame Repository" should {
    "add an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)

      val g = repo.add(student)

      val expectedGraph = URI(Student.generateUri(student)).a(lwm.Student)
        .--(lwm.systemId).->-(student.systemId)
        .--(lwm.firstname).->-(student.firstname)
        .--(lwm.lastname).->-(student.lastname)
        .--(lwm.registrationId).->-(student.registrationId)
        .--(lwm.email).->-(student.email)
        .--(lwm.enrollment).->-(student.enrollment)(ops, uuidRefBinder(Degree.splitter))
        .--(lwm.id).->-(student.id).graph

      g match {
        case Success(pointedGraph) =>
          pointedGraph.graph.isIsomorphicWith(expectedGraph) shouldBe true
        case Failure(e) =>
          fail(s"Addition not successful: $e")
      }
    }

    "simultaneously add many entities" in {
      val student1 = Student("mi1111", "Carl", "A", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student2 = Student("mi1112", "Claus", "B", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student3 = Student("mi1113", "Tom", "C", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student4 = Student("mi1114", "Bob", "D", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)

      val students = List(student1, student2, student3, student4)

      val g = repo.addMany(students)
      val studentsFromRepo = repo.get[Student]

      (g, studentsFromRepo) match {
        case (Success(graphs), Success(fromRepo)) =>
          fromRepo.size shouldBe students.size
          fromRepo foreach { student =>
            students.contains(student) shouldBe true
          }
        case _ => fail("Addition not successful")
      }
    }

    "delete an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)

      val repoSize = repo.size
      val graph = repo deleteCascading Student.generateUri(student)

      graph match {
        case Success(s) =>
          repo get Student.generateUri(student) map (o => o.isEmpty shouldBe true)
        case Failure(e) =>
          fail("repo could not delete the given entity")
      }
    }

    "delete an entity reflectively" in {
      import bindings.AuthorityBinding._
      import bindings.RefRoleBinding._

      val user = UUID.randomUUID()
      val employeeRole = Role("Employee", Permissions.course.all)
      val studentRole = Role("Student", Set(Permissions.labworkApplication.create))

      val refrole1 = RefRole(None, employeeRole.id)
      val refrole2 = RefRole(Some(UUID.randomUUID()), studentRole.id)
      val auth = Authority(user, Set(refrole1.id, refrole2.id))

      repo.addMany[RefRole](List(refrole1, refrole2))
      repo.add[Authority](auth)

      repo.get[Authority](Authority.generateUri(auth)) match {
        case Success(Some(auth2)) =>
          auth2.refRoles.size shouldBe 2
          List(refrole1, refrole2).forall(a => auth2.refRoles.contains(a.id)) shouldBe true
        case _ => fail("repo could not retrieve given entity")
      }

      repo.deleteCascading(RefRole.generateUri(refrole1))

      repo.get[Authority](Authority.generateUri(auth)) match {
        case Success(Some(auth2)) =>
          auth2.refRoles.size shouldBe 1
          auth2.refRoles.head shouldBe refrole2.id
        case _ => fail("repo could not retrieve the given entity")
      }
    }

    "delete an entity non-reflectively" in {
      import bindings.LabworkApplicationBinding._
      import bindings.LabworkBinding._

      val applicant = UUID.randomUUID()
      val plan = AssignmentPlan(0, Set.empty[AssignmentEntry])
      val lab = Labwork("Labwork", "Description", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), plan)
      val application = LabworkApplication(lab.id, applicant, Set.empty)

      repo.add(lab)
      repo.add(application)

      repo.get[LabworkApplication](LabworkApplication.generateUri(application.id)) match {
        case Success(Some(app)) =>
          app.labwork shouldBe lab.id
          app.applicant shouldBe applicant
          app.friends.isEmpty shouldBe true
        case _ => fail("repo could not retrieve given entity")
      }
      repo.deleteSimple(Labwork.generateUri(lab))

      repo.get[LabworkApplication](LabworkApplication.generateUri(application.id)) match {
        case Success(Some(app)) =>
          app.labwork shouldBe lab.id
          app.applicant shouldBe applicant
          app.friends.isEmpty shouldBe true
        case _ => fail("repo could not retrieve given entity")
      }

      repo.get[Labwork](Labwork.generateUri(lab)) match {
        case Success(opt) =>
          opt shouldBe None
        case _ => fail("repo could not retrieve given entity")
      }
    }

    "delete entities" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val studentUri = Student.generateUri(student)

      repo.add(student)
      repo.contains(studentUri) shouldBe true

      repo.deleteCascading(studentUri)
      repo.contains(studentUri) shouldBe false

      repo should have size 0
    }

    "get list of entities" in {
      val student1 = Student("mi1111", "Carl", "A", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student2 = Student("mi1112", "Claus", "B", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student3 = Student("mi1113", "Tom", "C", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student4 = Student("mi1114", "Bob", "D", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)

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
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      repo add student

      val explicitStudent = repo.get[Student](Student.generateUri(student))

      explicitStudent match {
        case Success(Some(s)) =>
          s shouldEqual student
        case Success(None) =>
          fail("repo could not unwrap an optional type")
        case Failure(e) =>
          fail("repo could not return explicit entity")
      }
    }

    "simultaneously get many entities" in {
      val student1 = Student("mi1111", "Carl", "A", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student2 = Student("mi1112", "Claus", "B", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student3 = Student("mi1113", "Tom", "C", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val student4 = Student("mi1114", "Bob", "D", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)

      val students = List(student1, student2, student3, student4)

      repo.addMany(students)
      val g = repo.getMany[Student](students.map(Student.generateUri))

      g match {
        case Success(s) =>
          s.toList shouldEqual students
        case Failure(e) =>
          fail(s"repo could not return many students: $e")
      }
    }

    "update an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val studentUpdated = Student("mi1111", "Carlo", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)

      val g = repo.add(student)

      val expectedGraph = URI(Student.generateUri(student)).a(lwm.Student)
        .--(lwm.systemId).->-(student.systemId)
        .--(lwm.firstname).->-(student.firstname)
        .--(lwm.lastname).->-(student.lastname)
        .--(lwm.registrationId).->-(student.registrationId)
        .--(lwm.email).->-(student.email)
        .--(lwm.enrollment).->-(student.enrollment)(ops, uuidRefBinder(Degree.splitter))
        .--(lwm.id).->-(student.id).graph

      val expectedGraphUpdated = URI(Student.generateUri(studentUpdated)).a(lwm.Student)
        .--(lwm.systemId).->-(studentUpdated.systemId)
        .--(lwm.firstname).->-(studentUpdated.firstname)
        .--(lwm.lastname).->-(studentUpdated.lastname)
        .--(lwm.registrationId).->-(studentUpdated.registrationId)
        .--(lwm.email).->-(studentUpdated.email)
        .--(lwm.enrollment).->-(studentUpdated.enrollment)(ops, uuidRefBinder(Degree.splitter))
        .--(lwm.id).->-(studentUpdated.id).graph

      g match {
        case Success(graph) =>
          graph.graph.isIsomorphicWith(expectedGraph) shouldBe true

          implicit val generator = Student
          val updated = repo.update(studentUpdated)
          updated match {
            case Success(pointedGraph) =>
              pointedGraph.graph.isIsomorphicWith(expectedGraphUpdated) shouldBe true
            case Failure(e) =>
              fail(s"Could not update student: $e")
          }

        case Failure(e) =>
          fail(s"Student could not be added to graph: $e")
      }
    }

    "update an entity that is referenced by further entities" in {
      import bindings.RoleBinding._
      import bindings.RefRoleBinding._

      val role1 = Role("Role1", Set(Permission("P1")))
      val role2 = Role("Role1", Set(Permission("P1"), Permission("P2")), role1.id)
      val refrole = RefRole(None, role1.id)

      repo.add(role1)
      repo.add(refrole)

      repo.update(role2)(roleBinder, Role)

      repo.get[Role](Role.generateUri(role1)) match {
        case Success(Some(role)) => role shouldBe role2
        case _ => fail("repo could not retrieve the given entity")
      }

      repo.get[RefRole](RefRole.generateUri(refrole)) match {
        case Success(Some(ref)) =>
          repo.get[Role](Role.generateUri(ref.role)) match {
            case Success(Some(role)) => role shouldBe role2
            case _ => fail("repo could not retrieve inner entity")
          }
        case _ => fail("repo could not retrieve the given entity")
      }
    }

    "contains an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)
      val anotherStudent = Student("mi1112", "Carlo", "Heinz", "117273", "mi1112@gm.fh-koeln.de", Degree.randomUUID, Student.randomUUID)

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
