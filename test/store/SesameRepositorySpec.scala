package store

import java.util.UUID

import base.TestBaseDefinition
import models._
import models.labwork.{Labwork, LabworkApplication, ReportCardEntry, ReportCardEntryType}
import models.security._
import models.users.{Employee, Student, StudentAtom, User}
import org.joda.time.{LocalDate, LocalTime}
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

  import bindings.{
  StudentDescriptor,
  uuidBinder,
  uuidRefBinder
  }

  lazy val repo = SesameRepository(ns)

  "Sesame Repository" should {

    "add an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)

      val g = repo.add(student)

      val expectedGraph = URI(User.generateUri(student)).a(lwm.User)
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
      val student1 = Student("mi1111", "Carl", "A", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student2 = Student("mi1112", "Claus", "B", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student3 = Student("mi1113", "Tom", "C", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student4 = Student("mi1114", "Bob", "D", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)

      val students = List(student1, student2, student3, student4)

      val g = repo.addMany(students)
      val studentsFromRepo = repo.getAll[Student]

      (g, studentsFromRepo) match {
        case (Success(graphs), Success(fromRepo)) =>
          fromRepo.size shouldBe students.size
          fromRepo foreach { student =>
            students.contains(student) shouldBe true
          }
        case _ => fail("Addition not successful")
      }
    }

    "add polymorphic entities" in {
      import bindings.UserDescriptor

      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())
      val student2 = Student("mi1818", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("wi1818", "Nahs", "Rustw", "lab@mail.de", "22331144", UUID.randomUUID())

      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "status")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "status")

      val users: Vector[User] = Vector(student1, student2, student3, employee1, employee2, employee3)

      repo.addMany[User](users)

      repo.getAll[User] match {
        case Success(s) => users forall s.contains shouldBe true
        case Failure(e) => fail(s"Retrieval not successful: $e")
      }
    }

    "delete an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)

      repo add student
      repo delete User.generateUri(student)

      val explicitStudent = repo.get[Student](User.generateUri(student))

      explicitStudent match {
        case Success(Some(s)) =>
          fail("repo should've deleted the student")
        case Success(None) =>
          repo.size shouldBe 0
        case Failure(e) =>
          fail("repo could not return explicit entity")
      }
    }

    "get list of entities" in {
      val student1 = Student("mi1111", "Carl", "A", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student2 = Student("mi1112", "Claus", "B", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student3 = Student("mi1113", "Tom", "C", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student4 = Student("mi1114", "Bob", "D", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)
      repo.add(student4)

      repo.getAll[Student] match {
        case Success(students) =>
          students should contain theSameElementsAs Set(student1, student2, student3, student4)
        case Failure(e) =>
          fail(s"Could not get list of students: $e")
      }
    }

    "get an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      repo add student

      val explicitStudent = repo.get[Student](User.generateUri(student))

      explicitStudent match {
        case Success(Some(s)) =>
          s shouldEqual student
        case Success(None) =>
          fail("repo could not unwrap an optional type")
        case Failure(e) =>
          fail("repo could not return explicit entity")
      }
    }

    "delete an arbitrarily nested entity" in {
      import bindings.{
      DegreeDescriptor,
      StudentAtomDescriptor
      }

      val degree = Degree("label", "abbr")
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", degree.id)

      repo add student
      repo add degree

      repo.delete[StudentAtom](User.generateUri(student))

      val postDegree = repo get[Degree] Degree.generateUri(degree)
      val postStudent = repo get[Student] User.generateUri(student)

      (postDegree, postStudent) match {
        case (Success(None), Success(None)) => repo.size shouldBe 0
        case (Success(Some(_)), _) => fail("one of the entities was not deleted")
        case (_, Success(Some(_))) => fail("one of the entities was not deleted")
        case _ => fail(s"entities could not be deleted")
      }
    }

    "delete arbitrarily nested entities from many others" in {
      import bindings.ReportCardEntryDescriptor

      def entries(labwork: UUID, amount: Int): Vector[ReportCardEntry] = (0 to amount).map { i =>
        ReportCardEntry(UUID.randomUUID(), labwork, s"entry$i", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(),
          Set(ReportCardEntryType(s"type$i", i % 2 == 0, scala.util.Random.nextInt),
            ReportCardEntryType(s"type$i", i % 3 == 0, scala.util.Random.nextInt)))
      }.toVector

      val batch1 = entries(UUID.randomUUID(), 15)
      val batch2 = entries(UUID.randomUUID(), 15)

      repo addMany batch1
      repo addMany batch2

      repo.delete[ReportCardEntry](ReportCardEntry.generateUri(batch2.head))

      repo.getAll[ReportCardEntry] match {
        case Success(ents) =>
          (batch1 ++ batch2.tail) foreach { elm =>
            ents contains elm shouldBe true
          }
          ents contains batch2.head shouldBe false
        case Failure(e) => fail(s"could not get entries: ${e.getMessage}")
      }
    }

    "get a polymorphic entity" in {
      import bindings.{
      StudentDescriptor,
      UserDescriptor
      }

      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())
      val student2 = Student("mi1818", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("wi1818", "Nahs", "Rustw", "lab@mail.de", "22331144", UUID.randomUUID())

      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "status")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "status")

      val users: Vector[User] = Vector(student1, student2, student3, employee1, employee2, employee3)

      repo.addMany[User](users)

      repo.get[Student](User.generateUri(student1.id)) match {
        case Success(Some(student)) =>
        case _ => fail(s"Retrieval not successful")
      }
    }

    "simultaneously get many entities" in {
      val student1 = Student("mi1111", "Carl", "A", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student2 = Student("mi1112", "Claus", "B", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student3 = Student("mi1113", "Tom", "C", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val student4 = Student("mi1114", "Bob", "D", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)

      val students = List(student1, student2, student3, student4)

      repo.addMany(students)
      val g = repo.getMany[Student](students.map(User.generateUri))

      g match {
        case Success(s) =>
          s.toList shouldEqual students
        case Failure(e) =>
          fail(s"repo could not return many students: $e")
      }
    }

    "update an entity" in {
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val studentUpdated = Student("mi1111", "Carlo", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)

      val g = repo.add(student)

      val expectedGraph = URI(User.generateUri(student)).a(lwm.User)
        .--(lwm.systemId).->-(student.systemId)
        .--(lwm.firstname).->-(student.firstname)
        .--(lwm.lastname).->-(student.lastname)
        .--(lwm.registrationId).->-(student.registrationId)
        .--(lwm.email).->-(student.email)
        .--(lwm.enrollment).->-(student.enrollment)(ops, uuidRefBinder(Degree.splitter))
        .--(lwm.id).->-(student.id).graph

      val expectedGraphUpdated = URI(User.generateUri(studentUpdated)).a(lwm.User)
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

          implicit val generator = User
          import bindings.UserDescriptor

          val updated = repo.update[User, UriGenerator[User]](studentUpdated)
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
      import bindings.{
      RoleDescriptor,
      RefRoleDescriptor
      }
      val role1 = Role("Role1", Set(Permission("P1")))
      val role2 = Role("Role1", Set(Permission("P1"), Permission("P2")), role1.invalidated, role1.id)
      val refrole = RefRole(None, role1.id)

      repo.add(role1)
      repo.add(refrole)

      repo.update(role2)(RoleDescriptor, Role)

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
      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Degree.randomUUID)
      val anotherStudent = Student("mi1112", "Carlo", "Heinz", "117273", "mi1112@gm.fh-koeln.de", Degree.randomUUID)

      repo add student

      val didContainStudent = repo contains User.generateUri(student)
      val didContainAnotherStudent = repo contains User.generateUri(anotherStudent)

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
