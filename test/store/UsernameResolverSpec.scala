package store

import base.TestBaseDefinition
import models.users.Student
import org.scalatest.WordSpec
import org.w3.banana.sesame.{Sesame, SesameModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings

class UsernameResolverSpec extends WordSpec with TestBaseDefinition with SesameModule {

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  val lwm = LWMPrefix[Sesame]

  import bindings.StudentBinding._

  val repo = SesameRepository(ns)

  val usernameResolver = new LwmUsernameResolver(repo)

  "A UsernameResolverSpec " should {
    "resolve a given username properly" in {
      val student1 = Student("mi1018", "last name", "first name", "email", "registrationId", Student.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Student.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Student.randomUUID)

      val previousSize = repo.size

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)

      val result = usernameResolver.resolve(student1.systemId)

      result match {
        case Some(uuid) =>
          previousSize shouldEqual 0
          repo.size > previousSize shouldBe true
          uuid shouldEqual student1.id
        case None =>
          fail("uuid is none")
      }
    }

    "return None when username is not found" in {
      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Student.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Student.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Student.randomUUID)

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)

      val result = usernameResolver.resolve("ai111")

      result shouldBe None
    }
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    repo.reset()
  }
}
