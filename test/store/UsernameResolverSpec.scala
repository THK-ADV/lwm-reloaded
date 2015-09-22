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
      val student = Student("mi1018", "last name", "first name", "email", "registrationId", Student.randomUUID)
      val previousSize = repo.size

      repo.add(student)

      val result = usernameResolver.resolve(student.systemId)

      result match {
        case Some(uuid) =>
          previousSize shouldEqual 0
          repo.size > previousSize shouldBe true
          uuid shouldEqual student.id
        case None =>
          fail("uuid is none")
      }
    }

    "return None when username is not found" in {
      val student = Student("mi1111", "last name", "first name", "email", "registrationId", Student.randomUUID)

      repo.add(student)

      val result = usernameResolver.resolve("ai111")

      result shouldBe None
    }
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    repo.reset()
  }
}
