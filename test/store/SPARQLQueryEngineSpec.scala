package store

import base.TestBaseDefinition
import models.users.Student
import org.scalatest.WordSpec
import org.w3.banana.sesame.{Sesame, SesameModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings


class SPARQLQueryEngineSpec extends WordSpec with TestBaseDefinition with SesameModule {

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  val lwm = LWMPrefix[Sesame]

  lazy val repo = SesameRepository(ns)
  
  lazy val prefixes = LWMPrefix[repo.Rdf]

  import bindings.StudentBinding._


  "A SPARQLQueryEngine" should {

    "execute select queries" in {
      import utils.Ops._

      val student = Student("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", Student.randomUUID)

      repo add student

      val result = repo.select.map { v =>
        sequence(v map { bs =>
          repo.get[Student](bs.getValue("s").stringValue()).toOption.flatten
        })
      } <> s"""
              |Select ?s where {
              |?s <${prefixes.systemId}> "${student.systemId}"
              |}
        """.stripMargin

      result.flatten match {
        case Some(s) => s.head shouldBe student
        case _ => fail("Query returned nothing")
      }

    }

    "execute ask queries" in {
      val anotherStudent = Student("mi1112", "Carlo", "Heinz", "117273", "mi1112@gm.fh-koeln.de", Student.randomUUID)

      repo add anotherStudent

      val result = repo.ask <>
        s"""
           |ASK {
           |?s <${prefixes.systemId}> "${anotherStudent.systemId}"
           |}
         """.stripMargin

      result match {
        case Some(v) => v shouldBe true
        case _ => fail("Result was not true")
      }
    }
  }

  override protected def beforeEach(): Unit = {
    repo.reset().foreach(r => assert(repo.size == 0))
  }

  override protected def beforeAll(): Unit = {
    repo.reset().foreach(r => assert(repo.size == 0))
  }

}
