package bind

import org.joda.time.LocalDate
import java.util.UUID

import base.TestBaseDefinition
import models.semester.Semester
import org.scalatest.WordSpec
import store.Namespace

class SplitterSpec extends WordSpec with TestBaseDefinition {


  "A URLSplitter" should {

    implicit val ns = Namespace("http://lwm.gm.th-koeln.de")

    "conjoin the domain and the actual model id of a resource" in {
      val uuid = UUID.randomUUID()

      val to = Semester.splitter.to(uuid)

      to shouldBe Semester.generateUri(uuid)
    }

    "separate the domain of the model of a resource from its actual UUID" in {
      val uuid = UUID.randomUUID()
      val uri = Semester.generateUri(uuid)

      val from = Semester.splitter.from(uri)

      from shouldBe uuid
    }
  }
}
