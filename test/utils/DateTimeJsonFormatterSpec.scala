package utils

import base.{DateGenerator, TestBaseDefinition}
import org.joda.time.DateTime
import org.scalatest.WordSpec
import play.api.libs.json.{JsObject, JsString}

class DateTimeJsonFormatterSpec extends WordSpec with TestBaseDefinition with DateGenerator {

  import utils.date.DateTimeJsonFormatter.{readLocalDate, readLocalTime, writeDateTime, writeLocalDate, writeLocalTime}

  "A DateTimeJsonFormatterSpec" should {

    "parse json to LocalDate and LocalTime" in {
      val localDateJson = JsString("2019-01-01")
      val localTimeJson = JsString("17:13:50")

      readLocalDate.reads(localDateJson).asEither.right.value shouldEqual localDate(2019, 1, 1)
      readLocalTime.reads(localTimeJson).asEither.right.value shouldEqual localTime(17, 13, 50, 0)
    }

    "write LocalDate, LocalTime and LocalDateTime to json" in {
      val date = localDate(1990, 2, 5)
      val time = localTime(15, 33, 10, 0)
      val dt = dateTime(2018, 3, 16, 18, 33, 15, 0)

      writeLocalDate.writes(date) shouldEqual JsString("1990-02-05")
      writeLocalTime.writes(time) shouldEqual JsString("15:33:10")
      writeDateTime.writes(dt) shouldEqual JsString("2018-03-16T18:33")
    }

    "fail parsing LocalDate and LocalTime" in {
      readLocalDate.reads(JsString("dadsads")).isError shouldBe true
      readLocalTime.reads(JsString("dadsads")).isError shouldBe true
      readLocalDate.reads(JsObject.empty).isError shouldBe true
      readLocalTime.reads(JsObject.empty).isError shouldBe true
    }
  }
}
