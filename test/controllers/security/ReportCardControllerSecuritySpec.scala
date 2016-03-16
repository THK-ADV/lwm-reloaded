package controllers.security


import base.TestBaseDefinition
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.WordSpec

import scala.concurrent.Future

class ReportCardControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition  {

  when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

  "A ReportCardControllerSecuritySpec " should {
    /*"Allow non restricted context invocations when student wants to get his report card" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, reportCard.get))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/reportCards/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block other non restricted context invocations" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, god))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/reportCards"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow restricted context invocations when mv wants to update a report card" in new FakeApplication() {
      import ReportCard.writes

      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCard.update))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.toJson(ReportCard.empty)

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCards/${UUID.randomUUID()}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardV1Json)),
        json
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted context invocations when ma wants to get all report cards" in new FakeApplication() {
      import ReportCard.writes

      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCard.getAll))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/reportCards"
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(Set.empty[ReportCard])
    }

    "Allow restricted context invocations when hk wants to get a single report card" in new FakeApplication() {
      import ReportCard.writes

      when(roleService.authorityFor(FakeHk.toString)).thenReturn(Success(Some(FakeHkAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCard.get))(FakeHkAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/reportCards/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeHk.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }*/
  }
}
