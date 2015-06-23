//package controllers
//
//import controllers.crud.{StudentScheduleCRUDController, AbstractCRUDController}
//import models.schedules.StudentSchedule
//import play.api.libs.json.Writes
//
//class StudentScheduleCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentSchedule] {
//  override val entityToPass: StudentSchedule = StudentSchedule(StudentSchedule.randomUUID)
//
//  override def entityTypeName: String = "StudentSchedule"
//
//  override val controller: AbstractCRUDController[StudentSchedule] = new StudentScheduleCRUDController(repository, namespace)
//
//  override val entityToFail: StudentSchedule = StudentSchedule(StudentSchedule.randomUUID)
//
//  override implicit val jsonWrites: Writes[StudentSchedule] = StudentSchedule.writes
//
//  override val mimeType: String = "application/json" //TODO: this should be a proper content type
//}
