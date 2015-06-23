//package controllers
//
//import controllers.crud.{AbstractCRUDController, GroupScheduleCRUDController}
//import models.schedules.GroupSchedule
//import play.api.libs.json.Writes
//
//class GroupScheduleCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupSchedule] {
//  override val entityToPass: GroupSchedule = GroupSchedule(GroupSchedule.randomUUID)
//
//  override def entityTypeName: String = "GroupSchedule"
//
//  override val controller: AbstractCRUDController[GroupSchedule] = new GroupScheduleCRUDController(repository, namespace)
//
//  override val entityToFail: GroupSchedule = GroupSchedule(GroupSchedule.randomUUID)
//
//  override implicit val jsonWrites: Writes[GroupSchedule] = GroupSchedule.writes
//
//  override val mimeType: String = "application/json" //TODO: this should be a proper content type
//}
