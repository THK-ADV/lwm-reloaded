//package modules
//
//import controllers.crud.StudentScheduleCRUDController
//
///**
// * Created by rgiacinto on 29/06/15.
// */
//trait StudentScheduleManagementModule {
//  self: SemanticRepositoryModule =>
//  def studentScheduleManagementController: StudentScheduleCRUDController
//}
//
//trait DefaultStudentScheduleManagementModuleImpl extends StudentScheduleManagementModule {
//  self: SemanticRepositoryModule =>
//  lazy val studentScheduleManagementController: StudentScheduleCRUDController = new StudentScheduleCRUDController(repository, namespace)
//}
