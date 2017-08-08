package modules

import controllers.{CourseCRUDController, CourseControllerPostgres}
import dao.{CourseDao, CourseDaoImpl}

trait CourseManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait CourseDaoModule { self: DatabaseModule =>
  def courseDao: CourseDao
}

trait DefaultCourseDaoModule extends CourseDaoModule {
  self: DatabaseModule with AuthorityDaoModule =>

  override lazy val courseDao = new CourseDaoImpl(db, authorityDao)
}

trait CourseManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with CourseDaoModule =>

  def courseManagementControllerPostgres: CourseControllerPostgres
}

trait DefaultCourseManagementModuleImplPostgres extends CourseManagementModulePostgres {
  self: SessionRepositoryModule with CourseDaoModule with AuthorityDaoModule =>

  lazy val courseManagementControllerPostgres: CourseControllerPostgres = new CourseControllerPostgres(sessionService, courseDao, authorityDao)
}