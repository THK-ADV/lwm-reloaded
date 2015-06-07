package utils

import controllers.{Assets, DegreeCRUDController, HomepageController}
import play.api.ApplicationLoader.Context
import play.api.routing.Router
import play.api.{Application, ApplicationLoader, BuiltInComponentsFromContext}
import router.Routes
import store.{Namespace, SesameRepository}

class LwmApplicationLoader extends ApplicationLoader {
  def load(context: Context): Application = {
    new DefaultLwmApplication(context).application
  }
}

trait SemanticRepositoryModule {
  def repository: SesameRepository

  def namespace: Namespace
}

trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  def namespace: Namespace = Namespace("http://lwm/")

  def repository: SesameRepository = SesameRepository(namespace)
}

trait DegreeManagementModule {
  self: SemanticRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}


trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule =>
  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, namespace)
}

trait DefaultHomepageModuleImpl extends HomepageModule {
  lazy val homepageController = new HomepageController
}

trait HomepageModule {
  def homepageController: HomepageController
}

trait AssetsModule {
  self: LwmApplication =>
  def assetsController: Assets
}

trait DefaultAssetsModuleImpl extends AssetsModule {
  self: LwmApplication =>
  lazy val assetsController = new Assets(httpErrorHandler)
}

abstract class LwmApplication(context: Context) extends BuiltInComponentsFromContext(context)
with HomepageModule
with SemanticRepositoryModule
with DegreeManagementModule
with AssetsModule {
  lazy val router: Router = new Routes(httpErrorHandler, homepageController, degreeManagementController, assetsController)
}

class DefaultLwmApplication(context: Context) extends LwmApplication(context)
with DefaultSemanticRepositoryModuleImpl
with DefaultHomepageModuleImpl
with DefaultDegreeManagementModuleImpl
with DefaultAssetsModuleImpl
