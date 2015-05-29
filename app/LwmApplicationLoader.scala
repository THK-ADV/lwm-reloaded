import java.io.File

import controllers.{Assets, HomepageController, DegreeCRUDController}
import play.api.routing.{SimpleRouter, Router}
import play.api.{BuiltInComponentsFromContext, Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import router.Routes
import store.{Namespace, SesameRepository, SemanticRepository}

class LwmApplicationLoader extends ApplicationLoader {
  def load(context: Context): Application = {
    new LwmApplication(context).application
  }
}

trait SemanticRepositoryModule {
  self: LwmApplication =>
  def semanticRepository: SesameRepository

  def namespace: Namespace
}

trait DefaultSemanticRepositoryModule extends SemanticRepositoryModule {
  self: LwmApplication =>
  def namespace: Namespace = Namespace("http://lwm/")

  def semanticRepository: SesameRepository = SesameRepository(namespace)
}

trait DegreeManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>
  lazy val degreeManagementController = new DegreeCRUDController(semanticRepository, namespace)
}

trait HomepageModule {
  self: LwmApplication =>
  lazy val homepage = new HomepageController
}

trait AssetsModule {
  self: LwmApplication =>
  lazy val assetsController = new Assets(httpErrorHandler)
}

class LwmApplication(context: Context) extends BuiltInComponentsFromContext(context)
with HomepageModule with DefaultSemanticRepositoryModule with DegreeManagementModule with AssetsModule {
  lazy val router: Router = new Routes(httpErrorHandler, homepage, degreeManagementController, assetsController)
}
