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
  self: LwmApplication =>
  def semanticRepository: SesameRepository

  def namespace: Namespace
}

trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  self: LwmApplication =>
  def namespace: Namespace = Namespace("http://lwm/")

  def semanticRepository: SesameRepository = SesameRepository(namespace)
}

trait DegreeManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}


trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>
  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(semanticRepository, namespace)
}

trait DefaultHomepageModuleImpl extends HomepageModule {
  self: LwmApplication =>
  lazy val homepageController = new HomepageController
}

trait HomepageModule {
  self: LwmApplication =>
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

trait LwmApplication extends BuiltInComponentsFromContext {
  self: HomepageModule with SemanticRepositoryModule with DegreeManagementModule with AssetsModule =>
  lazy val router: Router = new Routes(httpErrorHandler, homepageController, degreeManagementController, assetsController)
}

class DefaultLwmApplication(context: Context) extends LwmApplication(context)
with DefaultSemanticRepositoryModuleImpl
with DefaultHomepageModuleImpl
with DefaultDegreeManagementModuleImpl
with DefaultAssetsModuleImpl
