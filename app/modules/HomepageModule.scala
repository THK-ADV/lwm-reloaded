package modules

import controllers.HomepageController

trait HomepageModule {
  def homepageController: HomepageController
}

trait DefaultHomepageModuleImpl extends HomepageModule {
  lazy val homepageController = new HomepageController
}

