package modules

import controllers.Assets
import utils.LwmApplication

trait AssetsModule {
  self: LwmApplication =>

  def assetsController: Assets
}

trait DefaultAssetsModuleImpl extends AssetsModule {
  self: LwmApplication =>

  lazy val assetsController = new Assets(httpErrorHandler)
}