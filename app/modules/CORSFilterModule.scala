package modules

import play.api.Configuration
import play.filters.cors.{CORSConfig, CORSFilter}
import utils.LwmApplication

trait CORSFilterModule {
  self: LwmApplication =>
  def corsFilter(config: Configuration): CORSFilter
}

trait DefaultCORSFilterModule extends CORSFilterModule {
  self: LwmApplication =>
  override def corsFilter(config: Configuration): CORSFilter = CORSFilter(CORSConfig.fromConfiguration(config))

}
