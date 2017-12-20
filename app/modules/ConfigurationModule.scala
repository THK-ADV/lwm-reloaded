package modules

import com.typesafe.config.{ConfigFactory, Config}
import play.api.{Play, Configuration}

trait ConfigurationModule {
  def lwmConfig: Configuration
}

trait ConfigurationModuleImpl extends ConfigurationModule {
  override lazy val lwmConfig: Configuration = Configuration(ConfigFactory.defaultApplication().resolve())
}
