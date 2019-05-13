package di

import play.api.Configuration

trait ConfigReader {
  def config(name: String)(implicit cfg: Configuration): Option[String] = cfg getOptional[String] name

  def nonEmptyConfig(name: String)(implicit cfg: Configuration): Option[String] = config(name) filter (_.nonEmpty)
}
