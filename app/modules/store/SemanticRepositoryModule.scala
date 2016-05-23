package modules.store

import java.io.File
import java.nio.file.Files

import modules.ConfigurationModule
import store.{Namespace, SesameRepository}

trait DbFolder {
  def folder: Option[File]
}

//Note: Relative paths do not directly work.
trait DefaultDbFolderImpl extends DbFolder {
  self: ConfigurationModule =>

  override lazy val folder = lwmConfig.underlying.getString("lwm.store.path") match {
    case path if path.nonEmpty =>
      val file = new File(path)

      if (Files.exists(file.toPath))
        Some(file)
      else
        Some(Files.createDirectory(file.toPath).toFile)
    case _ => None
  }
}

trait BaseNamespace {
  def namespace: Namespace
}

trait ConfigurableBaseNamespace extends BaseNamespace {
  self: ConfigurationModule =>
  lwmConfig.underlying.resolve()

  override val namespace: Namespace = Namespace(lwmConfig.underlying.getString("lwm.namespace"))
}

trait SemanticRepositoryModule {

  def repository: SesameRepository
}

trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  self: BaseNamespace with DbFolder =>

  val repository: SesameRepository = SesameRepository(folder, namespace)
}
