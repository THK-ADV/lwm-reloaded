package modules.store

import java.io.File
import java.nio.file.Files

import modules.ConfigurationModule
import store.{Namespace, SesameRepository}


trait BaseNamespace {
  def namespace: Namespace
}

trait ConfigurableBaseNamespace extends BaseNamespace {
  self: ConfigurationModule =>
  lwmConfig.underlying.resolve()

  override def namespace: Namespace = Namespace(lwmConfig.underlying.getString("lwm.namespace"))
}

trait SemanticRepositoryModule {
  self: BaseNamespace with ConfigurationModule =>

  def repository: SesameRepository
}
//TODO: ADD A PROPER PATH FOR THE STORAGE FOLDER
//Note: Relative paths do not directly work.
trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  self: BaseNamespace with ConfigurationModule =>

  val folder = {
    val file = new File(lwmConfig.underlying.getString("lwm.store.path"))

    if(Files.exists(file.toPath)) file
    else Files.createDirectory(file.toPath).toFile
  }

  val repository: SesameRepository = SesameRepository(folder, namespace)
}
