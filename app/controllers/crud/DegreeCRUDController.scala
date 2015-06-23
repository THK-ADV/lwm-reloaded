package controllers.crud

import models.{Degree, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import store.{Namespace, SesameRepository}

class DegreeCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[Degree]{
  override implicit def reads: Reads[Degree] = Degree.reads

  override implicit def writes: Writes[Degree] = Degree.writes

  override implicit def rdfReads: FromPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def uriGenerator: UriGenerator[Degree] = Degree

  override implicit def rdfWrites: ToPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Degree] = defaultBindings.DegreeBinding.classUri
}