package controllers.crud

import models.UriGenerator
import models.schedules.StudentScheduleAssociation
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import store.{Namespace, SesameRepository}

class StudentScheduleAssociationCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[StudentScheduleAssociation] {
   override implicit def rdfWrites: ToPG[Sesame, StudentScheduleAssociation] = defaultBindings.StudentScheduleAssociationBinding.studentScheduleAssociationBinder

   override implicit def rdfReads: FromPG[Sesame, StudentScheduleAssociation] = defaultBindings.StudentScheduleAssociationBinding.studentScheduleAssociationBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, StudentScheduleAssociation] = defaultBindings.StudentScheduleAssociationBinding.classUri

   override implicit def uriGenerator: UriGenerator[StudentScheduleAssociation] = StudentScheduleAssociation

   override implicit def reads: Reads[StudentScheduleAssociation] = StudentScheduleAssociation.reads

   override implicit def writes: Writes[StudentScheduleAssociation] = StudentScheduleAssociation.writes
 }
