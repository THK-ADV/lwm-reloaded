package controllers.crud

import models.UriGenerator
import models.schedules.GroupScheduleAssociation
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import store.{Namespace, SesameRepository}

class GroupScheduleAssociationCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[GroupScheduleAssociation] {
   override implicit def rdfWrites: ToPG[Sesame, GroupScheduleAssociation] = defaultBindings.GroupScheduleAssociationBinding.groupScheduleAssociationBinder

   override implicit def rdfReads: FromPG[Sesame, GroupScheduleAssociation] = defaultBindings.GroupScheduleAssociationBinding.groupScheduleAssociationBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, GroupScheduleAssociation] = defaultBindings.GroupScheduleAssociationBinding.classUri

   override implicit def uriGenerator: UriGenerator[GroupScheduleAssociation] = GroupScheduleAssociation

   override implicit def reads: Reads[GroupScheduleAssociation] = GroupScheduleAssociation.reads

   override implicit def writes: Writes[GroupScheduleAssociation] = GroupScheduleAssociation.writes
 }
