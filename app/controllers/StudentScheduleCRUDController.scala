package controllers

import models.UriGenerator
import models.schedules.StudentSchedule
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import store.{Namespace, SesameRepository}

class StudentScheduleCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[StudentSchedule] {
   override implicit def rdfWrites: ToPG[Sesame, StudentSchedule] = defaultBindings.StudentScheduleBinding.studentScheduleBinder

   override implicit def rdfReads: FromPG[Sesame, StudentSchedule] = defaultBindings.StudentScheduleBinding.studentScheduleBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, StudentSchedule] = defaultBindings.StudentScheduleBinding.classUri

   override implicit def uriGenerator: UriGenerator[StudentSchedule] = StudentSchedule

   override implicit def reads: Reads[StudentSchedule] = StudentSchedule.reads

   override implicit def writes: Writes[StudentSchedule] = StudentSchedule.writes
 }
