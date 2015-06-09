package controllers

import models.{Semester, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import store.{Namespace, SesameRepository}

class SemesterCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[Semester] {
   override implicit def rdfWrites: ToPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

   override implicit def rdfReads: FromPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Semester] = defaultBindings.SemesterBinding.classUri

   override implicit def uriGenerator: UriGenerator[Semester] = Semester

   override implicit def reads: Reads[Semester] = Semester.reads

   override implicit def writes: Writes[Semester] = Semester.writes
 }
