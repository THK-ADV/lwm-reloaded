package services

import dao.{CourseDao, LabworkDao}
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.Future

trait BackupService[SerializationFormat] {
  def backup: Future[List[SerializationFormat]]
}

final class PostgresBackupService(private val courseDao: CourseDao,
                                  private val labworkDao: LabworkDao) extends BackupService[JsValue] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def backup: Future[List[JsValue]] = {
    val x = for {
      courses <- courseDao.get(atomic = false, validOnly = false)
      labworks <- labworkDao.get(atomic = false, validOnly = false)
    } yield List(
      Json.toJson(courses),
      Json.toJson(labworks)
    )

    x
  }
}
