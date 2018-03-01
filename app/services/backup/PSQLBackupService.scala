package services.backup

import java.io.File
import java.util.UUID

import dao.{CourseDao, LabworkDao}
import org.apache.commons.io.FileUtils
import org.joda.time.LocalDateTime
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.Future
import scala.util.Try

final class PSQLBackupService(private val courseDao: CourseDao,
                              private val labworkDao: LabworkDao) extends BackupService[JsValue] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def backupItems: Future[Vector[BackupItem[JsValue]]] = {
    for {
      courses <- courseDao.get(atomic = false, validOnly = false)
      labworks <- labworkDao.get(atomic = false, validOnly = false)
    } yield Map(
      "Courses" -> Json.toJson(courses),
      "Labworks" -> Json.toJson(labworks)
    ).map(t => JsonBackupItem(t._1, t._2)).toVector
  }

  override def persist(items: Vector[BackupItem[JsValue]], rootFolder: File, shouldOverride: Boolean)(implicit encoding: String): Try[Vector[File]] = {
    import utils.Ops.MonadInstances.tryM
    import utils.Ops._

    val backupFile = new File(rootFolder, s"Backup_${LocalDateTime.now.toString("yyyy-MM-dd_HH:mm:ss")}")

    items.map { item =>
      val file = if (shouldOverride)
        new File(backupFile, s"${item.fileName}.${item.fileExtension}")
      else
        new File(backupFile, s"${UUID.randomUUID.toString}_${item.fileName}.${item.fileExtension}")

      Try(FileUtils.write(file, item.writable, encoding)).map(_ => file) // side effect
    }.sequence
  }

  override def backup(rootFolder: File, shouldOverride: Boolean)(implicit encoding: String): Future[Vector[File]] = {
    backupItems.flatMap(items => Future.fromTry(persist(items, rootFolder, shouldOverride)))
  }
}