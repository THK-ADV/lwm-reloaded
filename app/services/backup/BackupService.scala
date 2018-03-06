package services.backup

import java.io.File

import scala.concurrent.Future
import scala.io.Codec
import scala.util.Try

trait BackupService[T] {
  import services.backup.BackupService._

  def backupItems: Future[Vector[BackupItem[T]]]
  def persist(items: Vector[BackupItem[T]], file: File, shouldOverride: Boolean)(implicit encoding: String = encoding): Try[Vector[File]]

  def backup(rootFolder: File, shouldOverride: Boolean)(implicit encoding: String = encoding): Future[Vector[File]]
}

object BackupService {
  val codec = Codec.UTF8
  val encoding = codec.name
}
