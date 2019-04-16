package service.backup

import java.io.File

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Codec
import scala.util.Try

trait BackupService {

  import service.backup.BackupService._

  def backupItems: Future[Vector[BackupItem]]

  def persist(items: Vector[BackupItem], file: File, shouldOverride: Boolean)(implicit encoding: String = encoding): Try[Vector[File]]

  final def backup(rootFolder: File, shouldOverride: Boolean)(implicit executor: ExecutionContext, encoding: String = encoding): Future[Vector[File]] =
    backupItems.flatMap(items => Future.fromTry(persist(items, rootFolder, shouldOverride)))
}

object BackupService {
  val codec = Codec.UTF8
  val encoding = codec.name
}
