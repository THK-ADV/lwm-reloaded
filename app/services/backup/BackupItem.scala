package services.backup

import play.api.libs.json.JsValue

sealed trait BackupItem[T] {
  def fileName: String
  def data: T
  def writable: String
  def fileExtension: String
}

case class JsonBackupItem(fileName: String, data: JsValue) extends BackupItem[JsValue] {
  override val writable = data.toString
  override val fileExtension = "json"
}

case class StringBackupItem(fileName: String, data: String) extends BackupItem[String] {
  override val writable = data
  override val fileExtension = "txt"
}
