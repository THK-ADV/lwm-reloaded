package service.backup

import play.api.libs.json.JsValue

case class BackupItem(fileName: String, data: JsValue) {
  val writable = data.toString
  val fileExtension = "json"
}
