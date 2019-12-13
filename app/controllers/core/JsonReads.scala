package controllers.core

import play.api.libs.json.Reads

trait JsonReads[I] {
  protected implicit def reads: Reads[I]
}
