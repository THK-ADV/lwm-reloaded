package controllers.helper

import utils.LwmMimeType

trait ContentTyped {
  implicit def mimeType: LwmMimeType
}