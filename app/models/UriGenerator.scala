package models

import store.Namespace

trait UriGenerator[T] {
  def generateUri(e: T)(implicit ns: Namespace): String
}