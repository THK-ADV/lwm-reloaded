package store

case class Namespace(base: String) {
  override def toString = if (base.endsWith("/")) base else s"$base/"
}
