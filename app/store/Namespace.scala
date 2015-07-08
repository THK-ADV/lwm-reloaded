package store

case class Namespace(base: String) {
  private val trimmed = if (base.endsWith("/")) base.substring(0, base.size - 1) else s"$base"
  override def toString = trimmed
}
