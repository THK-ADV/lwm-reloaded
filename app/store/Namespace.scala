package store

class Namespace private(base: String) {
  override def toString = if (base.endsWith("/")) base else s"$base/"
}

object Namespace {
  def apply(base: String) = new Namespace(base)
}