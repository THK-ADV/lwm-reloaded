package dao.helper

sealed trait DBError extends Throwable

object NoEntityFound extends DBError {
  override def getMessage = "no entity found"
}

case class MultipleEntitiesFound[A](entities: Seq[A]) extends DBError {
  override def getMessage = s"expected one entity, but found: $entities"
}

case class ModelAlreadyExists[A](value: A) extends DBError {
  override def getMessage = s"model already exists $value"
}
