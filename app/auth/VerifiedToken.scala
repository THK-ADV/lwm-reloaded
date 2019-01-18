package auth

sealed trait VerifiedToken {
  def id: String
  def allowedOrigins: Set[String]
}

case class UserToken(id: String, allowedOrigins: Set[String], firstName: String, lastName: String, systemId: String, email: String, status: String) extends VerifiedToken
