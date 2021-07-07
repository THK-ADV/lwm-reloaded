package auth

sealed trait VerifiedToken {
  def id: String
}

case class UserToken(
    id: String,
    firstName: String,
    lastName: String,
    systemId: String,
    campusId: String,
    email: String,
    status: String,
    degreeAbbrev: Option[String],
    registrationId: Option[String]
) extends VerifiedToken
