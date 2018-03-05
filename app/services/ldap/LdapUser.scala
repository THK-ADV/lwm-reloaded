package services.ldap

final case class LdapUser(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], degreeAbbrev: Option[String])
