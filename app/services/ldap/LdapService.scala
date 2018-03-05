package services.ldap

import scala.concurrent.Future

trait LdapService {
  def authenticate(user: String, password: String): Future[Boolean]

  def user(user: String): Future[LdapUser]

  def users(users: Set[String]): Future[Set[LdapUser]]
}
