package modules

import services.ldap.{LdapService, LdapServiceImpl}

trait LdapModule {

  def ldapService: LdapService
}

trait LdapModuleImpl extends LdapModule {
  self: ConfigurationModule =>

  val bindHost = lwmConfig.getString("lwm.ldap.bindHost").getOrElse("no bind host set")
  val bindPort = lwmConfig.getInt("lwm.ldap.bindPort").getOrElse(-1)
  val dn = lwmConfig.getString("lwm.ldap.bindDN").getOrElse("no dn set")
  val username = lwmConfig.getString("lwm.ldap.username")
  val password = lwmConfig.getString("lwm.ldap.password")

  override lazy val ldapService = new LdapServiceImpl(bindHost, bindPort, dn, username, password)
}
