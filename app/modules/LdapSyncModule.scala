package modules

import modules.store.{ResolversModule, SemanticRepositoryModule}
import services.{ActorBasedLdapSyncService, LdapSyncService}

trait LdapSyncModule {

  def ldapSyncService: LdapSyncService
}

trait DefaultLdapSyncService extends LdapSyncModule {
  self: AkkaActorSystemModule with ConfigurationModule with SemanticRepositoryModule with LDAPModule with ResolversModule =>

  val syncCron = lwmConfig.getString("lwm.ldap.cron") match {
    case Some(cron) if cron.nonEmpty => cron
    case _ => "0 0 4 1/1 * ? *" // every day at 04:00 am
  }

  override val ldapSyncService: LdapSyncService = new ActorBasedLdapSyncService(system, syncCron, repository, ldapService, resolvers)
}
