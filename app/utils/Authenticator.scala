package utils

import com.unboundid.ldap.sdk._
import com.unboundid.util.ssl.{SSLUtil, TrustAllTrustManager}
import org.joda.time
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Success, Try}

trait Authenticator {
  def authenticate(user: String, password: String): Future[Boolean]
}

case class LDAPAuthenticator(bindHost: String, bindPort: Int, DN: String, GDN: String) extends Authenticator {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val log = LoggerFactory.getLogger(getClass.getName)

  private val trustManager = new TrustAllTrustManager()
  private val sslUtil = new SSLUtil(trustManager)
  private val connectionOptions = new LDAPConnectionOptions()

  connectionOptions.setAutoReconnect(true)
  connectionOptions.setUseSynchronousMode(true)

  override def authenticate(user: String, password: String): Future[Boolean] = bind{ connection: LDAPConnection  =>
    val bindDN = s"uid=$user, $DN"
    val bindRequest = new SimpleBindRequest(bindDN, password)
    val bindResult = connection.bind(bindRequest)
    bindResult.getResultCode == ResultCode.SUCCESS
  }.recover {
    case NonFatal(t) =>
      log.error(s"Error while trying to authenticate '$user' [${DateTime.now().toString("dd-MM-yyyy (HH:mm)")}]\nMessage: ${t.getMessage}")
      false
  }

  def isMemberOfGroup(user: String, group: String): Future[Boolean] = bind{ connection: LDAPConnection  =>
    val results = connection.search(s"cn=$group,$DN", SearchScope.SUB, s"(memberUid=$user)", "*")
    results.getEntryCount > 0
  }.recover {
    case NonFatal(t) =>
      log.error(s"Error while trying to check if '$user' is member of '$group' [${DateTime.now().toString("dd-MM-yyyy (HH:mm)")}]\nMessage: ${t.getMessage}")
      false
  }


  def getName(user: String): Future[Option[(String, String)]] = bind{ connection: LDAPConnection  =>
    import scala.collection.JavaConverters._

    val results = connection.search(s"uid=$user,$DN", SearchScope.SUB, s"(uid=$user)", "sn", "givenName").getSearchEntries.asScala
    if (results.size == 1) {
      val sn = results.head.getAttribute("sn").getValue
      val givenName = results.head.getAttribute("givenName").getValue
      Some((givenName, sn))
    } else {
      None
    }
  }.recover {
    case NonFatal(t) =>
      log.error(s"Error while trying to get name for '$user' [${DateTime.now().toString("dd-MM-yyyy (HH:mm)")}]\nMessage: ${t.getMessage}")
      None
  }


  def groupMemberships(user: String): Future[Set[String]] = bind{ connection: LDAPConnection =>
    import scala.collection.JavaConverters._
    val results = connection.search(DN, SearchScope.SUB, "(cn=*)", "*")
    results.getSearchEntries.asScala.filter(_.getAttribute("memberUid").getValues.toList.contains(user)).map(_.getAttribute("cn").getValue).toSet
  }.recover {
    case NonFatal(t) =>
      log.error(s"Error while trying to get group memberships for '$user' [${DateTime.now().toString("dd-MM-yyyy (HH:mm)")}]\nMessage: ${t.getMessage}")
      Set.empty[String]
  }


  private def bind[A, B >: LDAPConnection](f: (B) => A): Future[A] = Future {
    val sslContext = sslUtil.createSSLContext("SSLv3")
    val connection = new LDAPConnection(sslContext.getSocketFactory)
    connection.setConnectionOptions(connectionOptions)
    connection.connect(bindHost, bindPort)
    val result = f(connection)
    connection.close()
    result
  }

}
