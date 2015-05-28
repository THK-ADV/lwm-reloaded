package utils


import com.unboundid.ldap.sdk._
import com.unboundid.util.ssl.{SSLUtil, TrustAllTrustManager}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
 * The [[LDAPAuthentication]] object enables the user to communicate with an LDAP service.
 */
object LDAPAuthentication {

  private val log = LoggerFactory.getLogger(getClass.getName)

  private val trustManager = new TrustAllTrustManager()
  // Yes, it is actually a bad idea to trust every server but for now it's okay as we only use it with exactly one server in a private network.
  private val sslUtil = new SSLUtil(trustManager)
  private val connectionOptions = new LDAPConnectionOptions()
  connectionOptions.setAutoReconnect(true)
  connectionOptions.setUseSynchronousMode(true)

  /**
   * Tries to authenticate a user with the the LDAP service.
   * @param user the user
   * @param password the password for this user
   * @return either a boolean if the connection was successful or a String with the error message
   */
  def authenticate(user: String, password: String, bindHost: String, bindPort: Int, dn: String): Future[Either[String, Boolean]] = {
    val bindDN = s"uid=$user, $dn"
    val bindRequest = new SimpleBindRequest(bindDN, password)
    Future {
      bind[Boolean](bindHost, bindPort, dn, "", ssl = true) { connection ⇒
        try {
          val bindResult = connection.bind(bindRequest)
          if (bindResult.getResultCode == ResultCode.SUCCESS) Right(true) else Left("Invalid credentials")
        } catch {
          case e: LDAPException ⇒
            Left(e.getMessage)
        } finally {
          connection.close()
        }
      }
    }.recover {
      case NonFatal(t) ⇒ Left("No response from LDAP.")
    }
  }

  /**
   * Grabs all groups from LDAP.
   * @param user the user
   * @param bindHost the host
   * @param bindPort the port
   * @param dn the dn
   * @return Either an error message or a with the names of the groups
   */
  def groupMembership(user: String, bindHost: String, bindPort: Int, dn: String): Future[Either[String, Set[String]]] = Future {
    bind(bindHost, bindPort, dn, "") {
      connection ⇒
        try {
          import scala.collection.JavaConverters._
          val results = connection.search(dn, SearchScope.SUB, "(cn=*)", "*")
          Right(results.getSearchEntries.asScala.filter(_.getAttribute("memberUid").getValues.toList.contains(user)).map(_.getAttribute("cn").getValue).toSet)
        } catch {
          case e: LDAPException ⇒ Left(e.getMessage)
        } finally {
          connection.close()
        }
    }
  }

  def isMemberOfGroup(user: String, group: String, bindHost: String, bindPort: Int, dn: String) = Future {
    bind(bindHost, bindPort, dn, "") {
      connection ⇒
        try {
          val results = connection.search(s"cn=$group,$dn", SearchScope.SUB, s"(memberUid=$user)", "*")
          Right(results.getEntryCount > 0)
        } catch {
          case e: LDAPException ⇒ Left(e.getMessage)
        } finally {
          connection.close()
        }
    }
  }

  def getName(user: String, bindHost: String, bindPort: Int, dn: String): Future[Either[String, Option[(String, String)]]] = Future {
    bind(bindHost, bindPort, dn, "") {
      connection ⇒
        try {
          import scala.collection.JavaConverters._
          val results = connection.search(s"uid=$user,$dn", SearchScope.SUB, s"(uid=$user)", "sn", "givenName").getSearchEntries.asScala

          if (results.size == 1) {
            val sn = results.head.getAttribute("sn").getValue
            val givenName = results.head.getAttribute("givenName").getValue
            Right(Some((givenName, sn)))
          } else {
            Right(None)
          }
        } catch {
          case e: LDAPException ⇒ Left(e.getMessage)
        } finally {
          connection.close()
        }
    }
  }

  def nameInfo(user: String, bindHost: String, bindPort: Int, dn: String): Future[Either[String, String]] = ???

  /**
   * Establishes a connection with the LDAP Server and runs an arbitrary function.
   * @param host the host of the LDAP server
   * @param port the port of the LDAP Server
   * @param dn
   * @param password the password needed for the binding operation
   * @param ssl is it a secure connection?
   * @param f the function that is executed when the connection was established
   * @tparam A the return value when the function was successfully executed
   * @return the result of the function f
   */
  def bind[A](host: String, port: Int, dn: String, password: String, ssl: Boolean = true)(f: LDAPConnection ⇒ Either[String, A]): Either[String, A] = {
    try {
      if (ssl) {
        val sslContext = sslUtil.createSSLContext("SSLv3")
        val connection = new LDAPConnection(sslContext.getSocketFactory)
        connection.setConnectionOptions(connectionOptions)
        connection.connect(host, port)
        f(connection)
      } else {
        val connection = new LDAPConnection(host, port)
        f(connection)
      }
    } catch {
      case NonFatal(t) ⇒
        Left(t.getMessage)

    }
  }
}