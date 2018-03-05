package services.ldap

import java.util.concurrent.{SynchronousQueue, ThreadPoolExecutor, TimeUnit}
import javax.net.ssl.SSLContext

import com.unboundid.ldap.sdk._
import com.unboundid.util.ssl.{SSLUtil, TrustAllTrustManager}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * The [[LdapServiceImpl]] object enables the user to communicate with an LDAP service.
  */
final class LdapServiceImpl(bindHost: String, bindPort: Int, dn: String, bindUsername: Option[String], bindPassword: Option[String]) extends LdapService {

  private implicit val executionContext: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(new ThreadPoolExecutor(0, 32, 60L, TimeUnit.SECONDS, new SynchronousQueue[Runnable]))

  private val trustManager = new TrustAllTrustManager()
  // Yes, it is actually a bad idea to trust every server but for now it's okay as we only use it with exactly one server in a private network.
  private val sslUtil = new SSLUtil(trustManager)
  private val connectionOptions = new LDAPConnectionOptions()
  connectionOptions.setAutoReconnect(true)
  connectionOptions.setUseSynchronousMode(true)

  override def authenticate(user: String, password: String): Future[Boolean] = bind() { connection ⇒
    Try(connection.bind(bindDN(user), password))
      .map(_.getResultCode == ResultCode.SUCCESS)
      .recoverWith {
        case e: LDAPException if e.getResultCode == ResultCode.INVALID_CREDENTIALS => Success(false)
        case NonFatal(e) => Failure(e)
      }
  }

  override def user(user: String) = bind(bindUsername, bindPassword) { connection ⇒
    user0(user, connection)
  }

  override def users(users: Set[String]) = bind(bindUsername, bindPassword) { connection ⇒
    Success {
      users.map { user =>
        user0(user, connection)
      }.filter(_.isSuccess).map(_.get)
    }
  }

  private def bind[A](user: Option[String] = None, password: Option[String] = None)(f: LDAPConnection => Try[A]): Future[A] = Future {
    f(connection(user, password)) match {
      case Success(a) => a
      case Failure(e) => throw new RuntimeException(e)
    }
  }

  private def connection(user: Option[String] = None, password: Option[String] = None): LDAPConnection = ssl { context =>
    val connection = (for {
      u <- user
      p <- password
      dn = bindDN(u)
    } yield new LDAPConnection(context.getSocketFactory, bindHost, bindPort, dn, p)
      ) getOrElse new LDAPConnection(context.getSocketFactory, bindHost, bindPort)

    connection.setConnectionOptions(connectionOptions)
    connection
  }

  private def bindDN(uid: String): String = s"uid=$uid,$dn"

  private def ssl[A](f: SSLContext => A): A = {
    f(sslUtil.createSSLContext("SSLv3"))
  }

  private def user0(user: String, connection: LDAPConnection) = {
    search(connection, bindDN(user)) flatMap {
      case h :: Nil => ldapUser(h).fold[Try[LdapUser]](Failure(new Throwable(s"Could not resolve user $user")))(u => Success(u))
      case _ :: t => Failure(new Throwable(s"More than one (${t.size}) LDAP entry found under username $user"))
      case _ => Failure(new Throwable("No attributes found"))
    }
  }

  private def search(connection: LDAPConnection, baseDN: String, predicate: String = "(cn=*)"): Try[List[SearchResultEntry]] = {
    import scala.collection.JavaConverters._
    Try(connection.search(baseDN, SearchScope.SUB, Filter.create(predicate))) map (_.getSearchEntries.asScala.toList)
  }

  private def ldapUser(entry: SearchResultEntry): Option[LdapUser] = {
    for {
      systemId <- attribute(entry, "uid")
      firstname <- attribute(entry, "givenName")
      lastname <- attribute(entry, "sn")
      status <- attribute(entry, "employeeType")
      regId = attribute(entry, "matriculationNumber")
      degreeAbbrev = attribute(entry, "studyPath")
      email = attribute(entry, "mail") getOrElse ""
    } yield LdapUser(systemId, lastname, firstname, email, status, regId, degreeAbbrev)
  }

  private def attribute(entry: SearchResultEntry, parameter: String): Option[String] = Option(entry.getAttributeValue(parameter))

  private def filter[B](predicate: String)(f: List[SearchResultEntry] => B): Future[B] = bind(bindUsername, bindPassword) { connection =>
    search(connection, dn, predicate) map f
  }
}
