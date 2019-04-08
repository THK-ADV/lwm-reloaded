package auth

import java.math.BigInteger
import java.security.{KeyFactory, PublicKey}
import java.security.spec.RSAPublicKeySpec
import java.util.Base64

import javax.inject.Inject
import org.keycloak.TokenVerifier
import org.keycloak.adapters.KeycloakDeployment
import org.keycloak.common.VerificationException
import org.keycloak.jose.jws.{AlgorithmType, JWSHeader}
import org.keycloak.representations.AccessToken
import play.api.libs.json
import play.api.libs.json.Reads
import play.api.mvc.Request
import services.Webservice

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object KeycloakAuthorization {
  protected val SystemIdAttribute = "systemId"
  protected val FirstNameAttribute = "firstName"
  protected val LastNameAttribute = "lastName"
  protected val StatusAttribute = "status"
  protected val DegreeAttribute = "degreeAbbrev"
  protected val RegistrationIdAttribute = "registrationId"
}

class KeycloakAuthorization @Inject()(keycloakDeployment: KeycloakDeployment, ws: Webservice) extends OAuthAuthorization {

  import OAuthAuthorization._
  import KeycloakAuthorization._

  override def authorized[R](request: Request[R]): Future[VerifiedToken] = for {
    token <- asFuture(bearerToken(request), s"could not find $BearerPrefix Token in $AuthorizationHeader header")
    tokenVerifier = buildTokenVerifier(token)
    key <- getPublicKey(tokenVerifier.getHeader)
    accessToken = tokenVerifier.publicKey(key).verify().getToken
    verifiedToken <- extractAttributes(accessToken)
  } yield verifiedToken

  private def extractAttributes(accessToken: AccessToken): Future[UserToken] = {
    val attributes = accessToken.getOtherClaims.asScala
    val maybeToken = for {
      systemId <- attributes.get(SystemIdAttribute).map(_.toString)
      firstname <- attributes.get(FirstNameAttribute).map(_.toString)
      lastname <- attributes.get(LastNameAttribute).map(_.toString)
      status <- attributes.get(StatusAttribute).map(_.toString)
      id = accessToken.getId
      email = accessToken.getEmail
      allowedOrigins = accessToken.getAllowedOrigins.asScala.toSet
      degreeAbbrev = attributes.get(DegreeAttribute).map(_.toString)
      registrationId = attributes.get(RegistrationIdAttribute).map(_.toString)
    } yield UserToken(id, allowedOrigins, firstname, lastname, systemId, email, status, degreeAbbrev, registrationId)

    asFuture(maybeToken, "Can't build VerifiedToken")
  }

  private def buildTokenVerifier(token: String): TokenVerifier[AccessToken] = {
    val tokenVerifier = TokenVerifier.create(token, classOf[AccessToken]).withDefaultChecks()
    tokenVerifier.realmUrl(keycloakDeployment.getRealmInfoUrl)
    tokenVerifier
  }

  private def asFuture[A](option: Option[A], message: String): Future[A] = option match {
    case Some(s) => Future.successful(s)
    case None => Future.failed(new Throwable(message))
  }

  private case class KeycloakCert(kid: String, n: String, e: String)

  private def getPublicKey(jwsHeader: JWSHeader): Future[PublicKey] = {
    implicit def reads: Reads[KeycloakCert] = json.Json.reads[KeycloakCert]

    ws.get(keycloakDeployment.getJwksUrl) { json =>
      val key = json \ "keys" \ 0

      key.validate[KeycloakCert].asOpt.filter(_.kid == jwsHeader.getKeyId) match {
        case Some(cert) =>
          val keyFactory = KeyFactory.getInstance(AlgorithmType.RSA.toString)
          val urlDecoder = Base64.getUrlDecoder
          val modulus = new BigInteger(1, urlDecoder.decode(cert.n))
          val publicExponent = new BigInteger(1, urlDecoder.decode(cert.e))

          keyFactory.generatePublic(new RSAPublicKeySpec(modulus, publicExponent))
        case None =>
          throw new VerificationException("No matching public key found")
      }
    }
  }
}
