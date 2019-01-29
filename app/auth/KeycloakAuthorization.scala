package auth

import java.math.BigInteger
import java.security.KeyFactory
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

trait OAuthAuthorization {
  protected val AuthorizationHeader = "Authorization"

  def authorized[R](request: Request[R]): Future[VerifiedToken]
}

class KeycloakAuthorization @Inject()(keycloakDeployment: KeycloakDeployment, ws: Webservice) extends OAuthAuthorization {

  override def authorized[R](request: Request[R]): Future[VerifiedToken] = for {
    token <- bearerToken(request)
    accessToken <- verifyToken(token)
    verifiedToken <- extractAttributes(accessToken)
  } yield verifiedToken

  private def extractAttributes(accessToken: AccessToken) = {
    val attributes = accessToken.getOtherClaims.asScala
    val maybeToken = for {
      systemId <- attributes.get("systemId").map(_.toString)
      firstname <- attributes.get("firstName").map(_.toString)
      lastname <- attributes.get("lastName").map(_.toString)
      status <- attributes.get("status").map(_.toString)
      id = accessToken.getId
      email = accessToken.getEmail
      allowedOrigins = accessToken.getAllowedOrigins.asScala.toSet
      degreeAbbrev = attributes.get("degreeAbbrev").map(_.toString)
      registrationId = attributes.get("registrationId").map(_.toString)
    } yield UserToken(id, allowedOrigins, firstname, lastname, systemId, email, status, degreeAbbrev, registrationId)

    asFuture(maybeToken, "Can't build VerifiedToken")
  }

  private def bearerToken[_](request: Request[_]) = {
    val token = request.headers.get(AuthorizationHeader).flatMap(_.split(" ").lastOption)
    asFuture(token, s"$AuthorizationHeader header key is missing")
  }

  private def verifyToken(token: String): Future[AccessToken] = {
    val tokenVerifier = TokenVerifier.create(token, classOf[AccessToken]).withDefaultChecks()
    tokenVerifier.realmUrl(keycloakDeployment.getRealmInfoUrl)

    getPublicKey(tokenVerifier.getHeader).map(key => tokenVerifier.publicKey(key).verify().getToken)
  }

  private def asFuture[A](option: Option[A], message: String): Future[A] = option match {
    case Some(s) => Future.successful(s)
    case None => Future.failed(new Throwable(message))
  }

  private case class KeycloakCert(kid: String, n: String, e: String)

  private def getPublicKey(jwsHeader: JWSHeader) = {
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
