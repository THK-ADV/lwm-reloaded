package auth

import org.keycloak.TokenVerifier
import org.keycloak.adapters.KeycloakDeployment
import org.keycloak.common.VerificationException
import org.keycloak.jose.jws.{AlgorithmType, JWSHeader}
import org.keycloak.representations.AccessToken
import play.api.libs.json
import play.api.libs.json.Reads
import play.api.mvc.Request
import service.Webservice

import java.math.BigInteger
import java.security.spec.RSAPublicKeySpec
import java.security.{KeyFactory, PublicKey}
import java.util.Base64
import javax.inject.Inject
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object KeycloakAuthorization {
  protected val SystemIdAttribute = "systemId"
  protected val CampusIdAttribute = "campusId"
  protected val FirstNameAttribute = "firstName"
  protected val LastNameAttribute = "lastName"
  protected val StatusAttribute = "status"
  protected val DegreeAttribute = "degreeAbbrev"
  protected val RegistrationIdAttribute = "registrationId"
}

class KeycloakAuthorization @Inject() (
    keycloakDeployment: KeycloakDeployment,
    ws: Webservice
) extends OAuthAuthorization {

  import KeycloakAuthorization._
  import OAuthAuthorization._

  override def authorized[R](request: Request[R]): Future[VerifiedToken] = for {
    token <- asFuture(
      bearerToken(request),
      s"could not find $BearerPrefix Token in $AuthorizationHeader header"
    )
    tokenVerifier = buildTokenVerifier(token)
    key <- getPublicKey(tokenVerifier.getHeader)
    accessToken = tokenVerifier.publicKey(key).verify().getToken
    verifiedToken <- extractAttributes(accessToken)
  } yield verifiedToken

  private def extractAttributes(accessToken: AccessToken): Future[UserToken] = {
    val attributes = accessToken.getOtherClaims.asScala
    val maybeToken = for {
      systemId <- attributes.get(SystemIdAttribute).map(_.toString.toLowerCase)
      campusId <- attributes.get(CampusIdAttribute).map(_.toString.toLowerCase)
      firstname <- attributes.get(FirstNameAttribute).map(_.toString)
      lastname <- attributes.get(LastNameAttribute).map(_.toString)
      status <- attributes.get(StatusAttribute).map(_.toString)
    } yield {
      val id = accessToken.getId
      val email = accessToken.getEmail
      val degreeAbbrev = attributes.get(DegreeAttribute).map(_.toString)
      val registrationId =
        attributes.get(RegistrationIdAttribute).map(_.toString)

      UserToken(
        id,
        firstname,
        lastname,
        systemId,
        campusId,
        email,
        status,
        degreeAbbrev,
        registrationId
      )
    }

    asFuture(maybeToken, "Can't build VerifiedToken")
  }

  private def buildTokenVerifier(token: String): TokenVerifier[AccessToken] = {
    val tokenVerifier =
      TokenVerifier.create(token, classOf[AccessToken]).withDefaultChecks()
    tokenVerifier.realmUrl(keycloakDeployment.getRealmInfoUrl)
    tokenVerifier
  }

  private def asFuture[A](option: Option[A], message: String): Future[A] =
    option match {
      case Some(s) => Future.successful(s)
      case None    => Future.failed(new Throwable(message))
    }

  private case class KeycloakCert(kid: String, n: String, e: String)

  private def getPublicKey(jwsHeader: JWSHeader): Future[PublicKey] = {
    implicit def reads: Reads[KeycloakCert] = json.Json.reads[KeycloakCert]

    ws.get(keycloakDeployment.getJwksUrl) { json =>
      val key = json \ "keys" \ 0

      key
        .validate[KeycloakCert]
        .asOpt
        .filter(_.kid == jwsHeader.getKeyId) match {
        case Some(cert) =>
          val keyFactory = KeyFactory.getInstance(AlgorithmType.RSA.toString)
          val urlDecoder = Base64.getUrlDecoder
          val modulus = new BigInteger(1, urlDecoder.decode(cert.n))
          val publicExponent = new BigInteger(1, urlDecoder.decode(cert.e))

          keyFactory.generatePublic(
            new RSAPublicKeySpec(modulus, publicExponent)
          )
        case None =>
          throw new VerificationException("No matching public key found")
      }
    }
  }
}
