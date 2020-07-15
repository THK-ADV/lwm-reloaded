package controllers

import controllers.helper._
import dao._
import javax.inject.{Inject, Singleton}
import models.Role.Admin
import play.api.mvc.{AbstractController, ControllerComponents}
import security.SecurityActionChain

import scala.concurrent.ExecutionContext

@Singleton
final class DeveloperController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractController(cc)
  with ResultOps
  with AttributeFilter
  with RequestOps
  with JsonParser
  with Secured
  with SecureControllerContext {

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(Admin))
  }
}
