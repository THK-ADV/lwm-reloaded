package controllers

import com.google.inject.Inject
import controllers.helper.{JsonParser, ResultOps, SecureControllerContext, Secured}
import dao.{AuthorityDao, UserDao}
import models.Role.Admin
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{AbstractController, ControllerComponents}
import security.SecurityActionChain
import utils.student_query_engine.{Expression, Operator, StudentQueryEngine}

import scala.concurrent.{ExecutionContext, Future}

final class QueryController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  private val userDao: UserDao,
  private val queryEngine: StudentQueryEngine,
  private implicit val context: ExecutionContext
) extends AbstractController(cc)
  with JsonParser
  with ResultOps
  with Secured
  with SecureControllerContext {

  implicit val readsOp: Reads[Operator] = json =>
    json.asOpt[String]
      .flatMap(Operator.apply)
      .fold[JsResult[Operator]](JsError(s"expected value to be any of ${Operator.values()}, but was $json"))(JsSuccess(_))

  implicit val readsExpr: Reads[Expression] = json => {
    val res = json.\("tag").asOpt[String].flatMap { tag =>
      val value = json.\("expr").getOrElse(JsNull)

      tag match {
        case "single" => Some(readsSingle.reads(value))
        case "combined" => Some(readsCombined.reads(value))
        case _ => None
      }
    }

    res getOrElse JsError(s"expected field 'tag' to be 'single' or 'combined', but was $json")
  }

  implicit val readsExprKey: Reads[Expression.Key] = json =>
    json.asOpt[String]
      .flatMap(Expression.Key.apply)
      .fold[JsResult[Expression.Key]](JsError(s"expected value to be any of ${Expression.Key.values()}, but was $json"))(JsSuccess(_))

  implicit val readsSingle: Reads[Expression.Single] = Json.reads[Expression.Single]

  implicit val readsCombined: Reads[Expression.Combined] = (
    (JsPath \ "lhs").lazyRead[Expression](readsExpr) and
      (JsPath \ "rhs").lazyRead[Expression](readsExpr) and
      (JsPath \ "operator").read[Operator](readsOp)
    ) (Expression.Combined.apply _)

  def queryOptions() = contextFrom(Get) action { _ =>
    Ok(Json.toJson(Expression.Key.values()))
  }

  def performQuery() = contextFrom(Create) asyncAction { request =>
    val result = for {
      expr <- Future.fromTry(parseJson[Expression](request))
      f = queryEngine.makeFilter(expr)
      users <- userDao.filter(f)
    } yield users

    result.jsonResult
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}
