package controllers.helper

import auth.UserToken
import play.api.libs.typedmap.TypedKey
import play.api.mvc.Request

trait RequestOps {

  implicit class RebaseRequest[A](val request: Request[A]) {
    def append(query: (String, Seq[String])*): Request[A] = {
      val queryString = query.foldLeft(request.queryString)(_ + _)
      val headers = request.withTarget(request.target.withQueryString(queryString))
      Request(headers, request.body)
    }

    def overrideQueryString(query: Map[String, Seq[String]]): Request[A] = {
      val headers = request.withTarget(request.target.withQueryString(query))
      Request(headers, request.body)
    }

    def withUserToken(userToken: UserToken): Request[A] = request.addAttr(RequestOps.UserToken, userToken)

    def userToken: Option[UserToken] = request.attrs.get(RequestOps.UserToken)

    def systemId: Option[String] = userToken.map(_.systemId)
  }

}

object RequestOps {
  val UserToken = TypedKey.apply[UserToken]("userToken")
}