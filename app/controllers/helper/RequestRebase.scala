package controllers.helper

import play.api.mvc.Request

trait RequestRebase {

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
  }
}