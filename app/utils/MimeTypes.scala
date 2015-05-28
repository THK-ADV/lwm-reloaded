package utils

import play.api.http.ContentTypes
import play.api.mvc.Accepting


object MimeTypes {
  /* protocol
  *   username
  *   password */
  val loginV1Json = "application/vnd.fhk.student.V1+json"

  /* protocol
  *   systemId
  *   lastname
  *   firstname
  *   email
  */
  val userV1Json = "application/vnd.fhk.user.V1+json"

  /* protocol
  *   systemId
  *   lastname
  *   firstname
  *   email
  *   registrationId */
  val studentV1Json = "application/vnd.fhk.student.V1+json"
}

object LWMContentTypes extends ContentTypes {

  import play.api.mvc.Codec

  def loginV1ContentType(implicit codec: Codec) = withCharset(MimeTypes.loginV1Json)

  def userV1ContentType(implicit codec: Codec) = withCharset(MimeTypes.userV1Json)

  def studentV1ContentType(implicit codec: Codec) = withCharset(MimeTypes.studentV1Json)
}

object Accepts {
  val LoginV1Accept = Accepting(MimeTypes.loginV1Json)
  val UserV1Accept = Accepting(MimeTypes.userV1Json)
  val StudentV1Accept = Accepting(MimeTypes.studentV1Json)
}
