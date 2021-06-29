package base

import org.mockito.ArgumentMatchers.{any, anyString}
import org.mockito.Mockito.when
import org.scalatest.mockito.MockitoSugar
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}

import scala.concurrent.Future

trait WSClientMock extends MockitoSugar {

  val wsClient = mock[WSClient]
  val wsRequest = mock[WSRequest]
  val wSResponse = mock[WSResponse]

  when(wsClient.url(anyString())).thenReturn(wsRequest)

  when(wsRequest.withHttpHeaders(any())).thenReturn(wsRequest)
  when(wsRequest.withAuth(any(), any(), any())).thenReturn(wsRequest)
  when(wsRequest.withBody(any())(any())).thenReturn(wsRequest)
  when(wsRequest.withQueryStringParameters(any())).thenReturn(wsRequest)
  when(wsRequest.withMethod(any())).thenReturn(wsRequest)

  when(wsRequest.get()).thenReturn(Future.successful(wSResponse))
  when(wsRequest.post(anyString())(any()))
    .thenReturn(Future.successful(wSResponse))
}
