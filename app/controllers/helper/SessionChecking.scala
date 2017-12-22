package controllers.helper

import services.SessionHandlingService

trait SessionChecking {
  implicit def sessionService: SessionHandlingService
}
