package controllers.helper

import dao.AuthorityDao

trait Secured {
  implicit def authorityDao: AuthorityDao
}