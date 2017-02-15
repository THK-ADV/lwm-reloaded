package models

import java.util.UUID

case class PostgresLecturer(systemId: String, lastname: String, firstname: String, email: String, id: UUID = User.randomUUID) extends User