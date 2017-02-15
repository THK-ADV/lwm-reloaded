import java.util.UUID

import org.joda.time.DateTime

package object models {

  trait UniqueEntity {
    def id: UUID
  }

  import slick.driver.PostgresDriver.api._

  trait UniqueTable { self: Table[_] =>
    def id = column[UUID]("ID", O.PrimaryKey)
  }
}
