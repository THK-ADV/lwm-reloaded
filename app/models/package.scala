import java.util.UUID

import org.joda.time.DateTime

package object models {

  trait UniqueEntity {
    def invalidated: Option[DateTime]
    def id: UUID
  }

}
