import java.sql.Timestamp
import java.util.UUID

package object models {

  trait UniqueEntity {
    def id: UUID
  }

  trait UniqueDbEntity extends UniqueEntity {
    def lastModified: Timestamp

    def invalidated: Option[Timestamp]

    def toUniqueEntity: UniqueEntity
  }

}
