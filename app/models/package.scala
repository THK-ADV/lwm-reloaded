import java.util.UUID

package object models {

  trait UniqueEntity {
    def id: UUID

    def randomUUID: UUID = UUID.randomUUID
  }
}
