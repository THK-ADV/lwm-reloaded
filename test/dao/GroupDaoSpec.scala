package dao

import database._
import models._
import play.api.inject.guice.GuiceableModule
import services.GroupService._
import slick.dbio.{DBIOAction, Effect, NoStream}
import slick.jdbc.PostgresProfile.api._

final class GroupDaoSpec extends AbstractExpandableDaoSpec[GroupTable, GroupDb, GroupLike] {

  import AbstractDaoSpec._
  import scala.util.Random.{nextBoolean, nextInt, shuffle}
  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val privateStudents = populateStudents(20 * 8 * 2)
  private lazy val privateLabs = populateLabworks(10)(semesters, courses, degrees)

  private def takeSomeStudents(amount: Int) = shuffle(privateStudents) take (if (nextBoolean) amount + nextInt(3) else amount - nextInt(3))

  "A GroupDaoSpec also" should {
    "filter by students properly" in {
      val superPrivateStudents = populateStudents(3)
      val superPrivateStudent1 = superPrivateStudents(0)
      val superPrivateStudent2 = superPrivateStudents(1)
      val superPrivateStudent3 = superPrivateStudents(2)

      val groups = List(
        GroupDb("A", takeOneOf(privateLabs).id, takeSomeStudents(5).+:(superPrivateStudent1).map(_.id).toSet),
        GroupDb("B", takeOneOf(privateLabs).id, takeSomeStudents(5).map(_.id).toSet),
        GroupDb("C", takeOneOf(privateLabs).id, takeSomeStudents(5).+:(superPrivateStudent1).map(_.id).toSet),
        GroupDb("D", takeOneOf(privateLabs).id, takeSomeStudents(5).+:(superPrivateStudent2).map(_.id).toSet),
        GroupDb("E", takeOneOf(privateLabs).id, takeSomeStudents(5).map(_.id).toSet),
        GroupDb("F", takeOneOf(privateLabs).id, takeSomeStudents(5).+:(superPrivateStudent3).map(_.id).toSet)
      )

      runAsync(TableQuery[UserTable].forceInsertAll(superPrivateStudents))(_ => Unit)
      async(dao.createMany(groups))(_ should contain theSameElementsAs groups)

      async(dao.get(List(GroupStudentTableFilter(superPrivateStudent1.id.toString)), atomic = false)) { g =>
        g.map(_.asInstanceOf[Group]) should contain theSameElementsAs groups.filter(_.members.contains(superPrivateStudent1.id)).map(_.toUniqueEntity)
      }

      async(dao.get(List(GroupStudentTableFilter(superPrivateStudent2.id.toString)), atomic = false)) { g =>
        g.map(_.asInstanceOf[Group]) should contain theSameElementsAs List(groups(3).toUniqueEntity)
      }

      async(dao.get(List(GroupStudentTableFilter(superPrivateStudent3.id.toString)), atomic = false)) { g =>
        g.map(_.asInstanceOf[Group]) should contain theSameElementsAs List(groups.last.toUniqueEntity)
      }
    }
  }

  override protected val toAdd: List[GroupDb] = populateGroups(20)(privateLabs, privateStudents)

  override protected val numberOfUpdates: Int = 5

  override protected val numberOfDeletions: Int = 5

  override protected def update(toUpdate: List[GroupDb]): List[GroupDb] = toUpdate.zip(alphabeticalOrdering(toUpdate.size)).map {
    case (g, alpha) => g.copy(label = alpha, members = takeSomeStudents(g.members.size).map(_.id).toSet)
  }

  override protected def atom(dbModel: GroupDb): GroupLike = GroupAtom(
    dbModel.label,
    (privateLabs ++ labworks).find(_.id == dbModel.labwork).get.toUniqueEntity,
    (students ++ privateStudents).filter(s => dbModel.members.contains(s.id)).map(_.toUniqueEntity).toSet,
    dbModel.id
  )

  override protected def name: String = "groupDao"

  override protected val dbEntity: GroupDb = populateGroups(maxGroups)(labworks, students).head.copy(members = Set.empty)

  override protected val invalidDuplicateOfDbEntity: GroupDb = dbEntity

  override protected val invalidUpdateOfDbEntity: GroupDb = dbEntity

  override protected val validUpdateOnDbEntity: GroupDb = dbEntity.copy(label = "hello")

  override protected val dbEntities: List[GroupDb] = groups

  override protected def lwmAtom: GroupLike = atom(dbEntity)

  override protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(students ++ privateStudents ++ employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks ++ privateLabs)
  )

  override protected def expanderSpecs(dbModel: GroupDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = DBIO.seq(
    dao.groupMembershipQuery.filter(_.group === dbModel.id).result.map { memberships =>
      memberships.map(_.student) should contain theSameElementsAs (if (isDefined) dbModel.members else Nil)
    }
  )

  override protected val dao: GroupDao = app.injector.instanceOf(classOf[GroupDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
