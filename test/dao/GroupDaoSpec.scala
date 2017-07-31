package dao

import models._
import slick.dbio.{DBIOAction, Effect, NoStream}
import store._
import slick.driver.PostgresDriver.api._
import services.GroupService._

final class GroupDaoSpec extends AbstractExpandableDaoSpec[GroupTable, GroupDb, Group] with GroupDao {
  import dao.AbstractDaoSpec._
  import scala.util.Random.{shuffle, nextBoolean, nextInt}

  private lazy val privateStudents = populateStudents(20*8*2)
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

      run(TableQuery[UserTable].forceInsertAll(superPrivateStudents))
      await(createMany(groups))

      val res1 = await(get(List(GroupStudentTableFilter(superPrivateStudent1.id.toString)), atomic = false)).map(_.asInstanceOf[PostgresGroup])
      val res2 = await(get(List(GroupStudentTableFilter(superPrivateStudent2.id.toString)), atomic = false)).map(_.asInstanceOf[PostgresGroup])
      val res3 = await(get(List(GroupStudentTableFilter(superPrivateStudent3.id.toString)), atomic = false)).map(_.asInstanceOf[PostgresGroup])

      res1.sortBy(_.label) shouldBe groups.filter(_.members.contains(superPrivateStudent1.id)).map(_.toLwmModel).sortBy(_.label)
      res2 shouldBe List(groups(3).toLwmModel)
      res3 shouldBe List(groups.last.toLwmModel)
    }
  }

  override protected val toAdd: List[GroupDb] = populateGroups(20)(privateLabs, privateStudents)

  override protected val numberOfUpdates: Int = 5

  override protected val numberOfDeletions: Int = 5

  override protected def update(toUpdate: List[GroupDb]): List[GroupDb] = toUpdate.zip(alphabeticalOrdering(toUpdate.size)).map {
    case (g, alpha) => g.copy(label = alpha, members = takeSomeStudents(g.members.size).map(_.id).toSet)
  }

  override protected def atom(dbModel: GroupDb): Group = PostgresGroupAtom(
    dbModel.label,
    (privateLabs ++ labworks).find(_.id == dbModel.labwork).get.toLwmModel,
    (students ++ privateStudents).filter(s => dbModel.members.contains(s.id)).map(_.toLwmModel).toSet,
    dbModel.id
  )

  override protected def name: String = "groupDao"

  override protected val dbEntity: GroupDb = populateGroups(maxGroups)(labworks, students).head.copy(members = Set.empty)

  override protected val invalidDuplicateOfDbEntity: GroupDb = dbEntity

  override protected val invalidUpdateOfDbEntity: GroupDb = dbEntity

  override protected val validUpdateOnDbEntity: GroupDb = dbEntity.copy(label = "hello")

  override protected val dbEntities: List[GroupDb] = groups

  override protected def lwmEntity: Group = dbEntity.toLwmModel

  override protected def lwmAtom: Group = atom(dbEntity)

  override protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(students ++ privateStudents ++ employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks ++ privateLabs)
  )

  override protected def expanderSpecs(dbModel: GroupDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = DBIO.seq(
    groupMembershipQuery.filter(_.group === dbModel.id).result.map { memberships =>
      memberships.map(_.student).toSet shouldBe (if (isDefined) dbModel.members else Set.empty)
    }
  )
}
