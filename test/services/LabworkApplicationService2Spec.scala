package services

import java.util.UUID

import base.PostgresDbSpec
import models._
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store.{DegreeTable, LabworkApplicationTable, UserTable}

final class LabworkApplicationService2Spec extends AbstractDaoSpec[LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] with LabworkApplicationService2 {
  import services.AbstractDaoSpec._
  import scala.util.Random.{nextInt, nextBoolean}

  val maxApplicants = 300
  val maxApplications = 100

  val applicants = (0 until maxApplicants).map(applicant).toList

  @scala.annotation.tailrec
  def randomApplicant(avoiding: Option[UUID] = None): DbUser = {
    val applicant = applicants(nextInt(maxApplicants))

    avoiding match {
      case Some(avoid) if applicant.id == avoid => randomApplicant(Some(avoid))
      case _ => applicant
    }
  }

  private def applicant(i: Int): DbUser = DbUser(i.toString, i.toString, i.toString, i.toString, User.StudentType, Some(i.toString), Some(randomDegree.id))

  private def labworkApplication(applicant: Option[UUID] = None, withFriends: Boolean = nextBoolean) = {
    val app = applicant.getOrElse(randomApplicant().id)
    val friends = if (withFriends) (0 until nextInt(2) + 1).map(_ => randomApplicant(Some(app)).id).toSet else Set.empty[UUID]

    LabworkApplicationDb(randomLabwork.id, app, friends)
  }

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(applicants)
  )

  override protected val labworkApplicationFriendService: LabworkApplicationFriendService = new LabworkApplicationFriendServiceSpec()

  override protected def name: String = "labworkApplication"

  override protected val entity: LabworkApplicationDb = labworkApplication(None, withFriends = false)

  override protected val invalidDuplicateOfEntity: LabworkApplicationDb = {
    val newFriends = if (entity.friends.isEmpty) Set(randomApplicant(Some(entity.applicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(entity.labwork, entity.applicant, newFriends)
  }

  override protected val invalidUpdateOfEntity: LabworkApplicationDb = {
    val newApplicant = randomApplicant(Some(entity.applicant)).id
    val newFriends = if (entity.friends.isEmpty) Set(randomApplicant(Some(newApplicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(entity.labwork, newApplicant, newFriends, entity.timestamp, entity.lastModified, entity.invalidated, entity.id)
  }

  override protected val validUpdateOnEntity: LabworkApplicationDb = {
    val newFriends = if (entity.friends.isEmpty) Set(randomApplicant(Some(entity.applicant)).id) else Set.empty[UUID]

    LabworkApplicationDb(entity.labwork, entity.applicant, newFriends, entity.timestamp, entity.lastModified, entity.invalidated, entity.id)
  }

  override protected val entities: List[LabworkApplicationDb] = (0 until maxApplications).map(_ => labworkApplication()).toList

  "A LabworkApplicationService2Spec " should {

    "create a labworkApplication with friends" in {
      val lapp = labworkApplication(Some(entity.applicant), withFriends = true)

      val result = await(create(lapp))
      val dbLapp = await(db.run(filterBy(List(LabworkApplicationIdFilter(lapp.id.toString))).result.headOption))
      val dbFriends = await(db.run(labworkApplicationFriendService.tableQuery.filter(_.labworkApplication === result.id).result))

      result shouldBe lapp
      Some(result.copy(result.labwork, result.applicant, Set.empty)) shouldBe dbLapp
      result.friends shouldBe dbFriends.map(_.friend).toSet
      dbFriends.forall(_.labworkApplication == result.id) shouldBe true
    }

    "create many labworkApplications with friends" in {}

    "return friends of an given applicant in a specific labwork" in {}
  }
}

final class LabworkApplicationFriendServiceSpec extends PostgresDbSpec with LabworkApplicationFriendService {
  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = ???
}