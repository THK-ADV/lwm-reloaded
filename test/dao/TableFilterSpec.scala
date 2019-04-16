package dao

import java.sql.{Date, Time}
import java.util.UUID

import base.{DateGenerator, PostgresDbSpec}
import database._
import database.helper.LdapUserStatus.EmployeeStatus
import play.api.inject.guice.GuiceableModule
import slick.jdbc.PostgresProfile.api._

class FakeLabworkTable(tag: Tag) extends Table[(UUID, UUID)](tag, "FAKE_LABWORK") with UniqueTable with LabworkIdTable {
  def * = (labwork, id)
}

class FakeLabelTable(tag: Tag) extends Table[(String, UUID)](tag, "FAKE_LABEL") with UniqueTable with LabelTable {
  def * = (label, id)
}

class FakeDateStartEndTable(tag: Tag) extends Table[(Date, Time, Time, UUID)](tag, "FAKE_DATESTARTEND") with UniqueTable with DateStartEndTable {
  def * = (date, start, end, id)
}

class FakeUserRefTable(tag: Tag) extends Table[(UUID, UUID)](tag, "FAKE_USERREF") with UniqueTable with UserIdTable {
  override protected def userColumnName: String = "FAKE_USER"

  def * = (user, id)
}

class TableFilterSpec extends PostgresDbSpec with DateGenerator {

  import dao.AbstractDaoSpec._
  import dao.helper.TableFilter._

  private val labworks_ = populateLabworks(20)(semesters, courses.take(3), degrees)

  private val schemas = List(
    TableQuery[FakeLabworkTable],
    TableQuery[FakeLabelTable],
    TableQuery[FakeDateStartEndTable],
    TableQuery[FakeUserRefTable],
  )

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    runAsyncSequence(schemas.map(_.schema.create): _*)

    runAsyncSequence(
      TableQuery[UserTable].forceInsertAll(employees),
      TableQuery[DegreeTable].forceInsertAll(degrees),
      TableQuery[SemesterTable].forceInsertAll(semesters),
      TableQuery[CourseTable].forceInsertAll(courses),
      TableQuery[LabworkTable].forceInsertAll(labworks_)
    )
  }

  override protected def afterAll(): Unit = {
    runAsyncSequence(schemas.map(_.schema.drop): _*)

    super.afterAll()
  }

  "A TableFilterSpec" should {

    "filter by course through labwork" in {
      val c = labworks_(0).course
      val ll1 = labworks_.filter(_.course == c)
      val ll2 = labworks_.filterNot(_.course == c)

      val l1 = (ll1(0).id, UUID.randomUUID)
      val l2 = (ll2(0).id, UUID.randomUUID)
      val l3 = (ll1(1).id, UUID.randomUUID)
      val l4 = (ll2(1).id, UUID.randomUUID)
      val l5 = (ll2(2).id, UUID.randomUUID)
      val entries = List(l1, l2, l3, l4, l5)

      runAsync(TableQuery[FakeLabworkTable].forceInsertAll(entries))(_ => Unit)

      runAsync(TableQuery[FakeLabworkTable].filter(courseFilter(c).apply).result) { e =>
        e.map(_._1) should contain theSameElementsAs Seq(ll1(0).id, ll1(1).id)
      }
      runAsync(TableQuery[FakeLabworkTable].filter(labworkFilter(UUID.randomUUID).apply).result)(_ shouldBe empty)
    }

    "filter by label" in {
      val entries = List(
        ("dunno", UUID.randomUUID),
        ("lorem ipsum", UUID.randomUUID),
        ("fake yo", UUID.randomUUID),
        ("some some", UUID.randomUUID),
        ("none failure", UUID.randomUUID),
        ("fake to success", UUID.randomUUID)
      )

      runAsync(TableQuery[FakeLabelTable].forceInsertAll(entries))(_ => Unit)

      runAsync(TableQuery[FakeLabelTable].filter(labelFilterEquals("dunno")).result)(_.map(_._1) shouldBe Seq("dunno"))
      runAsync(TableQuery[FakeLabelTable].filter(labelFilterEquals("fake")).result)(_.map(_._1) shouldBe empty)
      runAsync(TableQuery[FakeLabelTable].filter(labelFilterLike("fake")).result)(_.map(_._1) shouldBe Seq("fake yo", "fake to success"))
    }

    "filter by date" in {
      import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter}

      def make(y: Int, m: Int, d: Int, sh: Int, sm: Int, ss: Int, eh: Int, em: Int, es: Int): (Date, Time, Time, UUID) = {
        (localDate(y, m, d).sqlDate, localTime(sh, sm, ss, 0).sqlTime, localTime(eh, em, es, 0).sqlTime, UUID.randomUUID)
      }

      val entries = List(
        make(2010, 1, 1, 13, 0, 0, 15, 0, 0),
        make(2010, 1, 1, 15, 0, 0, 18, 0, 0),
        make(2011, 2, 5, 8, 0, 0, 18, 0, 0),
        make(2011, 7, 7, 10, 30, 0, 11, 30, 0),
        make(2012, 1, 1, 15, 0, 0, 18, 0, 0)
      )

      runAsync(TableQuery[FakeDateStartEndTable].forceInsertAll(entries))(_ => Unit)

      runAsync(TableQuery[FakeDateStartEndTable].filter(onDateFilter(localDate(2010, 1, 1).sqlDate)).result) { result =>
        result should contain theSameElementsAs entries.take(2)
      }
      runAsync(TableQuery[FakeDateStartEndTable].filter(onDateFilter(localDate(2010, 2, 1).sqlDate)).result) { result =>
        result shouldBe empty
      }
      runAsync(TableQuery[FakeDateStartEndTable].filter(sinceFilter(localDate(2010, 5, 1).sqlDate)).result) { result =>
        result should contain theSameElementsAs entries.drop(2)
      }
      runAsync(TableQuery[FakeDateStartEndTable].filter(sinceFilter(localDate(2012, 5, 1).sqlDate)).result) { result =>
        result shouldBe empty
      }
      runAsync(TableQuery[FakeDateStartEndTable].filter(untilFilter(localDate(2011, 3, 1).sqlDate)).result) { result =>
        result should contain theSameElementsAs entries.take(3)
      }
      runAsync(TableQuery[FakeDateStartEndTable].filter(untilFilter(localDate(2014, 3, 1).sqlDate)).result) { result =>
        result should contain theSameElementsAs entries
      }
      runAsync(TableQuery[FakeDateStartEndTable].filter(untilFilter(localDate(2009, 3, 1).sqlDate)).result) { result =>
        result shouldBe empty
      }
    }

    "filter by systemId through users" in {
      val users = (0 until 10).map { i =>
        UserDb(i.toString, i.toString, i.toString, i.toString, EmployeeStatus, None, None)
      }.toList

      val entries = users.map(u => (u.id, UUID.randomUUID))

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(users),
        TableQuery[FakeUserRefTable].forceInsertAll(entries)
      )

      runAsync(TableQuery[FakeUserRefTable].filter(systemIdFilter("3")).result) { result =>
        result shouldBe Seq(entries(3))
      }
      runAsync(TableQuery[FakeUserRefTable].filter(systemIdFilter("8")).result) { result =>
        result shouldBe Seq(entries(8))
      }
      runAsync(TableQuery[FakeUserRefTable].filter(systemIdFilter("other")).result) { result =>
        result shouldBe empty
      }
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected def dependencies: profile.api.DBIOAction[Unit, profile.api.NoStream, Effect.Write] = DBIO.seq()
}
