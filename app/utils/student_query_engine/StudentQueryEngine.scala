package utils.student_query_engine

import dao.UserDao
import database.{LabworkApplicationTable, ReportCardEntryTable, ReportCardEntryTypeTable, UserTable}
import slick.jdbc.JdbcProfile

import java.util.UUID
import javax.inject.{Inject, Singleton}

@Singleton
final class StudentQueryEngine @Inject()(val profile: JdbcProfile) {

  import Expression._
  import profile.api._

  def query(expr: Expression): UserTable => Rep[Boolean] = {
    var ctx = Set.empty[Context]

    def go(expr: Expression): UserTable => Rep[Boolean] = expr match {
      case Single(key, value) =>
        key match {
          case Key.Degee =>
            degree(value)
          case Key.Lastname =>
            lastName(value)
          case Key.PassedAssignment =>
            assignment(value, shouldPass = true, ctx)
          case Key.FailedAssignment =>
            assignment(value, shouldPass = false, ctx)
          case Key.Labwork =>
            val labworkId = UUID.fromString(value)
            ctx = ctx + Context.Labwork(labworkId)
            labwork(labworkId)
          case Key.Course =>
            val courseId = UUID.fromString(value)
            ctx = ctx + Context.Course(courseId)
            course(courseId)
          case Key.Semester =>
            val semesterId = UUID.fromString(value)
            ctx = ctx + Context.Semester(semesterId)
            semester(semesterId)
        }
      case Combined(lhs, rhs, operator) =>
        ctx = Set.empty

        user => {
          val a = go(lhs)(user)
          val b = go(rhs)(user)

          operator match {
            case Operator.And => a && b
            case Operator.Or => a || b
          }
        }
    }

    go(expr)
  }

  private def lastName(name: String): UserTable => Rep[Boolean] =
    user => UserDao.lastnameFilter(name)(user)

  private def semester(semesterId: UUID): UserTable => Rep[Boolean] =
    table => {
      val q = TableQuery[LabworkApplicationTable].filter(_.inSemester(semesterId)).map(_.user)
      table.id.in(q)
    }

  private def course(courseId: UUID): UserTable => Rep[Boolean] =
    table => {
      val q = TableQuery[LabworkApplicationTable].filter(_.memberOfCourse(courseId)).map(_.user)
      table.id.in(q)
    }

  private def degree(value: String): UserTable => Rep[Boolean] =
    user => UserDao.enrollmentFilter(UUID.fromString(value))(user)

  private def labwork(labworkId: UUID): UserTable => Rep[Boolean] =
    user => {
      val q = TableQuery[LabworkApplicationTable].filter(_.labwork === labworkId).map(_.user)
      user.id.in(q)
    }

  private def assignment(label: String, shouldPass: Boolean, ctx: Set[Context]): UserTable => Rep[Boolean] =
    user => {
      val inContext = ctx.foldLeft[ReportCardEntryTable => Rep[Boolean]](_ => true) {
        case (acc, Context.Course(id)) => table => acc(table) && table.memberOfCourse(id)
        case (acc, Context.Semester(id)) => table => acc(table) && table.inSemester(id)
        case (acc, Context.Labwork(id)) => table => acc(table) && table.labwork === id
      }

      val q = TableQuery[ReportCardEntryTable]
        .filter(t => inContext(t) && t.label === label)
        .joinLeft(TableQuery[ReportCardEntryTypeTable])
        .on(_.id === _.reportCardEntry)
        .filter(t => t._2.map(t => t.entryType === "Testat" && (if (shouldPass) t.bool.getOrElse(false) else !t.bool.getOrElse(false))).getOrElse(false))
        .map(t => t._1.user)

      user.id.in(q)
    }
}

object StudentQueryEngine {

  def pretty(expr: Expression): String = expr match {
    case Expression.Single(key, value) =>
      s"($key: $value)"
    case Expression.Combined(lhs, rhs, operator) =>
      val op = operator match {
        case Operator.And => "and"
        case Operator.Or => "or"
      }

      s"${pretty(lhs)} $op ${pretty(rhs)}"
  }
}