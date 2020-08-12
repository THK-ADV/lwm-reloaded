package utils.student_query_engine

import java.util.UUID

import dao.UserDao
import database.{LabworkApplicationTable, ReportCardEntryTable, ReportCardEntryTypeTable, UserTable}
import javax.inject.{Inject, Singleton}
import slick.jdbc.JdbcProfile

@Singleton
final class StudentQueryEngine @Inject()(val profile: JdbcProfile) {

  import profile.api._

  def makeFilter(expr: Expression): UserTable => Rep[Boolean] = {
    import Expression._

    var ctx = Set.empty[Context]

    def go(expr: Expression): UserTable => Rep[Boolean] = {
      expr match {
        case Single(key, value) =>
          key match {
            case Key.Degee => table =>
              UserDao.enrollmentFilter(UUID.fromString(value))(table)
            case Key.Labwork =>
              val labworkId = UUID.fromString(value)
              ctx = ctx + Context.Labwork(labworkId)

              table => {
                val q = TableQuery[LabworkApplicationTable].filter(t => t.labwork === labworkId).map(_.user)
                table.id.in(q)
              }
            case Key.PassedAssignment => queryAssignment(value, shouldPass = true, ctx)
            case Key.FailedAssignment => queryAssignment(value, shouldPass = false, ctx)
            case Key.Course =>
              val courseId = UUID.fromString(value)
              ctx = ctx + Context.Course(courseId)

              table => {
                val q = TableQuery[LabworkApplicationTable].filter(t => t.memberOfCourse(courseId)).map(_.user)
                table.id.in(q)
              }
            case Key.Semester =>
              val semesterId = UUID.fromString(value)
              ctx = ctx + Context.Semester(semesterId)

              table => {
                val q = TableQuery[LabworkApplicationTable].filter(t => t.inSemester(semesterId)).map(_.user)
                table.id.in(q)
              }
            case Key.Lastname => table => UserDao.lastnameFilter(value)(table)
          }
        case Combined(lhs, rhs, operator) =>
          ctx = Set.empty

          table => {
            val a = go(lhs)(table)
            val b = go(rhs)(table)

            operator match {
              case Operator.And => a && b
              case Operator.Or => a || b
            }
          }
      }
    }

    go(expr)
  }

  private def queryAssignment(label: String, shouldPass: Boolean, ctx: Set[Context]): UserTable => Rep[Boolean] = table => {
    val initQ = ctx.foldLeft[ReportCardEntryTable => Rep[Boolean]](_ => true) {
      case (acc, Context.Course(id)) => table => acc(table) && table.memberOfCourse(id)
      case (acc, Context.Semester(id)) => table => acc(table) && table.inSemester(id)
      case (acc, Context.Labwork(id)) => table => acc(table) && table.labwork === id
    }

    val q = TableQuery[ReportCardEntryTable]
      .filter(t => initQ(t) && t.label === label)
      .joinLeft(TableQuery[ReportCardEntryTypeTable])
      .on(_.id === _.reportCardEntry)
      .filter(t => t._2.map(t => t.entryType === "Testat" && (if (shouldPass) t.bool.getOrElse(false) else !t.bool.getOrElse(false))).getOrElse(false))
      .map(t => t._1.user)

    table.id.in(q)
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