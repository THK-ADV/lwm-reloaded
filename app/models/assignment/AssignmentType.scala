package models.assignment

sealed trait AssignmentType

object AssignmentType {

  object Attendance extends AssignmentType

  object Bonus extends AssignmentType

  object SimpleCertificate extends AssignmentType

  object DetailedCertificate extends AssignmentType

  object NumberedCertificate extends AssignmentType

  def apply(name: String): Option[AssignmentType] = name match {
    case "attendance" => Some(Attendance)
    case "bonus" => Some(Bonus)
    case "cert_simple" => Some(SimpleCertificate)
    case "cert_detailed" => Some(DetailedCertificate)
    case "cert_numbered" => Some(NumberedCertificate)
    case _ => None
  }

  def identifiers(): List[String] = types map (identifier)

  def identifier(t: AssignmentType): String = t match {
    case AssignmentType.Attendance => "attendance"
    case AssignmentType.Bonus => "bonus"
    case AssignmentType.SimpleCertificate => "cert_simple"
    case AssignmentType.DetailedCertificate => "cert_detailed"
    case AssignmentType.NumberedCertificate => "cert_numbered"
  }

  def types(): List[AssignmentType] = List(
    Attendance,
    Bonus,
    SimpleCertificate,
    DetailedCertificate,
    NumberedCertificate
  )
}