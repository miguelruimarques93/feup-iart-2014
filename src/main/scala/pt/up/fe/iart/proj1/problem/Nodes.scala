package pt.up.fe.iart.proj1.problem

sealed trait Patient {
    def destination: Option[Location] = None
}

case class PatientWithDestination(dest: Filiation) extends Patient {
    override def destination: Option[Location] = Some(dest)
    override def toString = s"Patient($dest)"
}

case class PatientWithoutDestination() extends Patient {
    override def toString = "Patient()"
}

import pt.up.fe.iart.proj1.problem.Location.Position

abstract sealed class Location(private var _position: Position) {
    override def equals(obj: scala.Any): Boolean = obj match {
        case loc: Location => _position == loc._position
        case _ => false
    }

    def position = _position
    def position_=(p: Position) = _position = p
}

case class GenericLocation(override var position : Position) extends Location(position)

case class GasStation(override var position : Position) extends Location(position)

case class PatientLocation(override var position : Position, patient: Patient) extends Location(position)

case class Filiation(override var position : Position, hasGarage: Boolean) extends Location(position)

object Location {
    type Position = (Double, Double)

    def patient(location: Location) = location match {
        case PatientLocation(_, p) => Some(p)
        case _ => None
    }

    def isPatientLocation(location: Location) = location match {
        case PatientLocation(_, _) => true
        case _ => false
    }

    def isGasStation(location: Location) = location match {
        case GasStation(_) => true
        case _ => false
    }

    def isFiliation(location: Location) = location match {
        case Filiation(_, _) => true
        case _ => false
    }

    def isGenericLocation(location: Location) = location match {
        case GenericLocation(_) => true
        case _ => false
    }
}
