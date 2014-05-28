package pt.up.fe.iart.proj1.gui

import scala.collection.mutable

import Location._

sealed trait Patient {
    def destination: Option[Int] = None
    def hasDestination: Boolean = false
}

object Patient {
    def apply(): Patient = PatientWithoutDestination
    def apply(dest: Int): Patient = PatientWithDestination(dest)
}

case class PatientWithDestination(var dest: Int) extends Patient {
    override def destination: Option[Int] = Some(dest)
    override def hasDestination: Boolean = true
    override def toString = s"Patient($dest)"
}

case object PatientWithoutDestination extends Patient {
    override def toString = "Patient()"
}

abstract sealed class Location(private var _position: Position) {
    override def equals(obj: scala.Any): Boolean = obj match {
        case loc: Location => _position == loc._position
        case _ => false
    }

    def position = _position
    def position_=(p: Position) = _position = p
}

case class GenericLocation(override var position: Position) extends Location(position)
case class GasStation(override var position: Position) extends Location(position)
case class Filiation(override var position: Position, val hasGarage: Boolean) extends Location(position)
case class PatientLocation(override var position: Position, patient: Patient) extends Location(position)

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

    def hasGarage(location: Location): Boolean = location match {
        case Filiation(_, garage) => garage
        case _ => false
    }
}

class Structure {
    val vertexMap = mutable.Map[Int, Location]()
    val edgesMap = mutable.Map[(Int, Int), Double]()
    val patientsMap = mutable.Map[Int, Patient]()
}
