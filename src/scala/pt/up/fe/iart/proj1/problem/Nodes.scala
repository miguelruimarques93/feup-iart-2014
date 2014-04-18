package pt.up.fe.iart.proj1.problem

case class Patient(val destination: Filiation)

object Location {
    type Position = (Int, Int)
}
import pt.up.fe.iart.proj1.problem.Location.Position

abstract sealed class Location(val pos: Position) {
    override def equals(obj: scala.Any): Boolean = obj match {
        case loc: Location => pos == loc.pos
        case _ => false
    }
}

case class GenericLocation(position : Position) extends Location(position)

case class GasStation(position : Position) extends Location(position)

case class PatientLocation(position : Position, patient: Patient) extends Location(position)

case class Filiation(position : Position, hasGarage: Boolean) extends Location(position)

