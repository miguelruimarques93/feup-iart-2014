package pt.up.fe.iart.proj1.problem

object Location {
    type Position = (Int, Int)
}
import Location.Position

abstract sealed class Location()

case class GenericLocation(val position : Position) extends Location()

case class GasStation(val position : Position) extends Location()

case class PatientLocation(val position : Position) extends Location()

case class Filiation(val position : Position, val hasGarage: Boolean) extends Location()
