package pt.up.fe.iart.proj1.problem

object Location {
    type Position = (Int, Int)
}
import pt.up.fe.iart.proj1.problem.Location.Position

abstract sealed class Location()

case class GenericLocation(position : Position) extends Location()

case class GasStation(position : Position) extends Location()

case class PatientLocation(position : Position) extends Location()

case class Filiation(position : Position, hasGarage: Boolean) extends Location()

