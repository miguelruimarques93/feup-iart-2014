package pt.up.fe.iart.proj1.problem

import scala.collection.immutable.HashSet

class State(val previousLocations: HashSet[Int], val currentLocation: Int, val numberPatientsAmbulance: Int, val gasLevel: Double) {
    override def toString = s"{ $currentLocation, $previousLocations, $numberPatientsAmbulance, $gasLevel }"
}
