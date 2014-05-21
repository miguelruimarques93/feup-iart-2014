package pt.up.fe.iart.proj1.problem

import scala.collection.immutable.HashSet

class State(val previousLocations: HashSet[Int], val currentLocation: Int, val patientsAmbulance: List[Patient], val gasLevel: Double) {
    def numberPatientsAmbulance = patientsAmbulance.size

    override def toString = s"{ $currentLocation, $previousLocations, $patientsAmbulance, $gasLevel }"
}
