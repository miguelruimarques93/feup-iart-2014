package pt.up.fe.iart.proj1.problem

class State(val previousLocations: List[Int], val currentLocation: Int, val numberPatientsAmbulance: Int, val gasLevel: Double) {
    override def toString = s"{ $currentLocation, $previousLocations, $numberPatientsAmbulance, $gasLevel }"
}
