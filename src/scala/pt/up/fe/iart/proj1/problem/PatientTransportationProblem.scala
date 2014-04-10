package pt.up.fe.iart.proj1.problem

import pt.up.fe.iart.proj1.Problem

/**
 * Created by miguel on 10/04/2014.
 */
class PatientTransportationProblem(map: Map[Int, Location], costs: Array[Array[Option[Double]]], ambulanceCapacity: Int) extends Problem[State, Int] {

    val patientLocations = map.filter { case (_, PatientLocation(_)) => true; case _ => false }.keySet.toList
    val nonPickupLocations = map.filter { case (_, GenericLocation(_)) | (_, PatientLocation(_)) => false; case _ => true }.keySet.toList

    override def estimatedCostToGoal(from: State): Double = ???

    override def stepCost(from: State, action: Int, to: State): Double = costs(from.currentLocation)(action)

    override def result(s: State, a: Int): State = new State(
        s.currentLocation :: s.previousLocations,
        a,
        map(a) match {
            case Some(PatientLocation(_)) => s.numberPatientsAmbulance + 1
            case Some(Filiation(_, _)) => 0
            case _ => s.numberPatientsAmbulance
        },
        s.gasLevel - costs(s.currentLocation)(a)
    )

    override def actions(s: State): List[Int] = (
        {
            if (s.numberPatientsAmbulance == ambulanceCapacity)
                List()
            else
                patientLocations.diff(s.previousLocations)
        } ::: nonPickupLocations) diff List(s.currentLocation)

    override def goalTest(s: State): Boolean = {
        patientLocations.forall(s.previousLocations.contains(_)) && (s.numberPatientsAmbulance == 0)
    }

    override def initialState: State = map.find { case (n, Filiation(_, true)) => n } match {
        case Some(n) => n
        case None => throw new Error("No initial state")
    }
}
