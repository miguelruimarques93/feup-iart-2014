package pt.up.fe.iart.proj1.problem

import pt.up.fe.iart.proj1.Problem
import scala.collection.immutable.HashSet
import scala.collection.GenSeq

class PatientTransportationProblem(map: Map[Int, Location], costs: Array[Array[Option[Double]]], ambulanceCapacity: Int, maxGasLevel: Double) extends Problem[State, Int] {

    val patientLocations = map.filter { case (_, PatientLocation(_)) => true; case _ => false }.keySet.to[HashSet]
    val filiations = map.filter { case (_, Filiation(_,_)) => true; case _ => false }.keySet.to[HashSet]
    val gasStations = map.filter { case (_, GasStation(_)) => true; case _ => false }.keySet.to[HashSet]
    val nonPickupLocations = map.filter { case (_, GenericLocation(_)) | (_, PatientLocation(_)) => false; case _ => true }.keySet.to[HashSet]
    val doubleCosts = costs.map(_.map{case Some(d) => d; case None => Double.MaxValue})

    val estimatedCostMap = {
        for (pl <- patientLocations) yield pl -> {
            for ((row, from) <- doubleCosts.zipWithIndex if from != pl) yield row(pl)
        }.min
    }.toMap

    override def estimatedCostToGoal(from: State): Double = {
        estimatedCostMap.filterKeys{k => from.currentLocation != k && !from.previousLocations.contains(k)}.foldLeft(0.0)(_ + _._2)
    }

    override def stepCost(from: State, action: Int, to: State): Double = doubleCosts(from.currentLocation)(action)

    override def result(s: State, a: Int): State = new State(
        s.previousLocations + s.currentLocation,
        a,
        map(a) match {
            case PatientLocation(_) => s.numberPatientsAmbulance + 1
            case Filiation(_, _) => 0
            case _ => s.numberPatientsAmbulance
        },
        map(a) match {
            case GasStation(_) => maxGasLevel
            case _ => s.gasLevel - doubleCosts(s.currentLocation)(a)
        }
    )

    override def actions(s: State): List[Int] = ((
        (if (s.numberPatientsAmbulance < ambulanceCapacity) patientLocations -- s.previousLocations else HashSet.empty[Int])
        ++ (if (s.numberPatientsAmbulance > 0) filiations else HashSet.empty[Int])
        ++ (if (s.gasLevel < maxGasLevel) gasStations else HashSet.empty[Int])
        ) - s.currentLocation).filter { index => doubleCosts(s.currentLocation)(index) <= s.gasLevel }.toList

    override def goalTest(s: State): Boolean = {
        patientLocations.forall(s.previousLocations.contains(_)) && (s.numberPatientsAmbulance == 0)
    }

    override def initialState: State = map.find { case (n, Filiation(_, true)) => true; case _ => false } match {
        case Some((index, _)) => new State(HashSet.empty, index, 0, maxGasLevel)
        case None => throw new Error("No initial state")
    }
}
