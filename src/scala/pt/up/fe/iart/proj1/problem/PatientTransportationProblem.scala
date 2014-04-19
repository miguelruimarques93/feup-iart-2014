package pt.up.fe.iart.proj1.problem

import pt.up.fe.iart.proj1.Problem
import scala.collection.immutable.HashSet
import scala.collection.GenSeq

class PatientTransportationProblem(map: Map[Int, Location], costs: Array[Array[Option[Double]]], ambulanceCapacity: Int, maxGasLevel: Double) extends Problem[State, Int] {

    val patientLocations = map.filter { case (_, PatientLocation(_,_)) => true; case _ => false }.keySet.to[HashSet]
    val filiations = map.filter { case (_, Filiation(_,_)) => true; case _ => false }.keySet.to[HashSet]
    val gasStations = map.filter { case (_, GasStation(_)) => true; case _ => false }.keySet.to[HashSet]
    val nonPickupLocations = map.filter { case (_, GenericLocation(_)) | (_, PatientLocation(_, _)) => false; case _ => true }.keySet.to[HashSet]
    val doubleCosts = costs.map(_.map{case Some(d) => d; case None => Double.MaxValue})

    val estimatedCostToDeliverMap = {
        val filiationsMap = map.filterKeys(filiations.contains(_)).map(_.swap)
        for (pl <- patientLocations) yield {
            val patient = map(pl) match { case PatientLocation(_, p) => p }
            patient -> doubleCosts(pl)(filiationsMap(patient.destination))
        }
    }.toMap

    val estimatedCostToPickupAndDeliverMap = {
        for (pl <- patientLocations) yield pl -> ({
            for ((row, from) <- doubleCosts.zipWithIndex if from != pl) yield row(pl)
        }.min + (map(pl) match { case PatientLocation(_, patient) => estimatedCostToDeliverMap(patient) case _ => 0 }))
    }.toMap

    override def estimatedCostToGoal(from: State): Double = {
        estimatedCostToPickupAndDeliverMap.filterKeys{k => from.currentLocation != k && !from.previousLocations.contains(k)}.foldLeft(0.0)(_ + _._2)
        + from.patientsAmbulance.map(estimatedCostToDeliverMap).sum
    }

    override def stepCost(from: State, action: Int, to: State): Double = doubleCosts(from.currentLocation)(action)

    override def result(s: State, a: Int): State = new State(
        s.previousLocations + s.currentLocation,
        a,
        map(a) match {
            case PatientLocation(_,p) => p :: s.patientsAmbulance
            case f: Filiation => s.patientsAmbulance.filterNot(_.destination == f)
            case _ => s.patientsAmbulance
        },
        map(a) match {
            case GasStation(_) => maxGasLevel
            case _ => s.gasLevel - doubleCosts(s.currentLocation)(a)
        }
    )

    override def actions(s: State): List[Int] = ((
        (if (s.numberPatientsAmbulance < ambulanceCapacity) patientLocations -- s.previousLocations else HashSet.empty[Int])
        ++ (if (s.numberPatientsAmbulance > 0) filiations.filter(x => { val f = map(x); s.patientsAmbulance.exists(p => p.destination == f) }) else HashSet.empty[Int])
        ++ (if (s.gasLevel < maxGasLevel) gasStations else HashSet.empty[Int])
        ) - s.currentLocation).filter { index => doubleCosts(s.currentLocation)(index) <= s.gasLevel }.toList

    override def goalTest(s: State): Boolean = {
        patientLocations.forall(s.previousLocations.contains(_)) && (s.numberPatientsAmbulance == 0)
    }

    override def initialState: State = map.find { case (n, Filiation(_, true)) => true; case _ => false } match {
        case Some((index, _)) => new State(HashSet.empty, index, List.empty, maxGasLevel)
        case None => throw new Error("No initial state")
    }
}
