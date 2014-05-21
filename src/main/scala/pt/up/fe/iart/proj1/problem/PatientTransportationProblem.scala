package pt.up.fe.iart.proj1.problem

import pt.up.fe.iart.proj1.Problem

import pt.up.fe.iart.proj1.problem.Location._

import scala.collection.immutable.HashSet
import scala.collection.GenSeq

class PatientTransportationProblem(map: Map[Int, Location], costs: Array[Array[Option[Double]]], ambulanceCapacity: Int, maxGasLevel: Double) extends Problem[State, Int] {

    val patientLocations = map.filter(x => isPatientLocation(x._2)).keySet.to[HashSet]
    val filiations = map.filter(x => isFiliation(x._2)).keySet.to[HashSet]
    val gasStations = map.filter(x => isGasStation(x._2)).keySet.to[HashSet]
    val nonPickupLocations = map.filter(x => isGenericLocation(x._2)).keySet.to[HashSet]
    val doubleCosts = for (row <- costs) yield for (value <- row) yield value.getOrElse(Double.MaxValue)

    val mapIndices: Map[Location, Int] = map.map(_.swap)

    private def nearestFiliation(locIndex: Int): (Location, Double) = {
        for (fl <- filiations) yield map(fl) -> doubleCosts(locIndex)(fl)
    }.minBy(_._2)

    private def nearestFiliation(location: Location): (Location, Double) = nearestFiliation(mapIndices(location))


    private def patientLocation(p: Patient) = map.values.find(loc => patient(loc).exists(_ == p))

    val estimatedCostToDeliverMap: Map[Patient, Double] = {
        for (pl <- patientLocations) yield {
            val patient = Location.patient(map(pl)).get
            patient -> doubleCosts(pl)(mapIndices(patient.destination.getOrElse(nearestFiliation(pl)._1)))
        }
    }.toMap

    def patientDestination(p: Patient): Location = {
        val pl = patientLocation(p).get
        p.destination.getOrElse(nearestFiliation(pl)._1)
    }

    val estimatedCostToPickupAndDeliverMap = {
        for (pl <- patientLocations) yield pl -> ({
            for ((row, from) <- doubleCosts.zipWithIndex if from != pl) yield row(pl)
        }.min + (map(pl) match {
            case PatientLocation(_, patient) => estimatedCostToDeliverMap(patient)
            case _ => 0
        }))
    }.toMap

    override def estimatedCostToGoal(from: State) = {
        estimatedCostToPickupAndDeliverMap.filterKeys { k => from.currentLocation != k && !from.previousLocations.contains(k)}.foldLeft(0.0)(_ + _._2)
        +from.patientsAmbulance.map(p => mapIndices(patientDestination(p))).distinct.map(doubleCosts(from.currentLocation)).sum
    }

    override def stepCost(from: State, action: Int, to: State): Double = doubleCosts(from.currentLocation)(action)

    override def result(s: State, a: Int): State = new State(
        s.previousLocations + s.currentLocation,
        a,
        map(a) match {
            case PatientLocation(_, p) => p :: s.patientsAmbulance
            case f @ Filiation(_, _) => s.patientsAmbulance.filterNot(p => p.destination.isEmpty || p.destination.exists(_ == f))
            case _ => s.patientsAmbulance
        },
        map(a) match {
            case GasStation(_) => maxGasLevel
            case _ => s.gasLevel - doubleCosts(s.currentLocation)(a)
        }
    )

    override def actions(s: State): List[Int] = ((
        (if (s.numberPatientsAmbulance < ambulanceCapacity) patientLocations -- s.previousLocations else HashSet.empty[Int])
            ++ (if (s.numberPatientsAmbulance > 0) {
            if (s.patientsAmbulance.exists(p => p.destination.isEmpty))
                filiations
            else
                filiations.filter(x => {
                    val f = map(x)
                    s.patientsAmbulance.exists(p => p.destination.exists(_ == f))
                })
        }
        else
            HashSet.empty[Int])
            ++ (if (s.gasLevel < maxGasLevel) gasStations else HashSet.empty[Int])
        ) - s.currentLocation).filter { index => doubleCosts(s.currentLocation)(index) <= s.gasLevel}.toList

    override def goalTest(s: State): Boolean = {
        patientLocations.forall(s.previousLocations.contains) && (s.numberPatientsAmbulance == 0)
    }

    override def initialState: State = map.find { case (n, Filiation(_, true)) => true; case _ => false} match {
        case Some((index, _)) => new State(HashSet.empty, index, List.empty, maxGasLevel)
        case None => throw new Error("No initial state")
    }
}
