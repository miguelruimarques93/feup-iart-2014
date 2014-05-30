package pt.up.fe.iart.proj1.problem

import pt.up.fe.iart.proj1.problem

import pt.up.fe.iart.proj1.problem.Location._

import scala.collection.immutable.HashSet
import scala.collection.GenSeq
import pt.up.fe.iart.proj1.collections.Graph
import java.io.FileInputStream
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import pt.up.fe.iart.proj1.parser.{GraphVisitor, PTPParser, PTPLexer}
import pt.up.fe.iart.proj1.solver.Problem

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

    val estimatedCostToPickupMap = {
        for (pl <- patientLocations) yield pl -> {
            for ((row, from) <- doubleCosts.zipWithIndex if from != pl) yield row(pl)
        }.min
    }.toMap

    private def distinctBy[A, B](fun: A => B, list: List[A]): List[A] = {
        val b = collection.mutable.ArrayBuffer[A]()
        val seen = collection.mutable.HashSet[B]()
        for (x <- list) {
            val y = fun(x)
            if (!seen(y)) {
                b += x
                seen += y
            }
        }
        b.to[List]
    }

    override def estimatedCostToGoal(from: State) = {
        val inAmbulance: List[Patient] = from.patientsAmbulance
        val all: List[Patient] = patientLocations.map(pl => Location.patient(map(pl))).flatMap(op => if (op.isDefined) List(op.get) else List()).to[List]

        val e = estimatedCostToPickupMap.filterKeys { k => from.currentLocation != k && !from.previousLocations.contains(k)}.foldLeft(0.0)(_ + _._2) +
            distinctBy(
                (p: Patient) => mapIndices(patientDestination(p)),
                patientLocations.diff(from.previousLocations + from.currentLocation).flatMap(pl => Location.patient(map(pl)) match {
                    case Some(p) => List(p);
                    case None => List()
                }).to[List] ++ from.patientsAmbulance
            ).map(estimatedCostToDeliverMap).sum


        // val e1 = from.patientsAmbulance.map(p => mapIndices(patientDestination(p))).distinct.map(doubleCosts(from.currentLocation)).sum

        //val e1 = distinctBy((p: Patient) => mapIndices(patientDestination(p)), from.patientsAmbulance).map(estimatedCostToDeliverMap).sum

        e
    }

    def printEstimatedCostToGoal(from: State) = {
        println(from)
        val e = estimatedCostToPickupMap.filterKeys { k => from.currentLocation != k && !from.previousLocations.contains(k)}.foldLeft(0.0)(_ + _._2)
        val e2 = distinctBy(
            (p: Patient) => mapIndices(patientDestination(p)),
            patientLocations.diff(from.previousLocations + from.currentLocation).flatMap(pl => Location.patient(map(pl)) match {
                case Some(p) => List(p);
                case None => List()
            }).to[List] ++ from.patientsAmbulance
        ).map(estimatedCostToDeliverMap).sum
        println("estimatedCostToPickupMap: " + e)
        println("estimatedCostToDeliver: " + e2)
        println("total: " + (e + e2))
        println()
    }

    override def stepCost(from: State, action: Int, to: State): Double = doubleCosts(from.currentLocation)(action)

    override def result(s: State, a: Int): State = new State(
        s.previousLocations + s.currentLocation,
        a,
        map(a) match {
            case PatientLocation(_, p) => p :: s.patientsAmbulance
            case f@Filiation(_, _) => s.patientsAmbulance.filterNot(p => p.destination.isEmpty || p.destination.exists(_ == f))
            case _ => s.patientsAmbulance
        },
        map(a) match {
            case GasStation(_) => maxGasLevel
            case _ => s.gasLevel - doubleCosts(s.currentLocation)(a)
        }
    )

    override def actions(s: State): List[Int] = ((
        (if (s.numberPatientsAmbulance < ambulanceCapacity) patientLocations -- s.previousLocations else HashSet.empty[Int]) ++
            (if (s.numberPatientsAmbulance > 0) {
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

    override def goalTest(s: State): Boolean = s.numberPatientsAmbulance == 0 && patientLocations.forall(s.previousLocations.contains)

    override def initialState: State = map.find { case (index, loc) => Location.isFiliation(loc) && Location.hasGarage(loc)} match {
        case Some((index, _)) => new State(HashSet.empty, index, List.empty, maxGasLevel)
        case None => throw new Error("No initial state")
    }
}


object PatientTransportationProblem {
    def readGraph(fileName: String): Graph[problem.Location] = {
        val inputFile = fileName

        val is = new FileInputStream(inputFile)
        val input = new ANTLRInputStream(is)
        val lexer = new PTPLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new PTPParser(tokens)
        val tree = parser.map()

        val visitor = new GraphVisitor()
        visitor.visit(tree)
    }
}
