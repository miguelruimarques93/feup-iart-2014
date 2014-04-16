package pt.up.fe.iart.proj1.collections

import scala.collection.mutable
import pt.up.fe.iart.proj1.problem._
import scala.Some
import pt.up.fe.iart.proj1._
import java.io.FileInputStream
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import pt.up.fe.iart.proj1.parser.{GraphVisitor, PTPParser, PTPLexer}
import scala.Some
import pt.up.fe.iart.proj1.Success

class Graph[V <: Any] {
    private var _lastVertex = -1
    private val _vertices = mutable.Map[V, Int]().withDefaultValue(-1)
    private val _adjacencyMatrix = mutable.Map[Int, mutable.Map[Int, Double]]() withDefaultValue mutable.Map.empty

    def vertices = _vertices.keySet
    def verticesMap = _vertices.toMap

    def addVertex(value: V) = {
        if (! _vertices.contains(value)) {
            _lastVertex += 1
            _vertices.put(value, _lastVertex)
            _adjacencyMatrix.put(_lastVertex, mutable.Map())
        }
    }

    def removeVertex(value: V) {
        if (_vertices.contains(value)) {
            val index = _vertices(value)
            _vertices.remove(value)
            _adjacencyMatrix.remove(index)
            for { (_, edges) <- _adjacencyMatrix } edges.remove(index)
        }
    }

    def addEdge(from: V, to: V, weight: Double): Boolean = _setEdge(_vertices(from), _vertices(to), weight)

    private def _setEdge(fromIndex: Int, toIndex: Int, weight: Double): Boolean = _adjacencyMatrix.get(fromIndex) match {
        case Some(row) =>
            row.update(toIndex, weight)
            true
        case None => false
    }

    case class Edge(from: V, to: V, weight: Double)
    def edges: List[Edge] = {
        val invertVertices = _vertices.map{_.swap}
        for { (from, row) <- _adjacencyMatrix
              (to, weight) <- row } yield Edge(invertVertices(from), invertVertices(to), weight)
    }.toList

    override def toString = s"${_vertices.keys.mkString("\n")}\n${edges.mkString("\n")}"

    def weightMatrix : Array[Array[Option[Double]]] = {
        val result = Array.fill[Option[Double]](_lastVertex + 1, _lastVertex + 1)(None)

        for { i <- 0 to _lastVertex } result(i)(i) = Some(0.0)

        for { (src, edges) <- _adjacencyMatrix
              (destination, weight) <- edges }
            result(src)(destination) = Some(weight)
        result
    }

    def predecessorMatrix : Array[Array[Option[Int]]] = {
        val result = Array.fill[Option[Int]](_lastVertex + 1, _lastVertex + 1)(None)
        for { (src, edges) <- _adjacencyMatrix
              (destination, _) <- edges }
            result(src)(destination) = Some(src)
        result
    }

    import scala.language.implicitConversions

    implicit def optionToDouble(weight:Option[Double]) = weight match{
        case Some(x) => x
        case _ => Double.MaxValue
    }

    implicit def doubleToOption(weight:Double) = Some(weight)

    def getShortestPaths: (Array[Array[Option[Double]]], Array[Array[Option[Int]]]) = {
        val W = weightMatrix
        val P = predecessorMatrix
        val n = _lastVertex + 1

        for {k <- 0 until n
             i <- 0 until n
             j <- 0 until n
             newWeight = W(i)(k) + W(k)(j)
             if W(i)(j) > newWeight } {
            W(i)(j) = newWeight
            P(i)(j) = P(k)(j)
        }

        (W, P)
    }
}

object Main {
    def main (args: Array[String]) {
        val g = new Graph[Location]()
        g addVertex GasStation((0, 0))
        g addVertex Filiation((1, 0), true)
        g addVertex GenericLocation((2, 0))

        g.addEdge(GasStation((0, 0)), Filiation((1, 0), true), 20)
        g.addEdge(Filiation((1, 0), true), GasStation((0, 0)), 10)
        g.addEdge(GasStation((0, 0)), GenericLocation((2, 0)), 5)
        g.addEdge(GenericLocation((2, 0)), Filiation((1, 0), true), 5)

        val vs = g.verticesMap.filter {
            case (GenericLocation(_), _) => false
            case _ => true
        }

        println(vs.mkString(", ") + "\n")

        g.getShortestPaths match {
            case (weights, predecessors) =>
                println(weights.deep.mkString("\n") + "\n")
                println(predecessors.deep.mkString("\n"))
        }
    }
}
