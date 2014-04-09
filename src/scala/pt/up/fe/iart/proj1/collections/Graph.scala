package pt.up.fe.iart.proj1.collections

import collection.mutable
import scala.collection.mutable.ArrayBuffer

class Graph[V] {
    private var _lastVertex = -1
    private val vertices = mutable.Map[V, Int]().withDefaultValue(-1);
    private val _adjacencyMatrix = mutable.Map[Int, mutable.Map[Int, Double]]() withDefaultValue mutable.Map.empty

    def addVertex(value: V) = {
        if (! vertices.contains(value)) {
            _lastVertex += 1
            vertices.put(value, _lastVertex)
            _adjacencyMatrix.put(_lastVertex, mutable.Map())
        }
    }

    def removeVertex(value: V) = {
        if (vertices.contains(value)) {
            val index = vertices(value)
            vertices.remove(value)
            _adjacencyMatrix.remove(index)
            for { (_, edges) <- _adjacencyMatrix } edges.remove(index)
        }
    }

    def addEdge(from: V, to: V, weight: Double): Boolean = _setEdge(vertices(from), vertices(to), weight)

    private def _setEdge(fromIndex: Int, toIndex: Int, weight: Double): Boolean = _adjacencyMatrix.get(fromIndex) match {
        case Some(row) =>
            row.update(toIndex, weight)
            true
        case None => false
    }

    override def toString = s"${vertices}\n${_adjacencyMatrix}"

    def weightMatrix : Array[Array[Option[Double]]] = {
        val result = Array.fill[Option[Double]](_lastVertex + 1, _lastVertex + 1)(Some(0.0))
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

    implicit def optionToDouble(weight:Option[Double]) = weight match{
        case Some(x) => x
        case _ => Double.MaxValue
    }

    implicit def doubleToOption(weight:Double) = Some(weight)

    def getShortestPaths = {
        val W = weightMatrix
        val P = predecessorMatrix
        val n = _lastVertex + 1

        for {k <- 0 until n
             i <- 0 until n
             j <- 0 until n
             newWeight = W(i)(k) + W(k)(j)
             if W(i)(j) > newWeight } {
            W(i)(j) = W(i)(k) + W(k)(j)
            P(i)(j) = P(k)(j)
        }

        (W, P)
    }
}

object Main {
    def main (args: Array[String]) {
        val g = new Graph[String]()
        g addVertex "Hello"
        g addVertex "World"

        println(g.addEdge("Hello", "World", 20))
        println(g.addEdge("World", "Hello", 10))

        //g removeVertex "Hello"

        g.getShortestPaths match {
            case (weights, predecessors) =>
                println(weights.deep.mkString("\n") + "\n")
                println(predecessors.deep.mkString("\n"))
        }
    }
}
