package pt.up.fe.iart.proj1.gui

import pt.up.fe.iart.proj1.collections.Graph

import edu.uci.ics.jung.graph.DirectedSparseMultigraph

object GraphToJungGraph {
    implicit def graphToJungGraph[V](g: Graph[V]) = {
        val graph = new DirectedSparseMultigraph[V, Graph[V]#Edge]

        for { ver <- g.vertices } graph.addVertex(ver)
        for { edge <- g.edges } graph.addEdge(edge, edge.from, edge.to)

        graph
    }
}
