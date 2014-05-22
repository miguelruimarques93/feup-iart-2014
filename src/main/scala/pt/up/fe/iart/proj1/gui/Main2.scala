package pt.up.fe.iart.proj1.gui

import scala.collection.mutable
import scala.swing.{Component, Dimension, MainFrame, SwingApplication}
import edu.uci.ics.jung
import jung.visualization.renderers.Renderer.VertexLabel.Position
import jung.visualization.VisualizationViewer
import jung.algorithms.layout.StaticLayout
import jung.graph.{DirectedSparseGraph, Graph}


import org.apache.commons.collections15.{Transformer, Factory}
import pt.up.fe.iart.proj1.problem._
import java.awt.geom.Point2D
import pt.up.fe.iart.proj1.problem.PatientLocation
import pt.up.fe.iart.proj1.problem.GasStation
import pt.up.fe.iart.proj1.problem.GenericLocation
import pt.up.fe.iart.proj1.gui.control.{VertexRemovedEvent, EdgeRemovedEvent, VertexMoveEvent, EditingModalGraphMouse}
import edu.uci.ics.jung.visualization.control.ModalGraphMouse

object Main2 extends SwingApplication {

    implicit def FunctionToTransformer[T1, T2](fun: T1 => T2) = new Transformer[T1, T2] {
        override def transform(p1: T1): T2 = fun(p1)
    }

    implicit def FunctionToFactory[T](fun: () => T) = new Factory[T] {
        override def create(): T = fun()
    }

    val vertexMap: mutable.Map[Int, Location] = mutable.Map(
        0 -> GenericLocation((100, 100)),
        1 -> GasStation((200, 200))
    )

    val edgesMap: mutable.Map[(Int, Int), Double] = mutable.Map(
        (0, 1) -> 100.0,
        (1, 0) -> 500.0
    )

    val graph = {
        val graph = new DirectedSparseGraph[Int, (Int, Int)]

        for {ver <- vertexMap.keys} graph.addVertex(ver)
        for {edge <- edgesMap.keys; (from, to) = edge} graph.addEdge(edge, from, to)

        graph
    }

    def top = new MainFrame {
        title = "Main2"
        contents = Component.wrap {
            val layout = new StaticLayout(graph)
            val size = new Dimension(800, 600)

            layout.setInitializer((index: Int) => {
                val vertex = vertexMap(index)
                new Point2D.Double(vertex.position._1 + 100, vertex.position._2 + 100)
            })

            val vv = new VisualizationViewer(layout)
            val rc = vv.getRenderContext

            rc.setEdgeLabelTransformer((edge: (Int, Int)) => edgesMap(edge).toString)

            rc.setVertexLabelTransformer { (index: Int) => vertexMap(index) match {
                case GenericLocation(_) => ""
                case GasStation(_) => "GS"
                case PatientLocation(_, _) => "PL"
                case Filiation(_, _) => "FL"
            } }

            vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)

            val gm = new EditingModalGraphMouse(
                rc,
                p => {
                    val index = vertexMap.size
                    vertexMap.put(index, GenericLocation((p.getX, p.getY)))
                    println(s"Vertices: $vertexMap")
                    index
                },
                (from: Int, to: Int) => {
                    edgesMap.put((from, to), 0.0)
                    println(s"Edges: $edgesMap")
                    (from ,to)
                }
            )

            gm.setMode(ModalGraphMouse.Mode.TRANSFORMING)

            listenTo(gm.movePublisher, gm.popupPublisher)

            reactions += {
                case VertexMoveEvent(index: Int, newLoc: (Double, Double)) =>
                    val vertex = vertexMap(index)
                    vertex.position = newLoc
                    vertexMap.put(index, vertex)
                    println(s"Vertices: $vertexMap")

                case VertexRemovedEvent(index: Int) =>
                    vertexMap.remove(index)
                    edgesMap.retain{ case ((from, to), _) => from != index && to != index }
                    println(s"Vertices: $vertexMap")
                    println(s"Vertices: $edgesMap")

                case EdgeRemovedEvent(edge: (Int, Int)) =>
                    edgesMap.remove(edge)
                    println(s"Vertices: $vertexMap")
                    println(s"Vertices: $edgesMap")

            }
            vv.setGraphMouse(gm)
            vv.addKeyListener(gm.getModeKeyListener)
            vv.setPreferredSize(size)
            vv
        }
    }

    override def startup(args: Array[String]): Unit = {
        val t = top
        if (t.size == new Dimension(0,0)) t.pack()
        t.visible = true
    }

    override def shutdown(): Unit = {
        println(s"Vertices: $vertexMap")
        println(s"Edges: $edgesMap")
    }
}
