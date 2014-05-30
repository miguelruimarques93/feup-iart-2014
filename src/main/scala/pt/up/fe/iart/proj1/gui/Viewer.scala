package pt.up.fe.iart.proj1.gui

import pt.up.fe.iart.proj1
import proj1.gui.Conversions._
import proj1.problem
import scala.swing._

import proj1.collections.Graph

import edu.uci.ics.jung
import jung.graph.DirectedSparseGraph
import jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import java.awt.geom.Point2D
import java.awt.{BasicStroke, Color}
import javax.swing.{JRootPane, WindowConstants}
import scala.collection.Set

class Viewer(g: Graph[problem.Location], rawSolution: List[(problem.Location, List[problem.Patient], Double)]) extends Dialog {
    private val verticesMap = g.verticesMap

    val solution = rawSolution.map {
        case (loc, ptAmb, gLevel) => (verticesMap(loc), ptAmb, gLevel)
    }

    val vertexMap: Map[Int, Map[Int, Location]] = {

        val tempVertexMap = verticesMap.map(_.swap)

        val nonPatientLocationMap = for {
            (index, loc) <- tempVertexMap if !problem.Location.isPatientLocation(loc)
        } yield {
            index -> Map(0 -> {
                (loc: @unchecked) match {
                    case gl@problem.GenericLocation(_) => GenericLocation(gl.position)
                    case gs@problem.GasStation(_) => GasStation(gs.position)
                    case problem.Filiation(pos, hasGarage) => Filiation(pos, hasGarage)
                }
            })
        }

        nonPatientLocationMap ++ (for {
            (index, loc) <- tempVertexMap if problem.Location.isPatientLocation(loc)
        } yield {
            index -> Map(0 -> ((loc: @unchecked) match {
                case pl@problem.PatientLocation(pos, _) => PatientLocation(pos, pl.patient.destination match {
                    case Some(dest) => Patient(verticesMap(dest))
                    case None => Patient()
                }

                )
            }),
            {
                val patient = problem.Location.patient(loc).get
                solution.indexWhere {
                    case (_, ptAmb, _) => ptAmb.contains(patient)
                }
            } -> GenericLocation(loc.position)


            )
        })
    }

    override def closeOperation() = close()

    solution.tail.map{ case (loc, _, _) => Some(loc) } :+ None

    val solution2 = (solution, solution.tail.map{ case (loc, _, _) => Some(loc) } :+ None).zipped.map {
        case ((loc, ptAmb, gLevel), loc2) => (loc, ptAmb, gLevel, loc2)
    }

    val edgesMap = g.edges.map { edge => (verticesMap(edge.from), verticesMap(edge.to)) -> edge.weight}.toMap

    def graph: DirectedSparseGraph[Int, (Int, Int)] = {
        val graph = new DirectedSparseGraph[Int, (Int, Int)]

        for {ver <- vertexMap.keys} graph.addVertex(ver)
        for {edge: (Int, Int) <- edgesMap.keys; (from, to) = edge} graph.addEdge(edge, from, to)

        graph
    }

    private val margin = (50.0, 50.0)

    private def initializer(index: Int) = {
        val vertex = vertexMap(index)(0)
        new Point2D.Double(vertex.position._1 + margin._1, vertex.position._2 + margin._2)
    }

    private def calculateGraphSize = {
        val vertices = g.vertices
        vertices.map(_.position).unzip match {
            case (xs, ys) =>
                (xs.max + xs.min + 2 * margin._1, ys.max + ys.min + 2 * margin._2)
        }

    }

    lazy val graphVV = {
        val size: Dimension = calculateGraphSize
        val layout = new StaticLayoutExtended(graph, size)

        layout.setInitializer(initializer _)

        val vv = new VisualizationViewer(layout)
        val rc = vv.getRenderContext

        rc.setEdgeLabelTransformer((edge: (Int, Int)) => edgesMap(edge).toString)

        rc.setVertexLabelTransformer((index: Int) => getVertex(index) match {
            case GenericLocation(_) => ""
            case GasStation(_) => "GS"
            case PatientLocation(_, _) => "PL"
            case Filiation(_, _) => "FL"
        })

        rc.setVertexFillPaintTransformer((index: Int) => {
            if (rc.getPickedVertexState.isPicked(index)) {
                Color.PINK
            } else {
                getVertex(index) match {
                    case GenericLocation(_) => Color.RED
                    case GasStation(_) => Color.GREEN
                    case PatientLocation(_, _) => Color.ORANGE
                    case Filiation(_, _) => Color.CYAN
                }
            }
        })

        vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)

        vv.setPreferredSize(size);

        vv
    }

    lazy val graphComponent = {
        val comp = Component.wrap(graphVV)
        comp.preferredSize = graphVV.getSize()
        comp
    }

    title = "Viewer"

    lazy val numPatientsAmbulanceLabel = new Label()

    lazy val gasLevelLabel = new Label()

    contents = new BorderPanel {
        add(new BorderPanel {
            add(new FlowPanel(new Label("# Patients in Ambulance: "), numPatientsAmbulanceLabel), BorderPanel.Position.East)
            add(new FlowPanel(new Label("Gas Level: "), gasLevelLabel), BorderPanel.Position.West)

            maximumSize = (maximumSize.width, contents.map(_.preferredSize.height).max)
        }, BorderPanel.Position.North)
        add(graphComponent, BorderPanel.Position.Center)

        preferredSize = (contents.map(_.preferredSize.width).max, contents.map(_.preferredSize.height).max)
    }

    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)

    pack()
    visible = true
    println("this = " + this.preferredSize)
    println("graphC = " + graphComponent.preferredSize)

    private def getVertex(index: Int) = {
        val vertexStates = vertexMap(index)
        if (vertexStates.size == 1) vertexStates(0)
        else vertexStates(vertexStates.keys.toList.sortBy(-_).find(_ <= animIndex).get)
    }

    private var animIndex = 0

    for { i <- 0 to solution2.size } {
        val (loc, pAmb, gLevel, loc2) = solution2(i)
        animIndex = i
        Swing onEDT {
            numPatientsAmbulanceLabel.text = pAmb.size.toString
            gasLevelLabel.text = gLevel.toString
            graphVV.getPickedVertexState.pick(loc, true)
            if (loc2.isDefined) graphVV.getPickedEdgeState.pick((loc, loc2.get), true)
            graphVV.repaint()
        }
        Thread.sleep(500)
        Swing onEDT {
            graphVV.getPickedVertexState.pick(loc, false)
            if (loc2.isDefined) graphVV.getPickedEdgeState.pick((loc, loc2.get), false)
            graphVV.repaint()
        }
    }
}
