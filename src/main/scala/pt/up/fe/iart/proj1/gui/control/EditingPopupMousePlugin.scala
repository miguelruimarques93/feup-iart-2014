package pt.up.fe.iart.proj1.gui.control

import edu.uci.ics.jung.visualization.control.AbstractPopupGraphMousePlugin
import java.awt.event.{ActionEvent, MouseEvent}
import java.awt.geom.Point2D
import javax.swing.{AbstractAction, JMenu, JPopupMenu}
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.graph.{DirectedGraph, UndirectedGraph}
import scala.collection.JavaConversions._
import edu.uci.ics.jung.graph.util.EdgeType
import scala.swing.Publisher
import scala.swing.event.Event

case class VertexRemovedEvent[V](vertex: V) extends Event
case class EdgeRemovedEvent[E](edge: E) extends Event

class EditingPopupMousePlugin[V, E](vertexFactory: Point2D => V, edgeFactory: (V, V) => E) extends AbstractPopupGraphMousePlugin with Publisher {

    protected val popup = new JPopupMenu()

    override def handlePopup(e: MouseEvent): Unit = {
        popup.removeAll()

        val vv = e.getSource.asInstanceOf[VisualizationViewer[V, E]]
        val layout = vv.getGraphLayout
        val graph = layout.getGraph
        val p = e.getPoint
        val ivp = p

        val pickSupport = vv.getPickSupport
        if (pickSupport != null) {
            val vertex = pickSupport.getVertex(layout, ivp.getX, ivp.getY)
            val edge = pickSupport.getEdge(layout, ivp.getX, ivp.getY)
            val pickedVertexState = vv.getPickedVertexState
            val pickedEdgeState = vv.getPickedEdgeState

            if (vertex != null) {
                val picked = pickedVertexState.getPicked
                if (picked.size > 0) {
                    if (graph.isInstanceOf[UndirectedGraph[_, _]] == false) {
                        val directedMenu: JMenu = new JMenu("Create Directed Edge")
                        popup.add(directedMenu)
                        import scala.collection.JavaConversions._
                        for (other <- picked) {
                            directedMenu.add(new AbstractAction(("[" + other + "," + vertex + "]")) {
                                def actionPerformed(e: ActionEvent) {
                                    graph.addEdge(edgeFactory(other, vertex), other, vertex, EdgeType.DIRECTED)
                                    vv.repaint()
                                }
                            })
                        }
                    }
                    if (graph.isInstanceOf[DirectedGraph[_, _]] == false) {
                        val undirectedMenu: JMenu = new JMenu("Create Undirected Edge")
                        popup.add(undirectedMenu)
                        import scala.collection.JavaConversions._
                        for (other <- picked) {
                            undirectedMenu.add(new AbstractAction(("[" + other + "," + vertex + "]")) {
                                def actionPerformed(e: ActionEvent) {
                                    graph.addEdge(edgeFactory(other, vertex), other, vertex)
                                    vv.repaint()
                                }
                            })
                        }
                    }
                }
                popup.add(new AbstractAction(("Delete Vertex")) {
                    def actionPerformed(e: ActionEvent) {
                        pickedVertexState.pick(vertex, false)
                        graph.removeVertex(vertex)
                        publish(VertexRemovedEvent(vertex))
                        vv.repaint()
                    }
                })
            } else if (edge != null) {
                popup.add(new AbstractAction(("Delete Edge")) {
                    def actionPerformed(e: ActionEvent) {
                        pickedEdgeState.pick(edge, false)
                        graph.removeEdge(edge)
                        publish(EdgeRemovedEvent(vertex))
                        vv.repaint()
                    }
                })
            } else {
                popup.add(new AbstractAction(("Create Vertex")) {
                    def actionPerformed(e: ActionEvent) {
                        val newVertex: V = vertexFactory(p)
                        graph.addVertex(newVertex)
                        layout.setLocation(newVertex, vv.getRenderContext.getMultiLayerTransformer.inverseTransform(p))
                        vv.repaint()
                    }
                })
            }

            if (popup.getComponentCount > 0) {
                popup.show(vv, e.getX, e.getY)
            }

        }

    }
}
