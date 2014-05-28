package pt.up.fe.iart.proj1.gui.control

import edu.uci.ics.jung.visualization.control.AbstractPopupGraphMousePlugin
import java.awt.event.{InputEvent, ActionEvent, MouseEvent}
import java.awt.geom.Point2D
import javax.swing.{AbstractAction, JPopupMenu}
import edu.uci.ics.jung.visualization.VisualizationViewer
import scala.swing.Publisher
import pt.up.fe.iart.proj1.gui.event.{EdgeRemovedEvent, VertexRemovedEvent}
import pt.up.fe.iart.proj1.gui.{ViewListener, MenuPointListener, EdgeMenuListener, VertexMenuListener}

class EditingPopupMousePlugin[V, E](vertexFactory: Point2D => V) extends AbstractPopupGraphMousePlugin(InputEvent.BUTTON3_DOWN_MASK) with Publisher {

    protected var _edgePopup: JPopupMenu = null
    protected var _vertexPopup: JPopupMenu = null
    protected var _genericPopup: JPopupMenu = null

    def vertexPopup_= (p: JPopupMenu) = _vertexPopup = p
    def vertexPopup = _vertexPopup

    def edgePopup_= (p: JPopupMenu) = _edgePopup = p
    def edgePopup = _edgePopup

    def genericPopup_= (p: JPopupMenu) = _genericPopup = p
    def genericPopup = _genericPopup

    private def updateVertexMenu(v: V, vv: VisualizationViewer[V, E], point: Point2D): Unit = {
        if (_vertexPopup != null) {
            for { comp <- _vertexPopup.getComponents() } {
                comp match {
                    case vml: VertexMenuListener[V] => vml.setVertexAndView(v, vv)
                    case _ =>
                }
                comp match {
                    case mpl: MenuPointListener => mpl.setPoint(point)
                    case _ =>
                }
                comp match {
                    case vl: ViewListener[V, E] => vl.setView(vv)
                    case _ =>
                }
            }
        }
    }

    private def updateEdgeMenu(edge: E, vv: VisualizationViewer[V, E], point: Point2D) {
        if (_edgePopup != null) {
            for {comp <- _edgePopup.getComponents() } {
                comp match {
                    case eml: EdgeMenuListener[E] => eml.setEdgeAndView(edge, vv)
                    case _ =>
                }
                comp match {
                    case mpl: MenuPointListener => mpl.setPoint(point)
                    case _ =>
                }
                comp match {
                    case vl: ViewListener[V, E] => vl.setView(vv)
                    case _ =>
                }
            }
        }
    }

    private def updateGenericMenu(vv: VisualizationViewer[V, E], point: Point2D) {
        if (_genericPopup != null) {
            for {comp <- _genericPopup.getComponents() } {
                comp match {
                    case mpl: MenuPointListener => mpl.setPoint(point)
                    case _ =>
                }
                comp match {
                    case vl: ViewListener[V, E] => vl.setView(vv)
                    case _ =>
                }
            }
        }
    }

    override def handlePopup(e: MouseEvent): Unit = {
        val vv= e.getSource.asInstanceOf[VisualizationViewer[V, E]]
        val layout = vv.getGraphLayout
        val graph = layout.getGraph
        val p = e.getPoint
        val ivp = p

        val pickSupport = vv.getPickSupport
        if (pickSupport != null) {
            val vertex = pickSupport.getVertex(layout, p.getX, p.getY)
            val edge = pickSupport.getEdge(layout, ivp.getX, ivp.getY)

            if (vertex != null && vertexPopup != null) {
                updateVertexMenu(vertex, vv, p)
                vertexPopup.show(vv, e.getX, e.getY)
            } else if (edge != null && edgePopup != null) {
                updateEdgeMenu(edge, vv, p)
                edgePopup.show(vv, e.getX, e.getY)
            } else if (genericPopup != null) {
                updateGenericMenu(vv, p)
                genericPopup.show(vv, e.getX, e.getY)
            }
        }

    }
}
