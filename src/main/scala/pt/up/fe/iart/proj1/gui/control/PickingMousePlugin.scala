package pt.up.fe.iart.proj1.gui.control

import edu.uci.ics.jung.visualization.control.AbstractGraphMousePlugin
import java.awt.event.InputEvent
import java.awt.Cursor
import edu.uci.ics.jung.visualization.VisualizationServer.Paintable
import java.awt.{Color, Graphics2D, Graphics}
import java.awt.geom.{Rectangle2D, Point2D}
import scala.swing._
import scala.swing.event._
import edu.uci.ics.jung.visualization.{ Layer, VisualizationViewer }
import scala.swing.event.MousePressed
import scala.swing.event.MouseEntered
import scala.swing.event.MouseReleased
import scala.Some
import scala.swing.event.MouseDragged
import scala.collection.JavaConversions._

case class VertexMoveEvent[V](vertex: V, offset: (Double, Double)) extends Event

class PickingMousePlugin[V, E](selectionModifiers: Int = InputEvent.BUTTON1_DOWN_MASK, addToSelectionModifiers: Int = InputEvent.BUTTON1_DOWN_MASK | InputEvent.SHIFT_MASK)
    extends AbstractGraphMousePlugin(selectionModifiers)
    with MousePublisher
    with MouseMotionPublisher
    with Publisher
{
    protected var vertex: Option[V] = None
    protected var edge: Option[E] = None
    protected var offset = (0.0, 0.0)
    protected val rect = new Rectangle2D.Float

    down = null

    protected var _lensColor = Color.cyan

    def lensColor: Color = _lensColor
    def lensColor_=(c: Color) = _lensColor = c

    protected var _locked: Boolean = false

    def locked: Boolean = _locked
    def locked_=(b: Boolean) = _locked = b


    private val lensPaintable = new Paintable {

        override def useTransform() = false

        override def paint(g: Graphics): Unit = {
            val oldColor = g.getColor
            g setColor _lensColor
            g.asInstanceOf[Graphics2D] draw rect
            g setColor oldColor
        }
    }

    listenTo(this)

    reactions += {
        case e: MousePressed =>
            down = e.point
            val vv = e.source.peer.asInstanceOf[VisualizationViewer[V, E]]
            val pickSupport = vv.getPickSupport

            val pickedVertexState = vv.getPickedVertexState
            val pickedEdgeState = vv.getPickedEdgeState

            if (pickSupport != null && pickedVertexState != null) {
                val layout = vv.getGraphLayout

                if (e.modifiers == modifiers) {
                    rect.setFrameFromDiagonal(down, down)

                    val v = pickSupport.getVertex(layout, down.getX, down.getY)
                    if (v != null) {
                        vertex = Some(v)
                        if (!pickedVertexState.isPicked(vertex.get)) {
                            pickedVertexState.clear()
                            pickedVertexState.pick(vertex.get, true)
                        }

                        val q = layout.transform(vertex.get)

                        val gp = vv.getRenderContext.getMultiLayerTransformer.inverseTransform(Layer.LAYOUT, down)

                        offset = (gp.getX - q.getX, gp.getY - q.getY)
                    } else if ({
                        val e = pickSupport.getEdge(layout, down.getX, down.getY)
                        if (e != null) edge = Some(e)
                        e != null
                    }) {
                        pickedEdgeState.clear()
                        pickedEdgeState.pick(edge.get, true)
                    } else {
                        vv.addPostRenderPaintable(lensPaintable)
                        pickedEdgeState.clear()
                        pickedVertexState.clear()
                    }

                } else if (e.modifiers == addToSelectionModifiers) {
                    vv.addPostRenderPaintable(lensPaintable)
                    rect.setFrameFromDiagonal(down, down)
                    val v = pickSupport.getVertex(layout, down.getX, down.getY)
                    if (v != null) {
                        vertex = Some(v)
                        if (pickedVertexState.pick(vertex.get, !pickedVertexState.isPicked(vertex.get))) {
                            vertex = None
                        } else {
                            val q = layout.transform(vertex.get)
                            val gp = vv.getRenderContext.getMultiLayerTransformer.inverseTransform(Layer.LAYOUT, down)

                            offset = (gp.getX - q.getX, gp.getY - q.getY)
                        }

                    } else if ({
                        val e = pickSupport.getEdge(layout, down.getX, down.getY)
                        if (e != null) edge = Some(e)
                        e != null
                    })
                    {
                        pickedEdgeState.pick(edge.get, !pickedEdgeState.isPicked(edge.get))
                    }
                }
            }
            if (vertex.isDefined) e.consume()

        case e: MouseReleased =>
            val vv = e.source.peer.asInstanceOf[VisualizationViewer[V, E]]
            if (e.modifiers == modifiers || e.modifiers == addToSelectionModifiers) {
                if (down != null) {
                    val out = e.point

                    if (vertex.isEmpty && !heyThatIsTooClose(down, out, 5))
                        pickContainedVertices(vv, down, out, e.modifiers == modifiers)
                }
            }

            down = null
            vertex = None
            edge = None
            rect.setFrame(0,0,0,0)

            vv.removePostRenderPaintable(lensPaintable)
            vv.repaint()

        case e: MouseDragged if !_locked =>
            val vv = e.source.peer.asInstanceOf[VisualizationViewer[V, E]]
            if (vertex.isDefined) {
                val p = e.point
                val mt = vv.getRenderContext.getMultiLayerTransformer

                val graphPoint = mt.inverseTransform(p)
                val graphDown = mt.inverseTransform(down)

                val layout = vv.getGraphLayout

                val (dx, dy) = (graphPoint.getX - graphDown.getX, graphPoint.getY - graphDown.getY)
                val ps = vv.getPickedVertexState

                for (v <- ps.getPicked) {
                    val vp = layout.transform(v)
                    vp.setLocation(vp.getX + dx, vp.getY + dy)
                    layout.setLocation(v, vp)
                    publish(VertexMoveEvent(v, (dx, dy)))
                }
                down = p
            } else {
                val out = e.point
                if (e.modifiers == addToSelectionModifiers || e.modifiers == modifiers)
                    rect.setFrameFromDiagonal(down, out)
            }
            if (vertex.isDefined) e.consume()
            vv.repaint()

        case e: MouseEntered => e.source.cursor = cursor
        case e: MouseExited => e.source.cursor = Cursor.getDefaultCursor
    }

    def heyThatIsTooClose(p: Point2D, q: Point2D, min: Double): Boolean = math.abs(p.getX - q.getX) < min && math.abs(p.getY - q.getY) < min

    def pickContainedVertices(vv: VisualizationViewer[V, E], down: Point2D, out: Point2D, clear: Boolean) = {
        val layout = vv.getGraphLayout
        val pickedVertexState = vv.getPickedVertexState

        val pickRectangle = new Rectangle2D.Double
        pickRectangle.setFrameFromDiagonal(down, out)

        if (pickedVertexState != null) {
            if (clear)
                pickedVertexState.clear()

            val pickSupport = vv.getPickSupport

            val picked = pickSupport.getVertices(layout, pickRectangle)
            for (v <- picked)
                pickedVertexState.pick(v, true)
        }
    }
}
