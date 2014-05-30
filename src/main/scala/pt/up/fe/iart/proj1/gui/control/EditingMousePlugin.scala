package pt.up.fe.iart.proj1.gui.control

import swing._
import swing.event._

import java.awt
import awt.{Color, Graphics, Cursor, Shape}
import awt.event.InputEvent
import awt.geom.{AffineTransform, QuadCurve2D, Point2D}

import edu.uci.ics.jung
import jung.visualization.control.AbstractGraphMousePlugin
import jung.visualization.{VisualizationServer, VisualizationViewer}
import jung.graph.util.EdgeType
import jung.visualization.util.ArrowFactory
import jung.visualization.VisualizationServer.Paintable


class EditingMousePlugin[V, E](vertexFactory: (Point2D) => V, edgeFactory: (V, V) => E, modifiers: Int = InputEvent.BUTTON1_DOWN_MASK) extends AbstractGraphMousePlugin (modifiers)
    with MousePublisher
    with MouseMotionPublisher
{
    private def checkModifiers(e: MouseEvent): Boolean = (e.modifiers & modifiers) != 0

    protected val rawEdge = {
        val r = new QuadCurve2D.Float
        r.setCurve(0.0f, 0.0f, 0.5f, 15.0f, 1.0f, 0.0f)
        r
    }
    protected val rawArrowShape = ArrowFactory.getNotchedArrow(20, 16, 8)
    protected val edgePaintable = new VisualizationServer.Paintable {
        override def paint(g: Graphics): Unit =
            if (edgeShape != null) {
                val oldColor = g.getColor
                g setColor Color.black
                g.asInstanceOf[Graphics2D] draw edgeShape
                g setColor oldColor
            }

        override def useTransform(): Boolean = false
    }

    protected val arrowPaintable = new Paintable {

        override def paint(g: Graphics): Unit =
            if (arrowShape != null) {
                val oldColor = g.getColor
                g setColor Color.black
                g.asInstanceOf[Graphics2D] fill arrowShape
                g setColor oldColor
            }

        override def useTransform(): Boolean = false
    }

    protected var edgeType = EdgeType.DIRECTED
    protected var startVertex: Option[V] = None
    down = null

    protected var edgeShape: Shape = null
    protected var arrowShape: Shape = null

    private def transformEdgeShape(down: Point2D, out: Point2D): Unit = {
        val (x1, y1, x2, y2) = (down.getX, down.getY, out.getX, out.getY)

        val xform = AffineTransform.getTranslateInstance(x1, y1)

        val (dx, dy) = (x2 - x1, y2 - y1)
        xform.rotate(math.atan2(dy, dx))
        xform.scale(math.sqrt(dx*dx + dy*dy) / rawEdge.getBounds.getWidth, 1.0)

        edgeShape = xform.createTransformedShape(rawEdge)
    }

    private def transformArrowShape(down: Point2D, out: Point2D): Unit = {
        val (x1, y1, x2, y2) = (down.getX, down.getY, out.getX, out.getY)

        val xform = AffineTransform.getTranslateInstance(x2, y2)

        val (dx, dy) = (x2 - x1, y2 - y1)
        xform.rotate(math.atan2(dy, dx))

        arrowShape = xform.createTransformedShape(rawArrowShape)
    }

    cursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)

    listenTo(this)

    reactions += {
        case e: MousePressed if checkModifiers(e) =>
            val vv = e.source.peer.asInstanceOf[VisualizationViewer[V, E]]
            val p = e.point

            val pickSupport = vv.getPickSupport
            if (pickSupport != null) {
                val graph = vv.getModel.getGraphLayout.getGraph

                edgeType = graph.getDefaultEdgeType

                val vertex = pickSupport.getVertex(vv.getModel.getGraphLayout, p.getX, p.getY)
                if (vertex != null) {
                    // make an edge
                    startVertex = Some(vertex)
                    down = e.point
                    transformEdgeShape(down, down)
                    vv addPostRenderPaintable edgePaintable
                    if (edgeType == EdgeType.DIRECTED) {
                        transformArrowShape(down, e.point)
                        vv.addPostRenderPaintable(arrowPaintable)
                    }
                }
            }
            vv.repaint()
        case e: MouseReleased =>
            val vv = e.source.peer.asInstanceOf[VisualizationViewer[V, E]]
            val p = e.point

            val layout = vv.getModel.getGraphLayout
            val pickSupport = vv.getPickSupport
            if (pickSupport != null) {
                val vertex = pickSupport.getVertex(layout, p.getX, p.getY)
                if (vertex != null && startVertex.isDefined) {
                    val graph = layout.getGraph
                    val edge = edgeFactory(startVertex.get, vertex)
                    if (edge != null) {
                        graph.addEdge(edge, startVertex.get, vertex, edgeType)
                    }
                } else { //create new vertex
                val newVertex = vertexFactory(p)
                    val layout = vv.getModel.getGraphLayout
                    layout.getGraph.addVertex(newVertex)
                    layout.setLocation(newVertex, vv.getRenderContext.getMultiLayerTransformer.inverseTransform(p))
                }
                vv.repaint()
            }

            startVertex = None
            down = null
            edgeType = EdgeType.DIRECTED

            vv removePostRenderPaintable edgePaintable
            vv removePostRenderPaintable arrowPaintable

        case e: MouseDragged  if checkModifiers(e)=>
            if (startVertex.isDefined) {
                transformEdgeShape(down, e.point)
                if (edgeType == EdgeType.DIRECTED)
                    transformArrowShape(down, e.point)
            }
            e.source.peer.asInstanceOf[VisualizationViewer[V, E]].repaint()
        case e: MouseEntered =>
            e.source.cursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)
        case e: MouseExited =>
            e.source.cursor = Cursor.getDefaultCursor
    }
}
