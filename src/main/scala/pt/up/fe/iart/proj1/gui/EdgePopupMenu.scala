package pt.up.fe.iart.proj1.gui

import javax.swing.{JDialog, JMenuItem, JFrame, JPopupMenu}
import edu.uci.ics.jung.visualization.VisualizationViewer
import java.awt.geom.Point2D
import scala.swing.Swing.ActionListener
import scala.swing.Publisher
import pt.up.fe.iart.proj1.gui.event.{EdgePropertyChanged, EdgeRemovedEvent, VertexRemovedEvent, VertexPropertyChanged}


trait EdgeMenuListener[E] {
    def setEdgeAndView(e: E, visView: VisualizationViewer[_, E]): Unit
}

trait ViewListener[V, E] {
    def setView(visView: VisualizationViewer[V, E]): Unit
}

trait VertexMenuListener[V] {
    def setVertexAndView(v: V, visView: VisualizationViewer[V, _]): Unit
}

trait MenuPointListener {
    def setPoint(point: Point2D): Unit
}

object MouseMenus {
    class DeleteEdgePopupMenu(frame: JFrame) extends JPopupMenu("Edge Menu") with Publisher {
        private val deleteEdgeMenuItem = new DeleteEdgeMenuItem

        add(deleteEdgeMenuItem)

        listenTo(deleteEdgeMenuItem)
        deafTo(this)

        reactions += {
            case ev => publish(ev)
        }
    }

    class EdgePopupMenu(frame: JFrame, trans: ((Int, Int)) => Double) extends JPopupMenu("Edge Menu") with Publisher {
        private val deleteEdgeMenuItem = new DeleteEdgeMenuItem
        private val edgePropItem = new EdgePropItem(frame, trans)

        add(deleteEdgeMenuItem)
        addSeparator()
        add(edgePropItem)

        listenTo(deleteEdgeMenuItem, edgePropItem)
        deafTo(this)

        reactions += {
            case ev => publish(ev)
        }
    }

    class VertexPopupMenu(frame: JFrame, trans: Int => Location) extends JPopupMenu("Vertex Menu") with Publisher {
        private val deleteVertexMenuItem = new DeleteVertexMenuItem
        private val vertexPropItem = new VertexPropItem(frame, trans)

        add(deleteVertexMenuItem)
        addSeparator()
        add(vertexPropItem)

        listenTo(deleteVertexMenuItem, vertexPropItem)
        deafTo(this)

        reactions += {
            case ev => publish(ev)
        }
    }

    class CreateVertexPopupMenu(vertexFactory : Point2D => Int) extends JPopupMenu("Vertex Menu") with Publisher {
        private val createVertexMenuItem = new CreateVertexMenuItem(vertexFactory)

        add(createVertexMenuItem)

        listenTo(createVertexMenuItem)
        deafTo(this)

        reactions += {
            case ev => publish(ev)
        }
    }


    class EdgePropItem(frame: JFrame, trans: ((Int, Int)) => Double) extends JMenuItem("Edit Properties...") with EdgeMenuListener[(Int, Int)] with Publisher with MenuPointListener {
        private var edge: (Int, Int) = null
        private var visComp: VisualizationViewer[_, (Int, Int)] = null
        private var point: Point2D = null

        override def setEdgeAndView(e: (Int, Int), visView: VisualizationViewer[_, (Int, Int)]): Unit = {
            edge = e
            visComp = visView
        }

        override def setPoint(p: Point2D) = {
            point = p
        }

        addActionListener(ActionListener { e =>
            val dialog = new EdgePropertyDialog(frame, trans(edge))
            dialog.setLocation(point.getX.toInt + frame.getX, point.getY.toInt + frame.getY)
            dialog.setVisible(true)

            if (!dialog.isCancelled) {
                val newEdge = dialog.getDistance
                publish(EdgePropertyChanged(edge, newEdge))
            }
        })
    }

    class VertexPropItem(frame: JFrame, trans: Int => Location) extends JMenuItem("Edit Properties...") with VertexMenuListener[Int] with Publisher with MenuPointListener{
        private var vertex: Int = -1
        private var visComp: VisualizationViewer[Int, _] = null
        private var point: Point2D = null

        override def setVertexAndView(v: Int, visView: VisualizationViewer[Int, _]): Unit = {
            vertex = v
            visComp = visView
        }

        override def setPoint(p: Point2D) = {
            point = p
        }

        addActionListener(ActionListener { e =>
            val dialog = new VertexPropertyDialog(frame, trans(vertex))
            dialog.setLocation(point.getX.toInt + frame.getX, point.getY.toInt + frame.getY)
            dialog.setVisible(true)

            if (!dialog.isCancelled) {
                val newVertex = dialog.getVertex
                publish(VertexPropertyChanged(vertex, newVertex))
            }
        })
    }



    class DeleteEdgeMenuItem extends JMenuItem("Delete Edge") with EdgeMenuListener[(Int, Int)] with Publisher {
        private var edge: (Int, Int) = null
        private var visComp: VisualizationViewer[_, (Int, Int)] = null

        override def setEdgeAndView(e: (Int, Int), visView: VisualizationViewer[_, (Int, Int)]): Unit = {
            edge = e
            visComp = visView
        }

        addActionListener(ActionListener { e =>
            visComp.getPickedEdgeState.pick(edge, false)
            visComp.getGraphLayout.getGraph.removeEdge(edge)
            publish(EdgeRemovedEvent(edge))
            visComp.repaint()
        })
    }

    class DeleteVertexMenuItem extends JMenuItem("Delete Vertex") with VertexMenuListener[Int] with Publisher {
        private var vertex: Int = -1
        private var visComp: VisualizationViewer[Int, _] = null

        override def setVertexAndView(v: Int, visView: VisualizationViewer[Int, _]): Unit = {
            vertex = v
            visComp = visView
        }

        addActionListener(ActionListener { e =>
            visComp.getPickedVertexState.pick(vertex, false)
            visComp.getGraphLayout.getGraph.removeVertex(vertex)
            publish(VertexRemovedEvent(vertex))
            visComp.repaint()
        })
    }

    class CreateVertexMenuItem(vertexFactory : Point2D => Int) extends JMenuItem("Create Vertex") with ViewListener[Int, (Int, Int)] with MenuPointListener with Publisher {
        private var point: Point2D = null
        private var visComp: VisualizationViewer[Int, (Int, Int)] = null

        override def setView(visView: VisualizationViewer[Int, (Int, Int)]): Unit = {
            visComp = visView
        }

        override def setPoint(p: Point2D): Unit = {
            point = p
        }

        addActionListener(ActionListener { e =>
            val newVertex = vertexFactory(point)
            val layout = visComp.getModel.getGraphLayout
            layout.getGraph.addVertex(newVertex)
            layout.setLocation(newVertex, visComp.getRenderContext.getMultiLayerTransformer.inverseTransform(point))
        })
    }

}
