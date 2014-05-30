package pt.up.fe.iart.proj1.gui

import scala.swing.Dimension
import edu.uci.ics.jung.algorithms.layout.StaticLayout
import edu.uci.ics.jung.graph.Graph

class StaticLayoutExtended[V, E](g: Graph[V, E], s: Dimension) extends StaticLayout[V, E](g, s) {
    override def setSize(size: Dimension): Unit = {
        if (size != null && graph != null) {
            this.size = size
            initialize()
        }
    }
}
