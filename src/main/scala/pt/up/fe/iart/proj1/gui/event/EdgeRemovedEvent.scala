package pt.up.fe.iart.proj1.gui.event

import scala.swing.event.Event
import pt.up.fe.iart.proj1.gui.Location

case class EdgeRemovedEvent[E](edge: E) extends Event

case class VertexPropertyChanged(index: Int, newVertex: Location) extends Event

case class EdgePropertyChanged(index: (Int, Int), newEdge: Double) extends Event
