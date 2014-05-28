package pt.up.fe.iart.proj1.gui.event

import scala.swing.event.Event

case class VertexRemovedEvent[V](vertex: V) extends Event
