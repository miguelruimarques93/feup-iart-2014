package pt.up.fe.iart.proj1.gui.control

import java.awt.event.{MouseMotionListener, MouseEvent, MouseListener}
import scala.swing.Publisher
import java.awt
import scala.swing.event._
import scala.swing.event.MousePressed
import scala.swing.event.MouseEntered
import scala.swing.event.MouseExited
import scala.swing.event.MouseReleased

trait MousePublisher extends MouseListener with Publisher
{
    override def mouseClicked(e: awt.event.MouseEvent): Unit = publish(new MouseClicked(e))

    override def mouseEntered(e: awt.event.MouseEvent): Unit = publish(new MouseEntered(e))

    override def mouseExited(e: awt.event.MouseEvent): Unit = publish(new MouseExited(e))

    override def mousePressed(e: awt.event.MouseEvent): Unit = publish(new MousePressed(e))

    override def mouseReleased(e: awt.event.MouseEvent): Unit = publish(new MouseReleased(e))
}

trait MouseMotionPublisher extends MouseMotionListener with Publisher
{
    override def mouseMoved(e: awt.event.MouseEvent): Unit = publish(new MouseMoved(e))

    override def mouseDragged(e: awt.event.MouseEvent): Unit = publish(new MouseDragged(e))
}
