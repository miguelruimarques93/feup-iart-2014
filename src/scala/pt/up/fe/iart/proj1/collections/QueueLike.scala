package pt.up.fe.iart.proj1.collections

import scala.collection.mutable

trait QueueLike[A] {
    def isEmpty: Boolean
    def insert(elem: A): QueueLike[A]
    def removeFirst(): Option[A]
}

object QueueLike {

    implicit class QueueLikeQueue[A](queue: mutable.Queue[A]) extends QueueLike[A] {
        override def removeFirst(): Option[A] = { if(queue.isEmpty) None else Some(queue.dequeue()) }
        override def insert(elem: A): QueueLike[A] = { queue += elem; queue }
        override def isEmpty: Boolean = queue.isEmpty
    }

    implicit class PriorityQueueLikeQueue[A](queue: mutable.PriorityQueue[A]) extends QueueLike[A] {
        override def removeFirst(): Option[A] = { if(queue.isEmpty) None else Some(queue.dequeue()) }
        override def insert(elem: A): QueueLike[A] = { queue += elem; queue }
        override def isEmpty: Boolean = queue.isEmpty
    }

    implicit class StackLikeQueue[A](queue: mutable.Stack[A]) extends QueueLike[A] {
        override def removeFirst(): Option[A] = { if(queue.isEmpty) None else Some(queue.pop()) }
        override def insert(elem: A): QueueLike[A] = { queue.push(elem); queue }
        override def isEmpty: Boolean = queue.isEmpty
    }
}
