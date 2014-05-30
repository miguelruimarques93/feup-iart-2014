package pt.up.fe.iart.proj1.solver

import scala.annotation.tailrec

class Node[S, A](val state: S, val parent: Option[Node[S, A]], val action: Option[A], val depth: Int, val pathCost: Double) {

    def solution: List[A] = {
        @tailrec
        def loop(node: Node[S, A], a: List[A]): List[A] =
            node.parent match {
                case None => a
                case Some(x) => loop(x, node.action.get :: a)
            }
        loop(this, List.empty)
    }

    override def toString: String = state.toString
}

/* Factory for easy Node creation */
object Node {
    def apply[S,A](state:S) = new Node[S,A](state,None,None,0,0.0)

    def apply[S,A](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int) =
        new Node[S,A](state,parent,action,depth,0.0)

    def apply[S,A](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int, pathCost: Double) =
        new Node[S,A](state,parent,action,depth,pathCost)

    def childNode[S,A](problem: Problem[S,A], parent: Node[S,A], action: A) = {
        val state = problem.result(parent.state,action)
        val pathCost = parent.pathCost + problem.stepCost(parent.state,action,state)
        new Node[S,A](state,Some(parent),Some(action),parent.depth+1,pathCost)
    }
}