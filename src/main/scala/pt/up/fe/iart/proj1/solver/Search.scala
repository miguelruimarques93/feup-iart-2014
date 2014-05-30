package pt.up.fe.iart.proj1.solver

import pt.up.fe.iart.proj1.collections.QueueLike
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable

/* Possible search results that various search algorithms can return */
sealed abstract class SearchResult[+A]

case class Success[A](result: A, numberExpanded: Int) extends SearchResult[A]

case object Failure extends SearchResult[Nothing]

case object CutOff extends SearchResult[Nothing]

object GraphSearch {
    def apply[S, A](problem: Problem[S, A], frontier: QueueLike[Node[S, A]]) = {
        @tailrec
        def loop(frontier: QueueLike[Node[S, A]], explored: immutable.HashSet[S], numberExpanded: Int): SearchResult[List[A]] = {
            val n = frontier.removeFirst()
            n match {
                case None => Failure
                case Some(node) if problem.goalTest(node.state) =>
                    Success(node.solution, numberExpanded + 1)
                case Some(node) =>
                    if (explored.exists(_ == node.state))
                        loop(frontier, explored, numberExpanded)
                    else {
                        val acts = problem.actions(node.state)
                        acts.foreach((a: A) => frontier.insert(Node.childNode(problem, node, a)))
                        loop(frontier, explored + node.state, numberExpanded + 1)
                    }
            }
        }

        loop(frontier.insert(Node(problem.initialState)), scala.collection.immutable.HashSet.empty, 0)
    }
}

object AStarSearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new  mutable.PriorityQueue[Node[S, A]]()(Ordering.by[Node[S, A], Double](AStarHeuristic(_, problem)).reverse))

    private def AStarHeuristic[S, A](node: Node[S, A], problem: Problem[S, A]): Double = {
        val g = node.pathCost
        val h = problem.estimatedCostToGoal(node.state)

        g + h //f(n) = g(n) + h(n)
    }
}

object GreedySearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new mutable.PriorityQueue[Node[S, A]]()(Ordering.by[Node[S, A], Double](GreedySearchHeuristic(_, problem)).reverse))

    private def GreedySearchHeuristic[S, A](node: Node[S, A], problem: Problem[S, A]) =
        problem.estimatedCostToGoal(node.state) //f(n) = h(n)
}

object UniformCostSearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new mutable.PriorityQueue[Node[S, A]]()(Ordering.by[Node[S, A], Double](UniformCostHeuristic(_, problem)).reverse))

    private def UniformCostHeuristic[S, A](node: Node[S, A], problem: Problem[S, A]) =
        node.pathCost //f(n) = g(n)
}

object BreadthFirstSearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new mutable.Queue[Node[S, A]]())
}

object DepthFirstSearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new mutable.Stack[Node[S, A]]())
}

object DepthLimitedSearch {
    def apply[S, A](problem: Problem[S, A], limit: Int) =
        recursiveDLS(Node[S, A](problem.initialState), problem, limit, 0)

    private def recursiveDLS[S, A](node: Node[S, A], problem: Problem[S, A], limit: Int, numberExpanded: Int): SearchResult[List[A]] = {
        if (problem.goalTest(node.state))
            Success(node.solution, numberExpanded + 1)
        else if (node.depth == limit)
            CutOff
        else {
            def loop(nodes: List[Node[S, A]], cutoffOccured: Boolean): SearchResult[List[A]] =
                nodes match {
                    case Nil if cutoffOccured => CutOff
                    case Nil => Failure
                    case n :: rest =>
                        recursiveDLS(n, problem, limit, numberExpanded + 1) match {
                            case Failure => loop(rest, cutoffOccured)
                            case CutOff => loop(rest, cutoffOccured = true)
                            case ret => ret
                        }
                }
            loop(problem.actions(node.state).map(Node.childNode(problem, node, _)), cutoffOccured = false)
        }
    }
}

object IterativeDeepeningSearch {
    def apply[S, A](problem: Problem[S, A]) = {
        def loop(depth: Int): SearchResult[List[A]] =
            DepthLimitedSearch(problem, depth) match {
                case CutOff => loop(depth + 1)
                case res => res
            }
        loop(0)
    }
}