package pt.up.fe.iart.proj1

import pt.up.fe.iart.proj1.collections.QueueLike
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable

/* Possible search results that various search algorithms can return */
sealed abstract class SearchResult[A]

case class Success[A](result: A) extends SearchResult[A]

case class Failure[A]() extends SearchResult[A]

case class CutOff[A]() extends SearchResult[A]

object GraphSearch {
    def apply[S, A](problem: Problem[S, A], frontier: QueueLike[Node[S, A]]) = {
        @tailrec
        def loop(frontier: QueueLike[Node[S, A]], explored: immutable.HashSet[S]): SearchResult[List[A]] = {
            val n = frontier.removeFirst()
            // println(n)
            n match {
                case None => Failure()
                case Some(node) if problem.goalTest(node.state) =>
                    Success(node.solution)
                case Some(node) =>
                    if (explored.exists(_ == node.state))
                        loop(frontier, explored)
                    else {
                        val acts = problem.actions(node.state)
                        // println(acts)
                        acts.foreach((a: A) => frontier.insert(Node.childNode(problem, node, a)))
                        loop(frontier, explored + node.state)
                    }
            }
        }

        loop(frontier.insert(Node(problem.initialState)), scala.collection.immutable.HashSet.empty)
    }
}

object AStarSearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new  mutable.PriorityQueue[Node[S, A]]()(Ordering.by[Node[S, A], Double](AStarHeuristic(_, problem)).reverse))

    private def AStarHeuristic[S, A](node: Node[S, A], problem: Problem[S, A]): Double =
        node.pathCost + problem.estimatedCostToGoal(node.state) //f(n) = g(n) + h(n)
}

object BestFirstSearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new mutable.PriorityQueue[Node[S, A]]()(Ordering.by[Node[S, A], Double](BestFirstHeuristic(_, problem)).reverse))

    private def BestFirstHeuristic[S, A](node: Node[S, A], problem: Problem[S, A]) =
        problem.estimatedCostToGoal(node.state) //f(n) = h(n)
}

object BranchAndBoundSearch {
    def apply[S, A](problem: Problem[S, A]): SearchResult[List[A]] =
        GraphSearch(problem, new mutable.PriorityQueue[Node[S, A]]()(Ordering.by[Node[S, A], Double](BranchAndBoundHeuristic(_, problem)).reverse))

    private def BranchAndBoundHeuristic[S, A](node: Node[S, A], problem: Problem[S, A]) =
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
        recursiveDLS(Node[S, A](problem.initialState), problem, limit)

    private def recursiveDLS[S, A](node: Node[S, A], problem: Problem[S, A], limit: Int): SearchResult[List[A]] = {
        if (problem.goalTest(node.state))
            Success(node.solution)
        else if (node.depth == limit)
            CutOff()
        else {
            def loop(nodes: List[Node[S, A]], cutoffOccured: Boolean): SearchResult[List[A]] =
                nodes match {
                    case Nil if cutoffOccured => CutOff()
                    case Nil => Failure()
                    case n :: rest =>
                        recursiveDLS(n, problem, limit) match {
                            case Failure() => loop(rest, cutoffOccured)
                            case CutOff() => loop(rest, cutoffOccured = true)
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
                case CutOff() => loop(depth + 1)
                case res => res
            }
        loop(0)
    }
}