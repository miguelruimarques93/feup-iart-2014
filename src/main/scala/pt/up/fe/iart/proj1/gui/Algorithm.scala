package pt.up.fe.iart.proj1.gui


sealed abstract class Algorithm(name: String) {
    override def toString = name
}

case object BreadthFirst extends Algorithm("Breadth First")
case object DepthFirst extends Algorithm("Depth First")
case object DepthLimited extends Algorithm("Depth Limited")
case object IterativeDeepening extends Algorithm("Iterative Deepening")
case object UniformCost extends Algorithm("Uniform Cost")
case object Greedy extends Algorithm("Greedy")
case object AStar extends Algorithm("A*")

object Algorithm {
    def values: Seq[Algorithm] = Seq(DepthFirst, DepthLimited, IterativeDeepening, BreadthFirst, UniformCost, Greedy, AStar)
}
