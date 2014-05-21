package pt.up.fe.iart.proj1

abstract class Problem[S, A] {
    def initialState: S
    def goalTest(s: S): Boolean

    def actions(s: S): List[A]
    def result(s: S,a: A): S

    def stepCost(from: S, action: A, to: S): Double

    def estimatedCostToGoal(from: S): Double
}