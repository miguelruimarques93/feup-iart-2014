package pt.up.fe.iart.proj1.solver

abstract class Problem[State, Action] {
    def initialState: State
    def goalTest(s: State): Boolean

    def actions(s: State): List[Action]
    def result(s: State,a: Action): State

    def stepCost(from: State, action: Action, to: State): Double

    def estimatedCostToGoal(from: State): Double
}