package pt.up.fe.iart.proj1.parser

import pt.up.fe.iart.proj1.collections.Graph
import pt.up.fe.iart.proj1.problem._
import pt.up.fe.iart.proj1.parser.PTPParser._
import pt.up.fe.iart.proj1.problem.PatientLocation
import pt.up.fe.iart.proj1.problem.GenericLocation

class GraphVisitor extends PTPBaseVisitor[Graph[Location]] {

    private class PatientVisitor extends PTPBaseVisitor[Patient] {
        private var lastIndex = -1

        override def visitPatient(ctx: PatientContext): Patient =
            if (ctx.filiation() == null) {
                lastIndex = lastIndex + 1
                PatientWithoutDestination(lastIndex)
            } else {
                lastIndex = lastIndex + 1
                PatientWithDestination(lastIndex, locationVisitor.visit(ctx.filiation()) match {
                    case f: Filiation => f;
                    case _ => throw new Error("Patient can only go to Filiation")
                })
            }
    }
    private val patientVisitor = new PatientVisitor()

    private object locationVisitor extends PTPBaseVisitor[Location] {
        override def visitGenericLocation(ctx: GenericLocationContext): Location = GenericLocation(positionVisitor.visit(ctx.position()))

        override def visitPatientLocation(ctx: PatientLocationContext): Location = PatientLocation(positionVisitor.visit(ctx.position()), patientVisitor.visit(ctx.patient()))

        override def visitFiliation(ctx: FiliationContext): Location = Filiation(positionVisitor.visit(ctx.position()), boolVisitor.visit(ctx.bool()))

        override def visitGasStation(ctx: GasStationContext): Location = GasStation(positionVisitor.visit(ctx.position()))
    }

    private object positionVisitor extends PTPBaseVisitor[Location.Position] {
        override def visitPosition(ctx: PositionContext): Location.Position = (ctx.x.getText.toDouble, ctx.y.getText.toDouble)
    }

    private object boolVisitor extends PTPBaseVisitor[Boolean] {
        override def visitBool(ctx: BoolContext): Boolean = ctx.getText match {
            case "true" => true
            case "false" => false
        }
    }

    override def visitMap(ctx: MapContext): Graph[Location] = {
        for {stmt <- scala.collection.JavaConversions.asScalaBuffer(ctx.stmt)}
            visit(stmt)
        _graph
    }

    override def visitNode(ctx: NodeContext): Graph[Location] = {
        val loc = locationVisitor.visit(ctx.node_stmt())
        _graph.addVertex(loc)
        _graph
    }

    override def visitEdge(ctx: EdgeContext): Graph[Location] = {
        val edgeCtx = ctx.edge_stmt()
        val tFrom = locationVisitor.visit(edgeCtx.from)
        val tTo = locationVisitor.visit(edgeCtx.to)

        val from = _graph.vertices.find(_.position == tFrom.position).get
        val to = _graph.vertices.find(_.position == tTo.position).get

        val weight = edgeCtx.weight.getText.toDouble
        _graph.addEdge(from, to, weight)
        _graph
    }

    private val _graph = new Graph[Location]()
}
