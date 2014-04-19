package pt.up.fe.iart.proj1.parser

import pt.up.fe.iart.proj1.collections.Graph
import pt.up.fe.iart.proj1.problem._
import pt.up.fe.iart.proj1.parser.PTPParser._
import pt.up.fe.iart.proj1.problem.PatientLocation
import pt.up.fe.iart.proj1.problem.GenericLocation
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import java.io.FileInputStream

class GraphVisitor extends PTPBaseVisitor[Graph[Location]] {

    private object patientVisitor extends PTPBaseVisitor[Patient] {
        override def visitPatient(ctx: PatientContext): Patient =
            if (ctx.filiation() == null)
                PatientWithoutDestination()
            else
                PatientWithDestination(locationVisitor.visit(ctx.filiation()) match {
                    case f: Filiation => f;
                    case _ => throw new Error("Patient can only go to Filiation")
                })
    }

    private object locationVisitor extends PTPBaseVisitor[Location] {
        override def visitGenericLocation(ctx: GenericLocationContext): Location = GenericLocation(positionVisitor.visit(ctx.position()))

        override def visitPatientLocation(ctx: PatientLocationContext): Location = PatientLocation(positionVisitor.visit(ctx.position()), patientVisitor.visit(ctx.patient()))

        override def visitFiliation(ctx: FiliationContext): Location = Filiation(positionVisitor.visit(ctx.position()), boolVisitor.visit(ctx.bool()))

        override def visitGasStation(ctx: GasStationContext): Location = GasStation(positionVisitor.visit(ctx.position()))
    }

    private object positionVisitor extends PTPBaseVisitor[Location.Position] {
        override def visitPosition(ctx: PositionContext): Location.Position = (ctx.x.getText.toInt, ctx.y.getText.toInt)
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
        val from = locationVisitor.visit(edgeCtx.from)
        val to = locationVisitor.visit(edgeCtx.to)
        val weight = edgeCtx.weight.getText.toDouble
        _graph.addEdge(from, to, weight)
        _graph
    }

    private val _graph = new Graph[Location]()
}

object GraphVisitor {
    def main(args: Array[String]) {

        if (args.length < 1) {
            Console.err.println("Not enough arguments.")
            sys.exit(1)
        }

        val inputFile = args(0)


        val is = new FileInputStream(inputFile)
        val input = new ANTLRInputStream(is)
        val lexer = new PTPLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new PTPParser(tokens)
        val tree = parser.map()

        val visitor = new GraphVisitor()
        val graph = visitor.visit(tree)
        println(graph)
    }
}
