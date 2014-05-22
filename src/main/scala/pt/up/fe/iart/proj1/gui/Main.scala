package pt.up.fe.iart.proj1.gui

import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.visualization.VisualizationViewer
import scala.swing._
import java.io.FileInputStream
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import pt.up.fe.iart.proj1.parser.{GraphVisitor, PTPParser, PTPLexer}
import pt.up.fe.iart.proj1.collections.Graph
import pt.up.fe.iart.proj1.problem._
import pt.up.fe.iart.proj1.gui.GraphToJungGraph._
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import pt.up.fe.iart.proj1.problem.PatientLocation
import pt.up.fe.iart.proj1.problem.GasStation
import pt.up.fe.iart.proj1.problem.Filiation
import pt.up.fe.iart.proj1.problem.GenericLocation
import java.awt.geom.Point2D
import edu.uci.ics.jung.visualization.control.{ModalGraphMouse, DefaultModalGraphMouse}
import org.apache.commons.collections15.{Transformer, Factory}
import pt.up.fe.iart.proj1.gui.control.EditingModalGraphMouse


object Main extends SwingApplication {

    def top(g: Graph[Location]) = new MainFrame {
        title = "Hello, World!"
        contents = Component.wrap {
            val layout = new StaticLayout(g)
            val size = new Dimension(800, 600)

            layout.setSize(size)

            layout.setInitializer(new Transformer[Location, Point2D] {
                override def transform(loc: Location): Point2D = new Point2D.Double(loc.position._1 * 100 + 100, loc.position._2 * 100 + 100)
            })

            val vv = new VisualizationViewer(layout)

            vv.getRenderContext.setEdgeLabelTransformer(new Transformer[Graph[Location]#Edge, String] {
                override def transform(p1: Graph[Location]#Edge): String = p1.weight.toString
            })

            vv.getRenderContext.setVertexLabelTransformer(new Transformer[Location, String] {
                override def transform(loc: Location): String = loc match {
                    case GenericLocation(_) => ""
                    case GasStation(_) => "GS"
                    case PatientLocation(_, _) => "PL"
                    case Filiation(_, _) => "FL"
                }
            })

            vv.getRenderer().getVertexLabelRenderer().setPosition(Position.CNTR)

            val gm = new EditingModalGraphMouse[Location, Graph[Location]#Edge](
                                                vv.getRenderContext,
                                                p => {
                                                    val ver = GenericLocation((p.getX, p.getY))
                                                    println(s"Creating vertex $ver")
                                                    ver
                                                },
                                                (from, to) => {
                                                    val edge = g.Edge(from, to, 0.0)
                                                    println(s"Create edge $edge")
                                                    edge
                                                })
            gm.setMode(ModalGraphMouse.Mode.TRANSFORMING);
            vv.setGraphMouse(gm);
            vv.addKeyListener(gm.getModeKeyListener)

            vv.setPreferredSize(size)
            vv
        }
    }

    def readGraph(fileName: String): Graph[Location] = {
        val inputFile = fileName

        val is = new FileInputStream(inputFile)
        val input = new ANTLRInputStream(is)
        val lexer = new PTPLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new PTPParser(tokens)
        val tree = parser.map()

        val visitor = new GraphVisitor()
        visitor.visit(tree)
    }

    override def startup(args: Array[String]) {
        if (args.length < 1) {
            Console.err.println("Not enough arguments.")
            sys.exit(1)
        }

        val g: Graph[Location] = readGraph(args(0))


        val t = top(g)
        if (t.size == new Dimension(0,0)) t.pack()
        t.visible = true
    }
}
