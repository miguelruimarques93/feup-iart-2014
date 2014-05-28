package pt.up.fe.iart.proj1.gui

import scala.collection.mutable
import scala.swing._
import edu.uci.ics.jung
import jung.visualization.renderers.Renderer.VertexLabel.Position
import edu.uci.ics.jung.visualization.{Layer, RenderContext, VisualizationViewer}
import edu.uci.ics.jung.algorithms.layout.StaticLayout
import jung.graph.DirectedSparseGraph

import java.awt.{BasicStroke, Color}

import org.apache.commons.collections15.{Transformer, Factory}
import java.awt.geom.Point2D
import pt.up.fe.iart.proj1.gui.control._
import edu.uci.ics.jung.visualization.control.ModalGraphMouse
import pt.up.fe.iart.proj1.collections.Graph
import java.io.{PrintWriter, FileInputStream}
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}
import pt.up.fe.iart.proj1.parser.{GraphVisitor, PTPParser, PTPLexer}
import pt.up.fe.iart.proj1.problem
import scala.swing.TabbedPane.Page
import scala.swing.event.UIElementResized
import scala.swing.Dimension
import scala.Some
import scala.swing.event.SelectionChanged
import pt.up.fe.iart.proj1.gui.event.{EdgePropertyChanged, VertexPropertyChanged, VertexRemovedEvent, EdgeRemovedEvent}
import scala.swing.event.UIElementResized
import pt.up.fe.iart.proj1.gui.control.VertexMoveEvent
import pt.up.fe.iart.proj1.gui.MouseMenus.{VertexPopupMenu, EdgePopupMenu}
import scala.swing.ListView.Renderer

object Main2 extends SwingApplication {

    object Mode {
        type Mode = ModalGraphMouse.Mode
        // val Move = ModalGraphMouse.Mode.TRANSFORMING
        val Transform = ModalGraphMouse.Mode.PICKING
        val Edit = ModalGraphMouse.Mode.EDITING

        val names = Map(
            // Move -> "Move",
            Transform -> "Transform",
            Edit -> "Edit"
        )
    }

    import Mode.Mode

    val graphModes = Array(/*Mode.Move,*/ Mode.Transform, Mode.Edit)
    val patientGraphModes = Array(/*Mode.Move,*/ Mode.Edit)

    val graphModeComboBox = new ComboBox[Mode](graphModes) {
        renderer = Renderer(Mode.names)
        maximumSize = new Dimension(maximumSize.width, minimumSize.height)
    }
    val patientGraphModeComboBox = new ComboBox[Mode](patientGraphModes) {
        renderer = Renderer(Mode.names)
        maximumSize = new Dimension(maximumSize.width, minimumSize.height)
    }

    implicit def FunctionToTransformer[T1, T2](fun: T1 => T2) = new Transformer[T1, T2] {
        override def transform(p1: T1): T2 = fun(p1)
    }

    implicit def FunctionToFactory[T](fun: () => T) = new Factory[T] {
        override def create(): T = fun()
    }

    val vertexMap: mutable.Map[Int, Location] = mutable.Map()

    val edgesMap: mutable.Map[(Int, Int), Double] = mutable.Map()

    def graph: DirectedSparseGraph[Int, (Int, Int)] = {
        val graph = new DirectedSparseGraph[Int, (Int, Int)]

        for {ver <- vertexMap.keys} graph.addVertex(ver)
        for {edge <- edgesMap.keys; (from, to) = edge} graph.addEdge(edge, from, to)

        graph
    }

    private class StaticLayoutExtended[V, E](g: edu.uci.ics.jung.graph.Graph[V, E], s: Dimension) extends StaticLayout[V, E](g, s) {
        override def setSize(size: Dimension): Unit = {
            if (size != null && graph != null) {
                val oldSize: Dimension = this.size
                this.size = size
                initialize
            }
        }
    }

    private val margin = (50.0, 50.0)

    private def initializer(index: Int) = {
        val vertex = vertexMap(index)
        new Point2D.Double(vertex.position._1 + margin._1, vertex.position._2 + margin._2)
    }

    def createVertex(p: Point2D) = {
        val index: Int = if (vertexMap.isEmpty) 0 else (vertexMap.keys.max + 1)
        vertexMap.put(index, GenericLocation((p.getX - margin._1, p.getY - margin._2)))
        println(s"Vertices: $vertexMap")
        index
    }

    def createEdge(from: Int, to: Int) = {
        edgesMap.put((from, to), 0.0)
        println(s"Edges: $edgesMap")
        (from, to)
    }

    lazy val graphVV = {
        val size = new Dimension(800, 600)
        val layout = new StaticLayoutExtended(graph, size)

        layout.setInitializer(initializer _)

        val vv = new VisualizationViewer(layout)
        val rc: RenderContext[Int, (Int, Int)] = vv.getRenderContext

        rc.setEdgeLabelTransformer((edge: (Int, Int)) => edgesMap(edge).toString)

        rc.setVertexLabelTransformer((index: Int) => vertexMap(index) match {
            case GenericLocation(_) => ""
            case GasStation(_) => "GS"
            case PatientLocation(_, _) => "PL"
            case Filiation(_, _) => "FL"
        })

        rc.setVertexFillPaintTransformer((index: Int) => {
            if (rc.getPickedVertexState.isPicked(index)) {
                Color.YELLOW
            } else {
                vertexMap(index) match {
                    case GenericLocation(_) => Color.RED
                    case GasStation(_) => Color.GREEN
                    case PatientLocation(_, _) => Color.ORANGE
                    case Filiation(_, _) => Color.CYAN
                }
            }
        })

        vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)

        val gm = new EditingModalGraphMouse[Int, (Int, Int)](
            rc,
            createVertex,
            createEdge
        )

        gm.setMode(ModalGraphMouse.Mode.PICKING)
        patientGraphModeComboBox.selection.item = ModalGraphMouse.Mode.PICKING

        gm.lockMode

        val popupPlugin = gm.popupEditingPlugin

        val edgePopup = new MouseMenus.EdgePopupMenu(mainFrame.peer, edgesMap)
        val vertexPopup = new MouseMenus.VertexPopupMenu(mainFrame.peer, vertexMap)
        val createPopup = new MouseMenus.CreateVertexPopupMenu(createVertex)

        popupPlugin.edgePopup = edgePopup
        popupPlugin.vertexPopup = vertexPopup
        popupPlugin.genericPopup = createPopup

        listenTo(gm.movePublisher, gm.popupPublisher, edgePopup, vertexPopup, createPopup, graphModeComboBox.selection)

        reactions += {
            case VertexMoveEvent(index: Int, offset: (Double, Double)) =>
                if (selectedPage == Page.Graph) {
                    val vertex = vertexMap(index)
                    val newLoc = (vertex.position._1 + offset._1, vertex.position._2 + offset._2)
                    vertex.position = newLoc
                    vertexMap.put(index, vertex)
                }

            case VertexRemovedEvent(index: Int) =>
                if (selectedPage == Page.Graph) {
                    vertexMap.remove(index)
                    edgesMap.retain {
                        case ((from, to), _) => from != index && to != index
                    }
                }

            case ev: EdgeRemovedEvent[(Int, Int)] =>
                if (selectedPage == Page.Graph) {
                    edgesMap.remove(ev.edge)
                }

            case ev: VertexPropertyChanged =>
                if (selectedPage == Page.Graph) {
                    vertexMap(ev.index) = ev.newVertex
                    vv.repaint()
                }

            case ev: EdgePropertyChanged =>
                if (selectedPage == Page.Graph) {
                    edgesMap(ev.index) = ev.newEdge
                    vv.repaint()
                }

            case SelectionChanged(`graphModeComboBox`) =>
                gm.setMode(graphModeComboBox.selection.item)
        }

        vv.setGraphMouse(gm)
        vv.addKeyListener(gm.getModeKeyListener)
        vv.setPreferredSize(size)

        vv
    }

    lazy val graphComponent = Component.wrap(graphVV)

    def patientGraph: DirectedSparseGraph[Int, (Int, Int)] = {
        val graph = new DirectedSparseGraph[Int, (Int, Int)]

        for {
            ver <- vertexMap.keys
        } graph.addVertex(ver)
        for {
            (index, loc) <- vertexMap if Location.isPatientLocation(loc)
            patient <- Location.patient(loc)
            destination <- patient.destination
        } graph.addEdge((index, destination), index, destination)

        graph
    }

    lazy val patientGraphVV = {
        val size = new Dimension(800, 600)
        val gr = patientGraph
        val layout = new StaticLayoutExtended(gr, size)

        layout.setInitializer(initializer _)

        val vv = new VisualizationViewer(layout)
        val rc: RenderContext[Int, (Int, Int)] = vv.getRenderContext

        rc.setVertexLabelTransformer((index: Int) => vertexMap(index) match {
            case GenericLocation(_) => ""
            case GasStation(_) => "GS"
            case PatientLocation(_, _) => "PL"
            case Filiation(_, _) => "FL"
        })

        rc.setVertexFillPaintTransformer((index: Int) => {
            if (rc.getPickedVertexState.isPicked(index)) {
                Color.YELLOW
            } else {
                vertexMap(index) match {
                    case GenericLocation(_) => Color.RED
                    case GasStation(_) => Color.GREEN
                    case PatientLocation(_, _) => Color.ORANGE
                    case Filiation(_, _) => Color.CYAN
                }
            }
        })

        val edgeStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(10.0f), 0.0f);

        rc.setEdgeStrokeTransformer((edge: (Int, Int)) => edgeStroke)

        vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)

        val gm = new EditingModalGraphMouse[Int, (Int, Int)](
            rc,
            createVertex,
            (from: Int, to: Int) => {
                val fromVer = vertexMap(from)
                val toVer = vertexMap(to)

                if (!Location.isPatientLocation(fromVer) || !Location.isFiliation(toVer)) {
                    null
                } else {
                    val dest = Location.patient(fromVer).flatMap(_.destination)

                    if (dest.isDefined) {
                        layout.getGraph.removeEdge((from, dest.get))
                    }

                    vertexMap.put(from, PatientLocation(fromVer.position, Patient(to)))
                    (from, to)
                }
            }
        )

        gm.setMode(ModalGraphMouse.Mode.EDITING)
        patientGraphModeComboBox.selection.item = ModalGraphMouse.Mode.EDITING

        gm.lockMode

        val popupPlugin = gm.popupEditingPlugin

        val edgePopup = new MouseMenus.DeleteEdgePopupMenu(mainFrame.peer)

        popupPlugin.edgePopup = edgePopup

        listenTo(gm.movePublisher, gm.popupPublisher, edgePopup, patientGraphModeComboBox.selection)

        reactions += {
            case VertexMoveEvent(index: Int, offset: (Double, Double)) =>
                if (selectedPage == Page.PatientGraph) {
                    val vertex = vertexMap(index)
                    val newLoc = (vertex.position._1 + offset._1, vertex.position._2 + offset._2)
                    vertex.position = newLoc
                    vertexMap.put(index, vertex)
                }

            case VertexRemovedEvent(index: Int) =>
                if (selectedPage == Page.PatientGraph) {
                    vertexMap.remove(index)
                    edgesMap.retain {
                        case ((from, to), _) => from != index && to != index
                    }
                }

            case ev: EdgeRemovedEvent[(Int, Int)] =>
                if (selectedPage == Page.PatientGraph) {
                    val edge = ev.edge
                    val fromVer = vertexMap(edge._1)
                    if (Location.isPatientLocation(fromVer)) {
                        vertexMap.put(edge._1, PatientLocation(fromVer.position, Patient()))
                    }
                }

            case SelectionChanged(`patientGraphModeComboBox`) =>
                gm.setMode(patientGraphModeComboBox.selection.item)
        }
        vv.setGraphMouse(gm)
        vv.addKeyListener(gm.getModeKeyListener)
        vv.setPreferredSize(size)

        vv
    }

    lazy val patientGraphComponent = Component.wrap(patientGraphVV)

    lazy val tabs = new TabbedPane {
        pages += new Page("Graph", graphComponent)
        pages += new Page("PatientGraph", patientGraphComponent)

        graphVV.getRenderContext.setMultiLayerTransformer(patientGraphVV.getRenderContext.getMultiLayerTransformer)
    }

    private object Page extends Enumeration {
        type Page = Value
        val Graph, PatientGraph = Value
    }

    private var selectedPage = Page.Graph
    graphModeComboBox.visible = true
    patientGraphModeComboBox.visible = false

    val mainFrame = new MainFrame

    lazy val top = {
        import mainFrame.{listenTo => _, reactions => _, _}

        title = "Main2"
        contents = new BoxPanel(Orientation.Vertical) {
            contents += graphModeComboBox
            contents += patientGraphModeComboBox
            contents += tabs
        }
        menuBar = new MenuBar {
            contents += new Menu("File") {
                contents += new MenuItem("Open") {
                    action = Action("Open") {
                        val fc = new FileChooser()

                        import FileChooser.Result._

                        fc.showOpenDialog(tabs) match {
                            case Approve =>
                                loadFile(fc.selectedFile.getAbsolutePath)

                                val oldSize = graphVV.getSize
                                graphVV.getGraphLayout.setGraph(graph)
                                graphVV.getGraphLayout.setInitializer(initializer _)
                                graphVV.getGraphLayout.setSize(oldSize)

                                val oldSize1 = patientGraphVV.getSize
                                patientGraphVV.getGraphLayout.setGraph(patientGraph)
                                patientGraphVV.getGraphLayout.setInitializer(initializer _)
                                patientGraphVV.getGraphLayout.setSize(oldSize1)

                                graphVV.repaint()
                                patientGraphVV.repaint()

                            case Cancel =>
                            case Error => println("Error")

                        }

                    }
                }
                contents += new MenuItem("Save") {
                    action = Action("Save") {
                        val fc = new FileChooser()

                        import FileChooser.Result._

                        fc.showSaveDialog(tabs) match {
                            case Approve =>
                                val g = new Graph[problem.Location]

                                val tempLocs = for {(index, loc) <- vertexMap} yield index -> {
                                    loc match {
                                        case GenericLocation(position) => problem.GenericLocation(position)
                                        case GasStation(position) => problem.GasStation(position)
                                        case Filiation(position, hasGarage) => problem.Filiation(position, hasGarage)
                                        case PatientLocation(position, patient) if patient.hasDestination =>
                                            problem.PatientLocation(
                                                position,
                                                problem.PatientWithDestination(patient.destination.map(dest => problem.Filiation(vertexMap(dest).position, vertexMap(dest).asInstanceOf[Filiation].hasGarage)).get))
                                        case PatientLocation(position, patient) =>
                                            problem.PatientLocation(
                                                position,
                                                problem.PatientWithoutDestination())
                                    }
                                }

                                println(tempLocs)

                                val tempEdges = for {((from, to), weight) <- edgesMap} yield (tempLocs(from), tempLocs(to)) -> weight

                                for {(_, loc) <- tempLocs} g.addVertex(loc)

                                for {((from, to), weight) <- tempEdges} g.addEdge(from, to, weight)

                                Some(new PrintWriter(fc.selectedFile)).foreach { p => p.write(g.toString); p.close}

                            case Cancel =>
                            case Error => println("Error")

                        }

                    }
                }
            }
        }

        listenTo(tabs.selection, mainFrame)

        reactions += {
            case ev: SelectionChanged =>
                val page = tabs.selection.page

                if (tabs.selection.page.content == graphComponent) {
                    val oldSize = patientGraphVV.getSize
                    graphVV.getGraphLayout.setGraph(graph)
                    graphVV.getGraphLayout.setInitializer(initializer _)
                    graphVV.getGraphLayout.setSize(oldSize)
                    selectedPage = Page.Graph
                    graphModeComboBox.visible = true
                    patientGraphModeComboBox.visible = false
                }
                else if (tabs.selection.page.content == patientGraphComponent) {
                    val oldSize = patientGraphVV.getSize
                    patientGraphVV.getGraphLayout.setGraph(patientGraph)
                    patientGraphVV.getGraphLayout.setInitializer(initializer _)
                    patientGraphVV.getGraphLayout.setSize(oldSize)
                    selectedPage = Page.PatientGraph
                    graphModeComboBox.visible = false
                    patientGraphModeComboBox.visible = true
                }

            case ev: UIElementResized =>
                if (tabs.selection.page.content == graphComponent) {
                    graphVV.getGraphLayout.setSize(graphVV.getSize)
                }
                else if (tabs.selection.page.content == patientGraphComponent) {
                    patientGraphVV.getGraphLayout.setSize(patientGraphVV.getSize)
                }
        }

        mainFrame
    }

    def readGraph(fileName: String): Graph[problem.Location] = {
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

    def loadFile(file: String) = {
        vertexMap.clear()
        edgesMap.clear()

        val g = readGraph(file)

        val verticesMap = g.verticesMap
        val (tempVertexMap, tempEdges) = (verticesMap.map(_.swap), g.edges)

        val nonPatientLocationMap = for {
            (index, loc) <- tempVertexMap if !problem.Location.isPatientLocation(loc)
        } yield {
            index -> {
                loc match {
                    case problem.GenericLocation(pos) => GenericLocation(pos)
                    case problem.GasStation(pos) => GasStation(pos)
                    case problem.Filiation(pos, hasGarage) => Filiation(pos, hasGarage)
                }
            }
        }

        vertexMap ++= nonPatientLocationMap ++ (for {
            (index, loc) <- tempVertexMap if problem.Location.isPatientLocation(loc)
        } yield {
            index -> (loc match {
                case pl@problem.PatientLocation(pos, _) => PatientLocation(pos, pl.patient.destination match {
                    case Some(dest) => Patient(verticesMap(dest))
                    case None => Patient()
                }

                )
            })
        })

        edgesMap ++= (for {
            edge <- tempEdges
        } yield {
            (verticesMap(edge.from), verticesMap(edge.to)) -> edge.weight
        })
    }

    override def startup(args: Array[String]): Unit = {
        if (args.size == 1)
            loadFile(args(0))

        val t = top
        if (t.size == new Dimension(0, 0)) t.pack()
        t.visible = true
    }
}