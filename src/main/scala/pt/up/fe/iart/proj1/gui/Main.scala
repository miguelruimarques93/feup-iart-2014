package pt.up.fe.iart.proj1.gui

import scala.swing._
import scala.swing.event._

import javax.swing.Box

import pt.up.fe.iart.proj1
import proj1.gui.Conversions._
import java.awt.event.{MouseEvent => AWTMouseEvent, KeyAdapter => AWTKeyAdapter, MouseAdapter, KeyEvent => AWTKeyEvent}

import javax.swing.table.DefaultTableModel
import scala.swing.Table.{IntervalMode, ElementMode, AutoResizeMode}
import pt.up.fe.iart.proj1._
import pt.up.fe.iart.proj1.problem.{PatientTransportationProblem}
import scala.io.Position
import scala.collection.mutable
import scala.swing.event.SelectionChanged
import pt.up.fe.iart.proj1.problem
import scala.swing.event.TableRowsSelected
import pt.up.fe.iart.proj1.solver._
import scala.swing.event.EditDone
import scala.swing.event.SelectionChanged
import pt.up.fe.iart.proj1.solver.Success
import scala.swing.event.TableRowsSelected
import org.jfree.data.category.DefaultCategoryDataset
import scalax.chart.CategoryChart
import scalax.chart.module.Imports
import scalax.chart.module.ChartFactories.BarChart
import org.jfree.chart.axis.{AxisLocation, CategoryAxis, NumberAxis}
import org.jfree.chart.renderer.category.{BarRenderer, LineAndShapeRenderer}
import org.jfree.chart.labels.StandardCategoryToolTipGenerator
import org.jfree.chart.plot.{CategoryPlot, DatasetRenderingOrder}
import org.jfree.chart.{JFreeChart, LegendItemCollection}

case class TableColumnHeaderSelected(override val source: Table, column: Int, clickCount: Int = 1) extends TableEvent(source)

class TableExtended(override val model: DefaultTableModel) extends Table {
    selection.elementMode = ElementMode.Row
    selection.intervalMode = IntervalMode.Single
    peer.setModel(model)

    def this(columnNames: String*) = this(new DefaultTableModel() {
        setColumnIdentifiers(Array[AnyRef](columnNames: _*))
    })

    def makeHeaderEvent(column: Int, clickCount: Int) = TableColumnHeaderSelected(this, column, clickCount)

    val tableHeader = peer.getTableHeader
    tableHeader.addMouseListener(new MouseAdapter() {
        override def mouseClicked(e: AWTMouseEvent) {
            selection.publish(makeHeaderEvent(tableHeader.columnAtPoint(e.getPoint), if (e.getClickCount > 1) 2 else 1))
        }
    })

    autoResizeMode = AutoResizeMode.AllColumns
}

object Main extends SimpleSwingApplication {

    lazy val top = new MainFrame {
        title = "Main Window"
        menuBar = menu
        contents = contentPane

        depthLimitedForm.visible = algorithmComboBox.selection.item == DepthLimited
        shouldRunButtonBeEnabled()
    }

    lazy val fileChooser = new FileChooser(new java.io.File(".").getCanonicalFile)

    lazy val loadedMaps = new TableExtended("File", "# Nodes", "# Patients", "# Filiations", "# Gas Stations", "# Edges")

    lazy val resultsTable = new TableExtended("Algorithm", "File", "Ambulance Capacity", "Gas Tank Capacity", "Result", "Time", "# Expanded Nodes", "Total Cost")

    lazy val menu = new MenuBar {
        contents += new Menu("File") {
            contents += new MenuItem(Action("Load File") {
                import FileChooser.Result._

                fileChooser.showOpenDialog(contentPane) match {
                    case Approve =>
                        addMap(fileChooser.selectedFile.getAbsolutePath)
                    case Cancel =>
                    case Error => println("Error")
                }

            })
            contents += new Separator
            contents += new MenuItem(Action("Exit") {
                quit()
            })
        }
    }

    def time[A](block: => A): (A, Long) = {
        val now = System.currentTimeMillis()
        val ret = block
        val t = System.currentTimeMillis() - now
        (ret, t)
    }

    lazy val runButton = new Button(Action("Run Selected") {
        val row = loadedMaps.selection.rows.leadIndex
        val ambCap = ambulanceCapacity.text.toInt
        val gas = gasTankCapacity.text.toDouble
        val alg = algorithmComboBox.selection.item
        val depthLimit = depthLimitedLimit.text

        import scala.concurrent.ExecutionContext.Implicits.global
        scala.concurrent.future {
            val file = loadedMaps.model.getValueAt(loadedMaps.selection.rows.leadIndex, 0).asInstanceOf[String]
            val g = PatientTransportationProblem.readGraph(file)
            val vs = g.verticesMap.map(_.swap)
            g.getShortestPaths match {
                case (weights, predecessors) =>
                    val ptp = new PatientTransportationProblem(vs, weights, ambCap, gas)
                    val (result: SearchResult[List[Int]], t: Long) = time(alg match {
                        case BreadthFirst => BreadthFirstSearch(ptp)
                        case DepthFirst => DepthFirstSearch(ptp)
                        case DepthLimited => DepthLimitedSearch(ptp, depthLimit.toInt)
                        case IterativeDeepening => IterativeDeepeningSearch(ptp)
                        case UniformCost => UniformCostSearch(ptp)
                        case Greedy => GreedySearch(ptp)
                        case AStar => AStarSearch(ptp)
                    })

                    result match {
                        case Success(path, numberExpanded) =>
                            val initial = ptp.initialState
                            val states = path.foldLeft(List(initial))((acc, action) => ptp.result(acc.head, action) :: acc).reverse
                            val totalCost = (states, path).zipped.map {
                                case (st, ac) =>
                                    val finalSt = ptp.result(st, ac)
                                    ptp.stepCost(st, ac, finalSt)
                            }.sum

                            // states.foreach(ptp.printEstimatedCostToGoal(_))

                            val statesComplete = (states, states.tail).zipped.flatMap({
                                case (i, f) =>
                                    val u = i.currentLocation
                                    var v = f.currentLocation

                                    var path = mutable.Buffer(v)
                                    while (u != v) {
                                        v = predecessors(u)(v).get
                                        path += v
                                    }

                                    val res = path.tail.reverse.map((_, i.patientsAmbulance, i.gasLevel)).to[List]

                                    res.head :: (res, res.tail).zipped.map {
                                        case ((iLoc, _, _), (fLoc, fPatientsAmbulance, fGas)) =>
                                            (fLoc, fPatientsAmbulance, fGas - weights(iLoc)(fLoc).get)
                                    }
                            }) :+ {
                                val st = states.last
                                (st.currentLocation, st.patientsAmbulance, st.gasLevel)
                            }

                            Swing.onEDT {
                                (0 until resultsTable.model.getRowCount).find { i =>
                                    alg.toString() == resultsTable.model.getValueAt(i, 0) && file == resultsTable.model.getValueAt(i, 1) &&
                                        ambCap.toString == resultsTable.model.getValueAt(i, 2) && gas.toString == resultsTable.model.getValueAt(i, 3)
                                } match {
                                    case Some(index) =>
                                        resultsTable.model.setValueAt("Success", index, 4)
                                        resultsTable.model.setValueAt(t.toString, index, 5)
                                        resultsTable.model.setValueAt(numberExpanded.toString, index, 6)
                                        resultsTable.model.setValueAt(totalCost.toString, index, 7)
                                    case None =>
                                        resultsTable.model.addRow(
                                            Array[AnyRef](
                                                alg.toString(),
                                                file,
                                                ambCap.toString,
                                                gas.toString,
                                                "Success",
                                                t.toString,
                                                numberExpanded.toString,
                                                totalCost.toString
                                            ))
                                }





                            }

                            new Thread {
                                val frm = new Viewer(g, statesComplete.map {
                                    case (locI, ptAmb, gLevel) => (vs(locI), ptAmb, gLevel)
                                })
                            }.start()

                        case _ => Swing.onEDT(resultsTable.model.addRow(
                            Array[AnyRef](
                                alg.toString(),
                                file,
                                ambCap.toString,
                                gas.toString,/**/
                                result.toString,
                                t.toString
                            )))
                    }
            }
        }
    })

    lazy val algorithmComboBox = new ComboBox[Algorithm](Algorithm.values)

    lazy val depthLimitedLimit = new TextField() {
        preferredSize = (100, preferredSize.height)
    }

    lazy val gasTankCapacity = new TextField() {
        preferredSize = (100, preferredSize.height)
    }

    lazy val ambulanceCapacity = new TextField() {
        preferredSize = (100, preferredSize.height)
    }

    val keyEvent = new AWTKeyAdapter {
        override def keyTyped(e: AWTKeyEvent): Unit = {
            Swing.onEDT( shouldRunButtonBeEnabled() )
        }
    }

    depthLimitedLimit.peer.addKeyListener(keyEvent)
    gasTankCapacity.peer.addKeyListener(keyEvent)
    ambulanceCapacity.peer.addKeyListener(keyEvent)

    lazy val depthLimitedForm = new FlowPanel(FlowPanel.Alignment.Right)(new Label("Limit: "), depthLimitedLimit)

    lazy val contentPane = new BoxPanel(Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new ScrollPane(loadedMaps)
            contents += new BorderPanel {
                add(new BoxPanel(Orientation.Vertical) {

                    contents += new FlowPanel(FlowPanel.Alignment.Center)(new Label("Options"))

                    contents += Swing.VStrut(5)

                    contents += new Separator
                    contents += Swing.VStrut(20)

                    contents += new FlowPanel(FlowPanel.Alignment.Right)(new Label("Ambulance Capacity: "), ambulanceCapacity)

                    contents += new FlowPanel(FlowPanel.Alignment.Right)(new Label("Gas Tank Capacity: "), gasTankCapacity)

                    contents += Swing.VStrut(20)
                    contents += new Separator
                    contents += Swing.VStrut(20)

                    contents += new FlowPanel(FlowPanel.Alignment.Right)(new Label("Algorithm: "), algorithmComboBox)
                    contents += depthLimitedForm

                    contents += new FlowPanel(FlowPanel.Alignment.Center)(runButton)
                    contents += new FlowPanel(FlowPanel.Alignment.Center)(new Button(Action("Generate Chart"){
                        val data = new DefaultCategoryDataset
                        val data1 = new DefaultCategoryDataset

                        for (i <- 0 until resultsTable.model.getRowCount) {
                            data.addValue(resultsTable.model.getValueAt(i, 6).asInstanceOf[String].toInt, "Number Expanded Nodes", resultsTable.model.getValueAt(i, 0).toString)
                            data.addValue(null, "Time", resultsTable.model.getValueAt(i, 0).toString)

                            data1.addValue(null, "Number Expanded Nodes", resultsTable.model.getValueAt(i, 0).toString)
                            data1.addValue(resultsTable.model.getValueAt(i, 5).asInstanceOf[String].toInt, "Time", resultsTable.model.getValueAt(i, 0).toString)
                        }

                        val chart1 = {
                            val domainAxis = new CategoryAxis("Algorithm")
                            val rangeAxis = new NumberAxis("# Expanded Nodes")
                            val renderer1 = new BarRenderer
                            renderer1.setMaximumBarWidth(0.45)
                            val plot1 = new CategoryPlot(data, domainAxis, rangeAxis, renderer1) {
                                override def getLegendItems: LegendItemCollection = {
                                    val result = new LegendItemCollection


                                    for {i <- 0 until getDatasetCount } {
                                        val dataS = getDataset(i)
                                        if (dataS != null) {
                                            val r = getRenderer(i)
                                            if (r != null) result.add(r.getLegendItem(i, i))
                                        }
                                    }

                                    result
                                }
                            }

                            new scalax.chart.Chart {
                                override def peer: Imports.JFreeChart = new JFreeChart("", plot1)

                                override def plot: Plot = plot1

                                override type Plot = plot1.type

                                plot.setDataset(1, data1);
                                plot.mapDatasetToRangeAxis(1, 1);

                                val axis2 = new NumberAxis("Time");
                                plot.setRangeAxis(1, axis2);
                                plot.setRangeAxisLocation(1, AxisLocation.BOTTOM_OR_RIGHT);
                                val renderer2 = new BarRenderer();
                                renderer2.setMaximumBarWidth(0.45)
                                plot.setRenderer(1, renderer2);
                            }
                        }




                        chart1.show()
                    }))

                }, BorderPanel.Position.North)
                maximumSize = (contents.map(_.preferredSize.width).max, maximumSize.height)

                contents.foreach(c => c.preferredSize = (maximumSize.width, c.preferredSize.height))
            }
        }
        contents += new ScrollPane(resultsTable)
    }

    listenTo(loadedMaps.selection, resultsTable.selection, algorithmComboBox.selection, gasTankCapacity, ambulanceCapacity, depthLimitedLimit)

    reactions += {
        case TableRowsSelected(`loadedMaps`, range, adjusting) =>
            shouldRunButtonBeEnabled()
        case TableColumnHeaderSelected(source, columnIndex, 2) =>
            val column = source.peer.getColumnModel.getColumn(columnIndex)

            val header = source.peer.getTableHeader
            val defaultHeaderRenderer = if (header != null) header.getDefaultRenderer else null
            val h = {
                val h = column.getHeaderRenderer
                if (h != null) h else defaultHeaderRenderer
            }

            column.setPreferredWidth((
                h.getTableCellRendererComponent(source.peer, column.getHeaderValue, false, false, -1, columnIndex).getPreferredSize.width +:
                    (for {row <- 0 until source.model.getRowCount} yield {
                        source.peer.getCellRenderer(row, columnIndex).getTableCellRendererComponent(source.peer, source.model.getValueAt(row, columnIndex), false, false, row, columnIndex).getPreferredSize.width
                    })).max)

        case SelectionChanged(`algorithmComboBox`) =>
            depthLimitedForm.visible = algorithmComboBox.selection.item == DepthLimited

        case EditDone(`gasTankCapacity`) =>
            shouldRunButtonBeEnabled()
    }

    def shouldRunButtonBeEnabled() = {
        runButton.enabled = !loadedMaps.selection.rows.isEmpty &&
            gasTankCapacity.text.matches("[0-9]+(.[0-9]+)?") &&
            ambulanceCapacity.text.matches("[0-9]+") &&
            (algorithmComboBox.selection.item != DepthLimited || depthLimitedLimit.text.matches("[0-9]+"))
    }

    def addMap(file: String): Unit = {
        val g = PatientTransportationProblem.readGraph(file)
        loadedMaps.model.addRow(Array[AnyRef](
            file,
            g.vertices.size.toString,
            g.vertices.count(problem.Location.isPatientLocation).toString,
            g.vertices.count(problem.Location.isFiliation).toString,
            g.vertices.count(problem.Location.isGasStation).toString,
            g.edges.size.toString
        ))
    }

}


