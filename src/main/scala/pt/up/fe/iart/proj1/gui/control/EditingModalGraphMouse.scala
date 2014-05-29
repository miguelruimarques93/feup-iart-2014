package pt.up.fe.iart.proj1.gui.control

import java.awt
import awt.ItemSelectable
import java.awt.event._
import java.awt.Cursor
import java.awt.Component
import java.awt.geom.Point2D

import edu.uci.ics.jung
import jung.visualization.RenderContext
import jung.visualization.control._
import jung.visualization.annotations.AnnotatingGraphMousePlugin
import scala.swing.Dimension
import javax.swing.{ButtonGroup, JRadioButtonMenuItem, JMenu, JComboBox}
import javax.swing.plaf.basic.BasicIconFactory

class EditingModalGraphMouse[V, E](rc: RenderContext[V, E], vertexFactory: (Point2D) => V, edgeFactory: (V, V) => E, in: Float = 1.1f, out: Float= 1/1.1f)
    extends AbstractModalGraphMouse(in, out)
    with ModalGraphMouse
    with ItemSelectable
{
    import ModalGraphMouse.Mode

    protected var editingPlugin : EditingMousePlugin[V, E] = null
    protected var labelEditingPlugin : LabelEditingGraphMousePlugin[V, E] = null
    protected var _popupEditingPlugin : EditingPopupMousePlugin[V, E] = null

    def popupEditingPlugin = _popupEditingPlugin

    protected val basicTransformer = rc.getMultiLayerTransformer

    override def loadPlugins(): Unit = {
        pickingPlugin = new PickingMousePlugin[V, E]()
        animatedPickingPlugin = new AnimatedPickingGraphMousePlugin[V, E]
        translatingPlugin = new TranslatingGraphMousePlugin(InputEvent.BUTTON1_MASK)
        scalingPlugin = new ScalingGraphMousePlugin(new CrossoverScalingControl(), 0, in, out)
        rotatingPlugin = new RotatingGraphMousePlugin()
        shearingPlugin = new ShearingGraphMousePlugin()
        editingPlugin = new EditingMousePlugin[V, E](vertexFactory, edgeFactory)
        labelEditingPlugin = new LabelEditingGraphMousePlugin[V, E]()
        _popupEditingPlugin = new EditingPopupMousePlugin[V, E](vertexFactory)
        add(scalingPlugin)
        setMode(ModalGraphMouse.Mode.EDITING)
    }

    override def setMode(mode: Mode): Unit =
        if (this.mode != mode) {
            fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED, this.mode, ItemEvent.DESELECTED))
            this.mode = mode
            mode match {
                case ModalGraphMouse.Mode.TRANSFORMING => setTransformingMode()
                case ModalGraphMouse.Mode.PICKING => setPickingMode()
                case ModalGraphMouse.Mode.EDITING => setEditingMode()
                case ModalGraphMouse.Mode.ANNOTATING => setAnnotatingMode()
            }

            if (modeBox != null) modeBox.setSelectedIndex(mode.ordinal())
            fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED, this.mode, ItemEvent.SELECTED))
        }

    override protected def setPickingMode() : Unit = {
        remove(translatingPlugin)
        remove(rotatingPlugin)
        remove(shearingPlugin)
        remove(editingPlugin)
        add(pickingPlugin)
        add(animatedPickingPlugin)
        add(labelEditingPlugin)
        add(_popupEditingPlugin)
    }

    override protected def setTransformingMode() : Unit = {
        remove(pickingPlugin)
        remove(animatedPickingPlugin)
        remove(editingPlugin)
        add(translatingPlugin)
        add(rotatingPlugin)
        add(shearingPlugin)
        add(labelEditingPlugin)
        add(_popupEditingPlugin)
    }

    protected def setEditingMode() : Unit = {
        remove(pickingPlugin)
        remove(animatedPickingPlugin)
        remove(translatingPlugin)
        remove(rotatingPlugin)
        remove(shearingPlugin)
        remove(labelEditingPlugin)
        add(editingPlugin)
        add(_popupEditingPlugin)
    }

    protected def setAnnotatingMode() : Unit = {
        remove(pickingPlugin)
        remove(animatedPickingPlugin)
        remove(translatingPlugin)
        remove(rotatingPlugin)
        remove(shearingPlugin)
        remove(labelEditingPlugin)
        remove(editingPlugin)
        remove(_popupEditingPlugin)
    }

    override def getModeComboBox: JComboBox[_] = {
        if (modeBox == null) {
            modeBox = new JComboBox[Mode](Mode.values())
            modeBox addItemListener getModeListener
        }
        modeBox setSelectedItem mode
        modeBox
    }

    override def getModeMenu: JMenu = {
        if (modeMenu == null) {
            modeMenu = new JMenu
            val icon = BasicIconFactory.getMenuArrowIcon
            modeMenu.setIcon(BasicIconFactory.getMenuArrowIcon)
            modeMenu.setPreferredSize(new Dimension(icon.getIconWidth + 10, icon.getIconHeight + 10))

            val transformingButton = new JRadioButtonMenuItem(Mode.TRANSFORMING.toString)
            transformingButton.addItemListener(new ItemListener {
                override def itemStateChanged(e: ItemEvent): Unit =
                    if (e.getStateChange == ItemEvent.SELECTED)
                        setMode(Mode.TRANSFORMING)
            })

            val pickingButton = new JRadioButtonMenuItem(Mode.PICKING.toString)
            pickingButton.addItemListener(new ItemListener {
                override def itemStateChanged(e: ItemEvent): Unit =
                    if (e.getStateChange == ItemEvent.SELECTED)
                        setMode(Mode.PICKING)
            })

            val editingButton = new JRadioButtonMenuItem(Mode.EDITING.toString)
            editingButton.addItemListener(new ItemListener {
                override def itemStateChanged(e: ItemEvent): Unit =
                    if (e.getStateChange == ItemEvent.SELECTED)
                        setMode(Mode.EDITING)
            })

            val radio = new ButtonGroup

            radio add transformingButton
            radio add pickingButton
            radio add editingButton

            transformingButton setSelected true

            modeMenu add transformingButton
            modeMenu add pickingButton
            modeMenu add editingButton
            modeMenu setToolTipText "Menu for setting Mouse Mode"

            addItemListener(new ItemListener {
                override def itemStateChanged(e: ItemEvent): Unit =
                    if (e.getStateChange == ItemEvent.SELECTED) e.getItem match {
                        case Mode.TRANSFORMING => transformingButton setSelected true
                        case Mode.PICKING => pickingButton setSelected true
                        case Mode.EDITING => editingButton setSelected true
                    }
            })
        }
        modeMenu
    }

    class ModeKeyAdapter(graphMouse: ModalGraphMouse, t: Char = 't', p: Char = 'p', e: Char = 'e', a: Char = 'a') extends KeyAdapter {
        override def keyTyped(event: KeyEvent): Unit = {
            val keyChar = event.getKeyChar
            if(keyChar == t) {
                event.getSource.asInstanceOf[Component].setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR))
                graphMouse.setMode(Mode.TRANSFORMING)
            } else if(keyChar == p) {
                event.getSource.asInstanceOf[Component].setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
                graphMouse.setMode(Mode.PICKING)
            } else if(keyChar == e) {
                event.getSource.asInstanceOf[Component].setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR))
                graphMouse.setMode(Mode.EDITING)
            }
        }
    }

    loadPlugins()

    private val mka = new ModeKeyAdapter(this)


    def lockMode() = setModeKeyListener(null)
    def unlockMode() = setModeKeyListener(mka)

    unlockMode()

    def movePublisher = pickingPlugin.asInstanceOf[PickingMousePlugin[V, E]]
    def popupPublisher = _popupEditingPlugin
}
