/*
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU Lesser General Public License as published
 *    by the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.violetlib.treetable.ui;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Composite;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.dnd.DragSource;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.Enumeration;
import java.util.EventObject;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractCellEditor;
import javax.swing.CellEditor;
import javax.swing.CellRendererPane;
import javax.swing.DefaultListSelectionModel;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.LookAndFeel;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.TransferHandler;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.RowSorter.SortKey;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.RowSorterListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TableModelEvent;
import javax.swing.plaf.TreeUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.plaf.synth.Region;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.text.Position;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.violetlib.aqua.AquaUtils;
import org.violetlib.treetable.TreeTable;
import org.violetlib.treetable.TreeTable.DropLocation;
import org.violetlib.treetable.CellEditorContainer;
import org.violetlib.treetable.DefaultTreeTableCellEditor;
import org.violetlib.treetable.DefaultTreeTableCellRenderer;
import org.violetlib.treetable.TreeColumnModel;
import org.violetlib.treetable.TreeTableCellEditor;
import org.violetlib.treetable.TreeTableCellRenderer;
import org.violetlib.treetable.TreeTableSorter;

public class BasicTreeTableUI extends TreeTableUI {

    public BasicTreeTableUI() {
    }

    @Override
    public void installUI(JComponent c) {
        treeTable = (TreeTable)c;
        installDefaults();
        installComponents();
        installListeners();
    }

    @Override
    public void uninstallUI(JComponent c) {
        uninstallListeners();
        uninstallComponents();
        uninstallDefaults();
        tree = null;
        table = null;
        treeTable = null;
    }

    protected void installDefaults() {
        AquaUtils.installFont(treeTable,"Table.font");
        if (treeTable.getAlternateRowColor() == null || treeTable.getAlternateRowColor() instanceof UIResource) {
            Color c = UIManager.getColor("Table.alternateRowColor");
            treeTable.setAlternateRowColor(c);
        }
    }

    protected void uninstallDefaults() {
    }

    protected void installComponents() {
        handler = createHandler();
        TreeTableCellRenderer focusRenderer = treeTable.getFocusRenderer();
        if (focusRenderer == null || focusRenderer instanceof UIResource)
            treeTable.setFocusRenderer(createFocusRenderer());
        tree = createAndConfigureTree();
        table = createAndConfigureTable();
        finishConfiguration(tree, table);
        treeTableCellRenderer = createCellRenderer();
        treeTableCellEditor = createCellEditor();

        defaultTreeCellRenderer = new DefaultTreeTableCellRenderer();
        tree.setCellRenderer(treeTableCellRenderer);

        if (treeTable.getRowSorter() != null)
            table.setRowSorter(new RowSorterAdapter());
    }

    protected void uninstallComponents() {
        unconfigureTree();
        unconfigureTable();
    }

    protected void installListeners() {
        focusListener = createFocusListener();
        if (focusListener != null)
            treeTable.addFocusListener(focusListener);
        keyListener = createKeyListener();
        if (keyListener != null)
            treeTable.addKeyListener(keyListener);
        mouseListener = createMouseListener();
        if (mouseListener != null)
            treeTable.addMouseListener(mouseListener);
        mouseMotionListener = createMouseMotionListener();
        if (mouseMotionListener != null)
            treeTable.addMouseMotionListener(mouseMotionListener);

        properties = getProperties();
        if (properties == null) {
            propertyChangeListener = createPropertyChangeListener();
            if (propertyChangeListener != null)
                treeTable.addPropertyChangeListener(propertyChangeListener);
        } else if (!properties.isEmpty()) {
            propertyChangeListener = createPropertyChangeListener();
            if (propertyChangeListener != null)
                for (String name : properties)
                    treeTable.addPropertyChangeListener(name, propertyChangeListener);
        }
        if (propertyChangeListener == null)
            properties = Collections.emptyList();
    }

    protected void uninstallListeners() {
        if (focusListener != null) {
            treeTable.removeFocusListener(focusListener);
            focusListener = null;
        }
        if (keyListener != null) {
            treeTable.removeKeyListener(keyListener);
            keyListener = null;
        }
        if (mouseListener != null) {
            treeTable.removeMouseListener(mouseListener);
            mouseListener = null;
        }
        if (mouseMotionListener != null) {
            treeTable.removeMouseMotionListener(mouseMotionListener);
            mouseMotionListener = null;
        }
        if (propertyChangeListener != null) {
            if (properties == null) {
                treeTable.removePropertyChangeListener(propertyChangeListener);
            } else {
                for (String name : properties)
                    treeTable.removePropertyChangeListener(
                            name, propertyChangeListener);
            }
            properties = null;
            propertyChangeListener = null;
        }
    }

    protected TreeTable treeTable;

    private JTree tree;

    private JTable table;

    protected Renderer treeTableCellRenderer;

    protected Editor treeTableCellEditor;

    protected DefaultTreeTableCellRenderer defaultTreeCellRenderer;

    protected Handler handler;

    private FocusListener focusListener;

    private KeyListener keyListener;

    private MouseListener mouseListener;

    private MouseMotionListener mouseMotionListener;

    private List<String> properties;

    private PropertyChangeListener propertyChangeListener;

    public void updateUI() {
        sortWidth = -1;
        noFocusBorder = null;
        ltrBorder = null;
        rtlBorder = null;
        installDefaults();
        tree.updateUI();
        table.updateUI();
        defaultTreeCellRenderer.updateUI();
        updateTreeClientProperties(tree);
        treeTableCellRenderer = createCellRenderer();
        tree.setCellRenderer(treeTableCellRenderer);
    }

    protected Renderer createCellRenderer() {
        return isNimbus() ? new NimbusRenderer() : new Renderer();
    }

    protected Editor createCellEditor() {
        return new Editor();
    }

    protected TreeTableCellRenderer createFocusRenderer() {
        return new FocusRenderer();
    }

    protected JTree createAndConfigureTree() {
        JTree tree = createTree(treeTable.getTreeTableModel());
        if (treeTable.getSelectionModel() == null) {
            treeTable.setSelectionModel(tree.getSelectionModel());
        } else {
            tree.setSelectionModel(treeTable.getSelectionModel());
        }
        tree.setOpaque(false);
        tree.setRowHeight(20);
        tree.putClientProperty("JTree.lineStyle", "None");
        InputMap inputs = tree.getInputMap();
        remap(inputs, KeyEvent.VK_LEFT);
        remap(inputs, KeyEvent.VK_RIGHT);
        updateTreeClientProperties(tree);
        return tree;
    }

    private void updateTreeClientProperties(JTree tree) {
//		if (isNimbus()) {
//			UIDefaults map = new UIDefaults();
//			// Problematic for 1.6 & 1.7 compatibility
//            // Use raw types to allow compilation in 1.6 and 1.7
//			Painter painter = new Painter() {
//				public void paint(Graphics2D g, Object c, int w, int h) {}
//			};
//
//			map.put("Tree:TreeCell[Enabled+Selected].backgroundPainter", painter);
//			map.put("Tree:TreeCell[Focused+Selected].backgroundPainter", painter);
////			map.put("\"Tree.cellEditor\"[Enabled+Focused].backgroundPainter", painter);
//
//			tree.putClientProperty("Nimbus.Overrides", map);
//		} else {
//			tree.putClientProperty("Nimus.Overrides", null);
//		}
    }

    private boolean isNimbus() {
        // TODO, change to class path
        return "Nimbus".equals(UIManager.getLookAndFeel().getName());
    }

    private void remap(InputMap inputs, int code) {
        Object key = inputs.get(KeyStroke.getKeyStroke(code, 0));
        if (key != null)
            inputs.put(KeyStroke.getKeyStroke(
                    code, InputEvent.ALT_DOWN_MASK), key);
    }

    protected JTree createTree(TreeModel tm) {
        return new Tree(tm);
    }

    protected void unconfigureTree() {
        tree.setCellRenderer(null);
        tree.setModel(new DefaultTreeModel(null));
        tree.setSelectionModel(new DefaultTreeSelectionModel());
        tree.setUI(null);
    }

    protected JTable createAndConfigureTable() {
        TableColumnModel cm = treeTable.getColumnModel();
        JTable table = createTable(treeTable.getTreeTableModel(),
                cm, treeTable.getRowSelectionModel());
        table.setOpaque(false);
        table.setFillsViewportHeight(true);
        table.setShowHorizontalLines(false);
        table.setRowMargin(0);
        if (cm == null) {
            cm = table.getColumnModel();
            cm.setColumnMargin(isNimbus() ? 0 : 3);
            int hc = treeTable.getTreeColumnModel().getHierarchicalColumn();
            if (hc >= 0)
                cm.getColumn(hc).setPreferredWidth(150);
            treeTable.setColumnModel(cm);
        }
        table.setShowVerticalLines(false);
        table.setRowHeight(20);
        table.setColumnModel(new ColumnModelAdapter(cm));
        return table;
    }

    /**
     * Called after the tree and the table have both been created and configured.
     */
    protected void finishConfiguration(JTree tree, JTable table) {
    }

    protected JTable createTable(TableModel tm,
                                 TableColumnModel cm, ListSelectionModel sm) {
        return new Table(tm, cm, sm);
    }

    protected void unconfigureTable() {
        table.setTableHeader(null);
        table.setModel(new DefaultTableModel());
        table.setColumnModel(new DefaultTableColumnModel());
        table.setSelectionModel(new DefaultListSelectionModel());
        table.setUI(null);
    }

    protected FocusListener createFocusListener() {
        return handler;
    }

    protected KeyListener createKeyListener() {
        return handler;
    }

    protected MouseListener createMouseListener() {
        return handler;
    }

    protected MouseMotionListener createMouseMotionListener() {
        return handler;
    }

    /**
     * If null, the created PropertyChangeListener will be global.
     * <p>
     * If Collections.EMPTY_LIST (or any empty list),
     * a PropertyChangeListener will not be installed.
     * <p>
     * Otherwise, only the specified properties will be listened to.
     *
     * @return properties to listen to.
     */
    protected List<String> getProperties() {
        return java.util.Arrays.asList("componentOrientation", "enabled", "rowSorter", "JTree.lineStyle", "columnModel");
    }

    protected PropertyChangeListener createPropertyChangeListener() {
        return handler;
    }

    protected Handler createHandler() {
        return new Handler();
    }


    protected JTable getTable() {
        return table;
    }

    protected JTree getTree() {
        return tree;
    }

    @Override
    public TreeInterface getTreeInterface(TreeTable treeTable) {
        return (TreeInterface)tree;
    }

    @Override
    public TableInterface getTableInterface(TreeTable treeTable) {
        return (TableInterface)table;
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        Dimension size = tree.getPreferredSize();
        size.width = table.getPreferredSize().width;
        return size;
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        try {
            paintTable(g);
            if (treeTable.getRowCount() > 0) {
                paintTree(g);
                if (treeTable.isNodeSortingEnabled())
                    paintSortIndicators(g);
            }
            if (!treeTable.isPaintingForPrint()) {
                if (treeTable.isFocusOwner())
                    paintFocus(g);
                paintDropLines(g);
            }
        } finally {
            treeTableCellRenderer.clearState();
        }
    }

    protected void paintTable(Graphics g) {
        treeTableCellRenderer.prepareForTable(g);
        Graphics cg = g.create(0, 0, treeTable.getWidth(), treeTable.getHeight());
        try {
            table.paint(cg);
        } finally {
            cg.dispose();
        }

        // JTable doesn't paint anything for the editing cell,
        // so painting the background color is placed here
        if (table.isEditing()) {
            int col = table.getEditingColumn();
            int row = table.getEditingRow();
            boolean sel = treeTableCellRenderer.isSelected(
                    tree.isRowSelected(row) && table.isColumnSelected(col));
            TreeTableCellRenderer r = treeTable.getCellRenderer(row, col);
            Component c = r.getTreeTableCellRendererComponent(
                    treeTable, null, sel, false, row, col);
            // TODO, create CellRendererPane for TreeTable, for use by the focus renderer too.
            CellRendererPane rp = (CellRendererPane)table.getComponent(0);
            Rectangle cell = table.getCellRect(row, col, true);
            rp.paintComponent(g, c, table, cell.x, cell.y, cell.width, cell.height, true);
            rp.removeAll();
        }
    }

    protected void paintTree(Graphics g) {
        Shape clip = g.getClip();
        if (tree.getWidth() <= 0 || !clip.intersects(tree.getBounds()))
            return;

        JTableHeader header = table.getTableHeader();
        int x = tree.getX();
        int clipX = 0;
        int clipW = tree.getWidth();
        if (header != null) {
            TableColumn dc = header.getDraggedColumn();
            if (dc != null) {
                if (dc.getModelIndex() == treeTable.getTreeColumnModel().getHierarchicalColumn()) {
                    // shift x distance for painting tree
                    x += header.getDraggedDistance();
                } else {
                    // see if dragged column overlaps tree
                    // if so, adjust clipX and clipW
                    int col = table.convertColumnIndexToView(dc.getModelIndex());
                    Rectangle r = table.getCellRect(0, col, true);
                    int dragX0 = r.x + header.getDraggedDistance();
                    int dragX1 = dragX0 + r.width;
                    if (x >= dragX0 && x < dragX1) {
                        clipX = dragX1 - x;
                        clipW -= clipX;
                    } else if (x < dragX0 && dragX0 < x+tree.getWidth()) {
                        clipW -= x + tree.getWidth() - dragX0;
                    }
                }
            }
        }

        treeTableCellRenderer.prepareForTree();
        Graphics cg = g.create(x, 0, tree.getWidth(), tree.getHeight());
        try {
            cg.clipRect(clipX, 0, clipW, tree.getHeight());
            tree.paint(cg);
        } finally {
            cg.dispose();
        }
    }

    protected void paintSortIndicators(Graphics g) {
        TreePath mouseOverPath = paintMouseOverSortPath ? mouseOverSortPath : null;
        int mouseOverColumn = mouseOverSortColumn;
        SortOrder order = null;
        for (Map.Entry<TreePath,SortKey> entry : treeTable.getSortedPaths().entrySet()) {
            TreePath path = entry.getKey();
            if (!tree.isExpanded(path))
                continue;
            SortKey key = entry.getValue();
            int col = table.convertColumnIndexToView(key.getColumn());
            if (col < 0)
                continue;
            if (mouseOverPath != null && col == mouseOverColumn && mouseOverPath.equals(path)) {
                order = entry.getValue().getSortOrder();
                continue;
            }
            paintSortIndicator(g, path, col, key.getSortOrder());
        }
        if (mouseOverPath != null) {
            RowSorter<?> sorter = treeTable.getRowSorter().getRowSorter(mouseOverPath);
            if (sorter instanceof TreeTableSorter.SortCycle) {
                List<SortOrder> cycle = ((TreeTableSorter.SortCycle)sorter).getSortCycle();
                if (order == null) {
                    order = cycle.get(0);
                } else {
                    int idx = cycle.indexOf(order);
                    if (idx < 0 || ++idx >= cycle.size())
                        idx = 0;
                    order = cycle.get(idx);
                }
            } else {
                order = order == SortOrder.ASCENDING ? SortOrder.DESCENDING : SortOrder.ASCENDING;
            }
            if (order == SortOrder.UNSORTED) {
                int row = tree.getRowForPath(mouseOverPath);
                Rectangle r = table.getCellRect(row, mouseOverColumn, true);
                int h = 2;
                int w = getSortWidth() - getSortMargin()*2;
                int y = r.y + (r.height-h)/2;
                int align = treeTable.getAlignment(getRendererComponent(false, false, row, mouseOverColumn), row, mouseOverColumn);
                g.setColor(Color.GRAY);
                switch (align) {
                    case SwingConstants.CENTER:
                    case SwingConstants.LEFT:
                        g.fillRect(r.x+r.width-w-getSortMargin(), y, w, h);
                        break;
                    case SwingConstants.RIGHT:
                        g.fillRect(r.x+getSortMargin(), y, w, h);
                        break;
                }
            } else {
                Graphics2D g2 = null;
                Composite comp = null;
                if (g instanceof Graphics2D) {
                    g2 = (Graphics2D)g;
                    comp = g2.getComposite();
                    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
                }
                paintSortIndicator(g, mouseOverPath, mouseOverSortColumn, order);
                if (g2 != null)
                    g2.setComposite(comp);
            }
        }
    }

    private void paintSortIndicator(Graphics g, TreePath path, int col, SortOrder sortOrder) {
        Icon icon;
        switch (sortOrder) {
            default: return;
            case ASCENDING: icon = treeTable.getAscendingSortIcon(); break;
            case DESCENDING: icon = treeTable.getDescendingSortIcon(); break;
        }
        int row = tree.getRowForPath(path);
        Rectangle r = table.getCellRect(row, col, true);
        int y = r.y + (r.height-icon.getIconHeight())/2;
        int align = treeTable.getAlignment(getRendererComponent(false, false, row, col), row, col);
        switch (align) {
            case SwingConstants.CENTER:
            case SwingConstants.LEFT:
                icon.paintIcon(treeTable, g, r.x+r.width-icon.getIconWidth()-getSortMargin(), y);
                break;
            case SwingConstants.RIGHT:
                icon.paintIcon(treeTable, g, r.x+getSortMargin(), y);
                break;
        }
    }

    private TreePath mouseOverSortPath;

    private int mouseOverSortColumn;

    private boolean paintMouseOverSortPath;

    void setMouseOverSortIndicator(TreePath path, int col) {
        TreePath oldPath = mouseOverSortPath;
        int oldColumn = mouseOverSortColumn;
        if (col != oldColumn ||
                (path == null ? oldPath != null : !path.equals(oldPath))) {
            mouseOverSortColumn = col;
            mouseOverSortPath = path;
            paintMouseOverSortPath = true;
            if (oldPath != null)
                repaint(oldPath, oldColumn);
            if (path != null)
                repaint(path, col);
        }
    }

    private void repaint(TreePath path, int col) {
        Rectangle r = table.getCellRect(tree.getRowForPath(path), col, true);
        treeTable.repaint(r);
    }

    private Component getRendererComponent(boolean sel, boolean foc, int row, int col) {
        TreeTableCellRenderer r = treeTable.getCellRenderer(row, col);
        Object value = treeTable.getValueAt(row, col);
        if (col == treeTable.getHierarchicalColumn()) {
            TreePath path = tree.getPathForRow(row);
            return r.getTreeTableCellRendererComponent(
                    treeTable, value, sel, foc, row, col,
                    treeTable.isExpanded(path), treeTable.isLeaf(path));
        }
        return r.getTreeTableCellRendererComponent(
                treeTable, value, sel, foc, row, col);
    }

    protected void paintFocus(Graphics g) {
        // TODO, need to use CellRendererPane for focusRenderer?
        // TODO, drag clipping is too simplistic, i.e. wrong.
        // it is possible for a dragged column to be "land locked"
        // on top of another if the bottom column has a large enough width
        TreeTableCellRenderer focusRenderer = treeTable.getFocusRenderer();
        if (focusRenderer == null)
            return;
        if (treeTable.isColumnFocusEnabled()) {
            int leadColumn = treeTable.getLeadSelectionColumn();
            if (leadColumn < 0)
                return;
            int leadRow = treeTable.getLeadSelectionRow();
            Rectangle r = table.getCellRect(leadRow, leadColumn, true);
            Rectangle clipR = null;
            JTableHeader header = table.getTableHeader();
            if (header != null) {
                TableColumn dc = header.getDraggedColumn();
                if (dc != null && header.getDraggedDistance() != 0) {
                    if (dc.getModelIndex() == treeTable.getTreeColumnModel().getHierarchicalColumn()) {
                        r.x += header.getDraggedDistance();
                    } else {
                        int col = table.convertColumnIndexToView(dc.getModelIndex());
                        Rectangle dr = table.getCellRect(leadRow, col, true);
                        dr.x += header.getDraggedDistance();
                        clipR = r.intersection(dr);
                        clipR.x -= r.x;
                    }
                }
            }
            if (!g.getClip().intersects(r))
                return;
            Component c = focusRenderer.getTreeTableCellRendererComponent(
                    treeTable, "", false, true, leadRow, leadColumn);
            c.setBounds(0, 0, r.width, r.height);
            Graphics cg = g.create(r.x, r.y, r.width, r.height);
            try {
                if (clipR != null) {
                    // clipR is actually the inverse of the "clip"
                    if (clipR.x == 0) {
                        cg.setClip(clipR.width, 0, r.width, r.height);
                    } else {
                        cg.setClip(0, 0, clipR.x, r.height);
                    }
                }
                c.paint(cg);
            } finally {
                cg.dispose();
            }
        } else {
            // paint focus around lead row
            int row = tree.getRowForPath(tree.getLeadSelectionPath());
            int columns = table.getColumnModel().getColumnCount();
            if (row >= 0 && columns > 0) {
                Rectangle r = table.getCellRect(row, 0, true);
                if (columns > 1)
                    r.add(table.getCellRect(row, columns-1, true));
                if (g.getClip().intersects(r)) {
                    Component c = focusRenderer.getTreeTableCellRendererComponent(
                            treeTable, "", false, true, row, -1);
                    c.setBounds(0, 0, r.width, r.height);
                    Graphics cg = g.create(r.x, r.y, r.width, r.height);
                    try {
                        boolean paintFullRow = true;
                        JTableHeader header = table.getTableHeader();
                        if (header != null) {
                            TableColumn dc = header.getDraggedColumn();
                            if (dc != null && header.getDraggedDistance() != 0) {
                                // only paint focus over valid column bounds
                                paintFullRow = false;
                                int col = table.convertColumnIndexToView(dc.getModelIndex());
                                Rectangle dr = table.getCellRect(row, col, true);
                                if (header.getDraggedDistance() < 0) {
                                    int w = dr.x + dr.width + header.getDraggedDistance();
                                    cg.setClip(0, 0, w, r.height);
                                    c.paint(cg);
                                    int x = dr.x + dr.width;
                                    cg.setClip(x, 0, r.width - x, r.height);
                                    c.paint(cg);
                                } else {
                                    cg.setClip(0, 0, dr.x, r.height);
                                    c.paint(cg);
                                    int x = dr.x + header.getDraggedDistance();
                                    cg.setClip(x, 0, r.width - x, r.height);
                                    c.paint(cg);
                                }
                            }
                        }
                        if (paintFullRow)
                            c.paint(cg);
                    } finally {
                        cg.dispose();
                    }
                }
            }
        }
    }

    protected void layoutTable() {
        table.setBounds(0, 0, treeTable.getWidth(), treeTable.getHeight());
    }

    protected void layoutTree() {
        int hier = treeTable.getHierarchicalColumn();
        if (hier < 0) {
            tree.setBounds(0, 0, 0, 0);
        } else {
            Rectangle r = table.getCellRect(-1, hier, true);
            int cm = treeTable.getColumnModel().getColumnMargin();
            r.x += cm/2;
            r.width -= cm;
            r.height = treeTable.getHeight();
            tree.setBounds(r);
        }
    }

    @Override
    public void configureCellRenderer(DefaultTreeTableCellRenderer renderer,
                                      TreeTable treeTable, Object value, boolean selected,
                                      boolean hasFocus, int row, int column) {
        treeTableCellRenderer.configureCellRenderer(renderer,
                treeTable, value, selected, hasFocus, row, column);
//		renderer.getTableCellRendererComponent(
//				table, value, selected, hasFocus, row, column);
    }

    @Override
    public void configureCellRenderer(DefaultTreeTableCellRenderer renderer,
                                      TreeTable treeTable, Object value, boolean selected,
                                      boolean hasFocus, int row, int column, boolean expanded,
                                      boolean leaf) {
        treeTableCellRenderer.configureCellRenderer(renderer, treeTable,
                value, selected, hasFocus, row, column, expanded, leaf);
//		renderer.getTreeCellRendererComponent(
//				tree, value, selected, expanded, leaf, row, hasFocus);
    }

    @Override
    public void configureCellEditor(DefaultTreeTableCellEditor editor,
                                    TreeTable treeTable, Object value, boolean selected, int row, int column) {
        editor.getTableCellEditorComponent(
                table, value, selected, row, column);
    }

    @Override
    public void configureCellEditor(DefaultTreeTableCellEditor editor,
                                    TreeTable treeTable, Object value, boolean selected,
                                    int row, int column, boolean expanded, boolean leaf) {
        editor.getTreeCellEditorComponent(
                tree, value, selected, expanded, leaf, row);
    }

    @Override
    public TreeTableCellRenderer getDefaultRenderer(TreeTable treeTable, Class<?> columnClass) {
        treeTableCellRenderer.loadRenderer(columnClass);
        return treeTableCellRenderer;
    }

    @Override
    public TreeTableCellEditor getDefaultEditor(TreeTable treeTable, Class<?> columnClass, int column) {
        treeTableCellEditor.loadEditor(columnClass, column);
        return treeTableCellEditor;
    }

    @Override
    public Rectangle getPathBounds(TreeTable treeTable, TreePath path) {
        Rectangle r = tree.getPathBounds(path);
        if (r != null) {
            r.x += tree.getX();
            r.y += tree.getY();
        }
        return r;
    }

    @Override
    public TreePath getPathForLocation(TreeTable treeTable, int x, int y) {
        TreePath path = tree.getPathForLocation(x-tree.getX(), y-tree.getY());
        if (path != null) {
            return path;
        }
        int row = table.rowAtPoint(new Point(x, y));
        return row >= 0 ? tree.getPathForRow(row) : null;
    }

    @Override
    public TreePath getClosestPathForLocation(TreeTable treeTable, int x, int y) {
        return tree.getClosestPathForLocation(x-tree.getX(), y-tree.getY());
    }

    protected boolean hasTreeHandle(TreeTable treeTable, TreePath path) {
        return !(treeTable.isLeaf(path) ||
                (treeTable.getChildCount(path) <= 0
                        && !treeTable.hasBeenExpanded(path)));
    }

    static boolean isOverTreeHandle(boolean ltr, Rectangle nb, int thw, int x) {
        return ltr ?
                x < nb.x && (thw < 0 || x > nb.x - thw) :
                x > nb.x + nb.width && (thw < 0
                        || x < nb.x + nb.width + thw);
    }

    @Override
    public int getDistanceToTreeHandle(TreeTable treeTable, TreePath path, int x) {
        Rectangle nb = tree.getPathBounds(path);
        if (nb == null) return 0;
        boolean ltr = treeTable.getComponentOrientation().isLeftToRight();
        int treePosition;
        if (ltr ? x < nb.x : x > nb.x + nb.width) {
            // leading margin/columns
            // Check if the node has a tree handle
            boolean hasTreeHandle = hasTreeHandle(treeTable, path);

            // Check if the event location falls over the tree handle.
            if (hasTreeHandle && isOverTreeHandle(ltr, nb, getTreeHandleWidth(), x)) {
                treePosition = 0;
            } else {
                treePosition = ltr ?
                        x - nb.x :
                        nb.x + nb.width - x;
                if (hasTreeHandle)
                    treePosition += getTreeHandleWidth();
            }
        } else {
            // node & trailing margin/columns
            treePosition = ltr ?
                    x - nb.x :
                    nb.x + nb.width - x;
        }
        return treePosition;
    }

    /**
     * @param path expanded or collapsed path
     * @param interval number of rows to add or remove
     */
    protected void updateTableAfterExpansion(TreePath path, int interval) {
        if (interval < 0) {
            treeTable.processTreeCollapse(path, -interval);
        } else if (interval > 0) {
            treeTable.processTreeExpansion(path, interval);
        }
    }

    protected void scrollToVisible(Rectangle r, int x, int y) {
        r.x += x;
        r.y += y;
        treeTable.scrollRectToVisible(r);
        r.x -= x;
        r.y -= y;
    }

    protected interface ProcessKeyBinding {
        boolean processKeyBinding(KeyStroke ks, KeyEvent e, int condition, boolean pressed);
    }

    private class Tree extends JTree implements TreeInterface, ProcessKeyBinding {

        Tree(TreeModel model) {
            super(model);
        }

        /**
         * Overridden to safe-guard against external changing of the
         * cell renderer.
         */
//		public void setCellRenderer(TreeCellRenderer renderer) {
//			if (renderer != null && getCellRenderer() != null && (renderer == null || !(renderer instanceof Renderer)))
//				throw new UnsupportedOperationException();
//			super.setCellRenderer(renderer);
//		}

        @Override
        public boolean processKeyBinding(KeyStroke ks,
                                         KeyEvent e, int condition, boolean pressed) {
            return super.processKeyBinding(ks, e, condition, pressed);
        }

        public Container getParent() {
            return treeTable.getParent();
        }

        public boolean getDragEnabled ()
        {
            return treeTable.getDragEnabled();
        }

        public boolean hasFocus() {
            return false;
        }

        public void computeVisibleRect(Rectangle visibleRect) {
            treeTable.computeVisibleRect(visibleRect);
            Rectangle2D.intersect(visibleRect, getBounds(), visibleRect);
            visibleRect.x -= getX();
            visibleRect.y -= getY();
        }

        public void repaint(long tm, int x, int y, int width, int height) {
            treeTable.repaint(tm, x+getX(), y+getY(), width, height);
        }

        public void paintImmediately(int x, int y, int w, int h) {
            treeTable.paintImmediately(x+getX(), y+getY(), w, h);
        }

        public void revalidate() {
            treeTable.revalidate();
        }

        public boolean requestFocusInWindow() {
            return treeTable.requestFocusInWindow();
        }

        public void requestFocus() {
            treeTable.requestFocus();
        }

        public boolean requestFocus(boolean temporary) {
            javax.swing.plaf.basic.BasicTableUI u;
            return treeTable.requestFocus(temporary);
        }

        public void doLayout() {
            layoutTree();
            super.doLayout();
        }

        public void scrollRectToVisible(Rectangle r) {
            scrollToVisible(r, getX(), getY());
        }

        @Override
        protected void setExpandedState(TreePath path, boolean state) {
//			if (isExpanded(path) == state)
//				return;
            TreePath updatePath = path;
            if (state) {
                for (;;) {
                    TreePath par = updatePath.getParentPath();
                    if (par == null || isExpanded(par))
                        break;
                    updatePath = par;
                }
            }
            int rowCount = getRowCount();
            super.setExpandedState(path, state);
            updateTableAfterExpansion(updatePath, getRowCount() - rowCount);
        }

        @Override
        public void setUI(TreeUI ui) {
            super.setUI(ui);
            if (ui instanceof BasicTreeUI) {
                BasicTreeUI bui = (BasicTreeUI)ui;
                treeHandleWidth = bui.getLeftChildIndent() + bui.getRightChildIndent();
            } else {
                treeHandleWidth = -1;
            }
        }

        public Color getBackground() {
            return treeTable.getBackground();
        }

        public Color getForeground() {
            return treeTable.getForeground();
        }

        public Font getFont() {
            return treeTable.getFont();
        }
    }

    private class Table extends JTable implements TableInterface, ProcessKeyBinding {

        Table(TableModel tm, TableColumnModel cm, ListSelectionModel sm) {
            super(tm, cm, sm);
        }

        /**
         * Override to safe-guard against changing the model.
         */
//		public void setModel(TableModel model) {
//			if (dataModel != null)
//				throw new UnsupportedOperationException();
//			super.setModel(model);
//		}

        /**
         * Override to safe-guard against changing the selection model.
         */
//		public void setSelectionModel(ListSelectionModel model) {
//			if (selectionModel != null)
//				throw new UnsupportedOperationException();
//			super.setSelectionModel(model);
//		}

        @Override
        public boolean processKeyBinding(KeyStroke ks,
                                         KeyEvent e, int condition, boolean pressed) {
            return super.processKeyBinding(ks, e, condition, pressed);
        }

        public boolean getDragEnabled ()
        {
            return treeTable.getDragEnabled();
        }

        public Container getParent() {
            return treeTable.getParent();
        }

        public void computeVisibleRect(Rectangle visibleRect) {
            treeTable.computeVisibleRect(visibleRect);
        }

        public void repaint(long tm, int x, int y, int width, int height) {
            treeTable.repaint(tm, x+getX(), y+getY(), width, height);
        }

        public void paintImmediately(int x, int y, int w, int h) {
            treeTable.paintImmediately(x+getX(), y+getY(), w, h);
        }

        public void revalidate() {
            treeTable.revalidate();
        }

        public boolean requestFocusInWindow() {
            return treeTable.requestFocusInWindow();
        }

        public void requestFocus() {
            treeTable.requestFocus();
        }

        public boolean requestFocus(boolean temporary) {
            return treeTable.requestFocus(temporary);
        }

        public void doLayout() {
            layoutTable();
            super.doLayout();
        }

        public void scrollRectToVisible(Rectangle r) {
            scrollToVisible(r, getX(), getY());
        }

        public boolean hasFocus() {
            return treeTable.hasFocus();
        }

        public TableCellEditor getCellEditor(int row, int column) {
            treeTableCellEditor.loadEditor(row, column);
            return treeTableCellEditor;
        }

        public boolean editCellAt(int row, int col, EventObject e) {
            if (super.editCellAt(row, col, e)) {
                if (col == treeTable.getHierarchicalColumn()) {
                    Rectangle b = tree.getRowBounds(row);
                    b.x         = 0;
                    b.width     = tree.getWidth();
                    tree.repaint(b);
                }
                return true;
            }
            return false;
            // return super.editCellAt(row, col, e);
        }

        public Component prepareEditor(TableCellEditor editor, int row, int col) {
            return super.prepareEditor(editor, row, col);
        }

        protected void addImpl(Component comp, Object constraints, int index) {
            if (comp instanceof CellRendererPane) {
                super.addImpl(comp, constraints, index);
            } else {
                treeTable.add(comp, constraints, index);
            }
        }

        public void remove(Component comp) {
            if (comp instanceof CellRendererPane) {
                super.remove(comp);
            } else {
                treeTable.remove(comp);
            }
        }

        public Color getBackground() {
            return treeTable.getBackground();
        }

        public Color getForeground() {
            return treeTable.getForeground();
        }

        public Font getFont() {
            return treeTable.getFont();
        }

        public TableCellRenderer getCellRenderer(int row, int column) {
            return treeTableCellRenderer;
        }

        public Component prepareRenderer(TableCellRenderer renderer, int row, int column) {
            TreeTable t = treeTable;
            boolean hier = column == t.getHierarchicalColumn();
            Object value = hier ? "" : getValueAt(row, column);
            boolean sel = !isPaintingForPrint() && isCellSelected(row, column);
//			boolean foc = t.isColumnFocusEnabled()
//				&& t.getFocusRenderer() == null
//				&& column == t.getLeadSelectionColumn()
//				&& row == t.getLeadSelectionRow()
//				&& t.isFocusOwner();
            // Renderer will determine hasFocus value
            boolean foc = false;
            return renderer.getTableCellRendererComponent(
                    this, value, sel, foc, row, column);
        }

        public void tableChanged(TableModelEvent e) {
            if (table == null) { // constructor specialty
                if (getAutoCreateColumnsFromModel())
                    createDefaultColumnsFromModel();
            } else {
                super.tableChanged(e);
            }
        }

        public void columnAdded(TableColumnModelEvent e) {
            super.columnAdded(e);
            if (tree.getRowHeight() <= 0)
                treeTable.invalidateAllRows();
        }

        public void columnMoved(TableColumnModelEvent e) {
            super.columnMoved(e);
            if (e.getFromIndex() != e.getToIndex())
                tree.doLayout();
        }

        public void columnRemoved(TableColumnModelEvent e) {
            super.columnRemoved(e);
            if (tree.getRowHeight() <= 0)
                treeTable.invalidateAllRows();
        }

        public void createDefaultColumnsFromModel() {
            TableModel m = getModel();
            if (m != null) {
                // Remove any current columns
                TableColumnModel cm = getColumnModel();
                while (cm.getColumnCount() > 0)
                    cm.removeColumn(cm.getColumn(0));

                // Create new columns from the data model info
                for (int i = 0; i < m.getColumnCount(); i++)
                    addColumn(new TreeTableColumn(i));
            }
        }

        // bypass JTable's SortManager
        private RowSorter<? extends TableModel> rowSorter;

        public RowSorter<? extends TableModel> getRowSorter() {
            return rowSorter;
        }

        public void setRowSorter(RowSorter<? extends TableModel> rowSorter) {
            RowSorter<?> oldValue = getRowSorter();
            this.rowSorter = rowSorter;
            firePropertyChange("rowSorter", oldValue, rowSorter);
        }

        public TransferHandler getTransferHandler() {
            TransferHandler th = treeTable.getTransferHandler();
            if (th != null)
                return th;
            return super.getTransferHandler();
        }

//		private int limit(int i, int a, int b) {
//			return Math.min(b, Math.max(i, a));
//		}
//		private int getAdjustedIndex(int index, boolean row) {
//			int compare = row ? getRowCount() : getColumnCount();
//			return index < compare ? index : -1;
//		}

//		/**
//		 * Overridden to include intercell spacing in repaint
//		 */
//		@Override
//		public void columnSelectionChanged(ListSelectionEvent e) {
////			boolean isAdjusting = e.getValueIsAdjusting();
////			if (columnSelectionAdjusting && !isAdjusting) {
////				// The assumption is that when the model is no longer adjusting
////				// we will have already gotten all the changes, and therefore
////				// don't need to do an additional paint.
////				columnSelectionAdjusting = false;
////				return;
////			}
////			columnSelectionAdjusting = isAdjusting;
//			// The getCellRect() call will fail unless there is at least one row.
//			if (getRowCount() <= 0 || getColumnCount() <= 0) {
//				return;
//			}
//			int firstIndex = limit(e.getFirstIndex(), 0, getColumnCount()-1);
//			int lastIndex = limit(e.getLastIndex(), 0, getColumnCount()-1);
//			int minRow = 0;
//			int maxRow = getRowCount() - 1;
//			if (getRowSelectionAllowed()) {
//				minRow = selectionModel.getMinSelectionIndex();
//				maxRow = selectionModel.getMaxSelectionIndex();
//				int leadRow = getAdjustedIndex(selectionModel.getLeadSelectionIndex(), true);
//
//				if (minRow == -1 || maxRow == -1) {
//					if (leadRow == -1) {
//						// nothing to repaint, return
//						return;
//					}
//
//					// only thing to repaint is the lead
//					minRow = maxRow = leadRow;
//				} else {
//					// We need to consider more than just the range between
//					// the min and max selected index. The lead row, which could
//					// be outside this range, should be considered also.
//					if (leadRow != -1) {
//						minRow = Math.min(minRow, leadRow);
//						maxRow = Math.max(maxRow, leadRow);
//					}
//				}
//			}
//			Rectangle r = getCellRect(minRow, firstIndex, true);
//			r.add(getCellRect(maxRow, lastIndex, true));
//			repaint(r);
//		}

        public Rectangle getCellBounds(int row, int column, boolean includeSpacing) {
            return super.getCellRect(row, column, includeSpacing);
        }

//		@Override
//		public Rectangle getCellRect(int row, int column, boolean includeSpacing) {
//			return super.getCellRect(row, column, true);
//		}
    }

    /**
     * Type-checking TableColumn.
     * <p>
     * Requires TableCellRenderer/TableCellEditor properties
     * to implement TreeTableCellRenderer/TreeTableCellEditor.
     */
    protected static class TreeTableColumn extends TableColumn {

        public TreeTableColumn(int modelIndex) {
            super(modelIndex);
        }

        @Override
        public void setCellRenderer(TableCellRenderer renderer) {
            if (renderer != null && !(renderer instanceof TreeTableCellRenderer))
                throw new IllegalArgumentException("renderer must implement TreeTableCellRenderer");
            super.setCellRenderer(renderer);
        }

        @Override
        public void setCellEditor(TableCellEditor editor) {
            if (editor != null && !(editor instanceof TreeTableCellEditor))
                throw new IllegalArgumentException("editor must implement TreeTableCellEditor");
            super.setCellEditor(editor);
        }
    }

    private class RowSorterAdapter extends RowSorter<TableModel> {

        @Override
        public int convertRowIndexToModel(int index) {
            return index;
        }

        @Override
        public int convertRowIndexToView(int index) {
            return index;
        }

        @Override
        public TableModel getModel() {
            return getTable().getModel();
        }

        @Override
        public int getModelRowCount() {
            return getModel().getRowCount();
        }

        @Override
        public int getViewRowCount() {
            return getModelRowCount();
        }

        @Override
        public List<? extends SortKey> getSortKeys() {
            return treeTable.getRowSorter().getSortKeys();
        }

        @Override
        public void setSortKeys(List<? extends SortKey> keys) {
            treeTable.getRowSorter().setSortKeys(keys);
        }

        @Override
        public void toggleSortOrder(int column) {
            treeTable.getRowSorter().toggleSortOrder(column);
        }

        @Override
        public void allRowsChanged() {}

        @Override
        public void modelStructureChanged() {}

        @Override
        public void rowsDeleted(int firstRow, int endRow) {}

        @Override
        public void rowsInserted(int firstRow, int endRow) {}

        @Override
        public void rowsUpdated(int firstRow, int endRow) {}

        @Override
        public void rowsUpdated(int firstRow, int endRow, int column) {}

        @Override
        public void addRowSorterListener(RowSorterListener l) {}

        @Override
        public void removeRowSorterListener(RowSorterListener l) {}
    }

    private static class ColumnModelAdapter implements TableColumnModel {

        ColumnModelAdapter(TableColumnModel cm) {
            delegate = cm;
        }

        private TableColumnModel delegate;

        @Override
        public int getColumnMargin() {
            return 0;
        }

        @Override
        public void addColumn(TableColumn aColumn) {
            delegate.addColumn(aColumn);
        }

        @Override
        public void addColumnModelListener(TableColumnModelListener x) {
            delegate.addColumnModelListener(x);
        }

        @Override
        public TableColumn getColumn(int columnIndex) {
            return delegate.getColumn(columnIndex);
        }

        @Override
        public int getColumnCount() {
            return delegate.getColumnCount();
        }

        @Override
        public int getColumnIndex(Object columnIdentifier) {
            return delegate.getColumnIndex(columnIdentifier);
        }

        @Override
        public int getColumnIndexAtX(int xPosition) {
            return delegate.getColumnIndexAtX(xPosition);
        }

        @Override
        public Enumeration<TableColumn> getColumns() {
            return delegate.getColumns();
        }

        @Override
        public boolean getColumnSelectionAllowed() {
            return delegate.getColumnSelectionAllowed();
        }

        @Override
        public int getSelectedColumnCount() {
            return delegate.getSelectedColumnCount();
        }

        @Override
        public int[] getSelectedColumns() {
            return delegate.getSelectedColumns();
        }

        @Override
        public ListSelectionModel getSelectionModel() {
            return delegate.getSelectionModel();
        }

        @Override
        public int getTotalColumnWidth() {
            return delegate.getTotalColumnWidth();
        }

        @Override
        public void moveColumn(int columnIndex, int newIndex) {
            delegate.moveColumn(columnIndex, newIndex);
        }

        @Override
        public void removeColumn(TableColumn column) {
            delegate.removeColumn(column);
        }

        @Override
        public void removeColumnModelListener(TableColumnModelListener x) {
            delegate.removeColumnModelListener(x);
        }

        @Override
        public void setColumnMargin(int newMargin) {
            delegate.setColumnMargin(newMargin);
        }

        @Override
        public void setColumnSelectionAllowed(boolean flag) {
            delegate.setColumnSelectionAllowed(flag);
        }

        @Override
        public void setSelectionModel(ListSelectionModel newModel) {
            delegate.setSelectionModel(newModel);
        }
    }

    private static Color createColor(Color c) {
        if (c instanceof UIResource)
            c = new Color(c.getRed(), c.getGreen(), c.getBlue(), c.getAlpha());
        return c;
    }

    // Hacks for Nimbus...
    private class NimbusRenderer extends Renderer {

        private static final String collapsedSelected = "Tree[Enabled+Selected].collapsedIconPainter";

        private static final String expandedSelected = "Tree[Enabled+Selected].expandedIconPainter";

        private static final String collapsed = "Tree[Enabled].collapsedIconPainter";

        private static final String expanded = "Tree[Enabled].expandedIconPainter";

//		private static final String collapsedFocused = "Tree[Enabled+Focused].collapsedIconPainter";
//
//		private static final String expandedFocused = "Tree[Enabled+Focused].expandedIconPainter";
//
//		private static final String collapsedFocusedSelected = "Tree[Enabled+Focused+Selected].collapsedIconPainter";
//
//		private static final String expandedFocusedSelected = "Tree[Enabled+Focused+Selected].expandedIconPainter";

        NimbusRenderer() {
            current = (UIDefaults)tree.getClientProperty("Nimbus.Overrides");
            rowSelection = current;
            rowSelection.put(collapsed, UIManager.get(collapsed));
            rowSelection.put(expanded, UIManager.get(expanded));
            rowSelection.put(collapsedSelected, UIManager.get(collapsedSelected));
            rowSelection.put(expandedSelected, UIManager.get(expandedSelected));
        }

        private UIDefaults current;

        private UIDefaults rowSelection;

        private UIDefaults columnSelected;

        private UIDefaults noneSelected;

        private UIDefaults getDefaults() {
            if (rowSelectionAllowed) {
                if (!columnSelectionAllowed || treeColumnSelected) {
                    return rowSelection;
                }
            } else if (columnSelectionAllowed && treeColumnSelected) {
                if (columnSelected == null) {
                    columnSelected = new UIDefaults();
                    columnSelected.putAll(rowSelection);
                    columnSelected.put(collapsed, UIManager.get(collapsedSelected));
                    columnSelected.put(expanded, UIManager.get(expandedSelected));
                }
                return columnSelected;
            }
            if (noneSelected == null) {
                noneSelected = new UIDefaults();
                noneSelected.putAll(rowSelection);
                noneSelected.put(collapsedSelected, UIManager.get(collapsed));
                noneSelected.put(expandedSelected, UIManager.get(expanded));
            }
            return noneSelected;
        }

        @Override
        public void prepareForTable(Graphics g) {
            super.prepareForTable(g);
            focus = false;
        }

        @Override
        protected void prepareBackgroundColors() {
            super.prepareBackgroundColors();
            backgroundColor = createColor(backgroundColor);
            alternateRowColor = createColor(alternateRowColor);
        }

        @Override
        public void prepareForTree() {
            super.prepareForTree();
            if (System.getSecurityManager() != null)
                return;
            UIDefaults map = getDefaults();
            if (map == current)
                return;
            current = map;
            clearCachedOverrides(tree, Region.TREE);
            tree.putClientProperty("Nimbus.Overrides", map);
            focus = false;
        }

        @Override
        protected Color getDropCellForeground() {
            return createColor(super.getDropCellForeground());
        }

        @Override
        protected Color getDropCellBackground() {
            return createColor(super.getDropCellBackground());
        }

        private boolean focus;

        private int row;

        private int column;

        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable,
                                                           Object value, boolean sel, boolean foc, int row, int col) {
            Component c = super.getTreeTableCellRendererComponent(
                    treeTable, value, sel, foc, row, col);
            focus = foc;
            this.row = row;
            this.column = col;
            return c;
        }

        @Override
        protected void paintTableComponent(Graphics g, Component c) {
            super.paintTableComponent(g, c);
            // Nimbus fails to paint its focus for Boolean columns
            if (focus && c instanceof JCheckBox) {
                Component fc = getFocusRenderer().getTreeTableCellRendererComponent(
                        treeTable, "", false, true, row, column);
                if (treeTable.isNodeSortingEnabled()) {
                    paintComponent(g, fc, treeTable.getAlignment(c, row, column));
                } else {
                    paintComponent(g, fc, table, 0, 0, getWidth(), getHeight(), true);
                }
            }
        }

        private TreeTableCellRenderer focusRenderer;

        private TreeTableCellRenderer getFocusRenderer() {
            TreeTableCellRenderer r = treeTable.getFocusRenderer();
            if (r == null) {
                if (focusRenderer == null)
                    focusRenderer = new FocusRenderer();
                r = focusRenderer;
            }
            return r;
        }

        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6752660
        private void clearCachedOverrides(JComponent c, Object region) {
            try {
                LookAndFeel nimbusLaf = UIManager.getLookAndFeel();
                Class nimbusLafClass = nimbusLaf.getClass();

                Field defaultsField = nimbusLafClass.getDeclaredField("defaults");
                defaultsField.setAccessible(true);

                Object defaults = defaultsField.get(nimbusLaf);
                Class defaultsClass = defaults.getClass();

                // NimbusDefaults has this private field:
                // private Map<Region, List<LazyStyle>> m;
                Field mField = defaultsClass.getDeclaredField("m");
                mField.setAccessible(true);
                Map m = (Map) mField.get(defaults);
                List values = (List) m.get(region);
                if (values != null) {
                    for (Object lazyStyleObj : values) {
                        // find this field:
                        // private WeakHashMap<JComponent, WeakReference<NimbusStyle>> overridesCache;
                        Class lazyStyleClass = lazyStyleObj.getClass();
                        Field overridesCacheField =
                                lazyStyleClass.getDeclaredField("overridesCache");
                        overridesCacheField.setAccessible(true);
                        Map overridesCache = (Map) overridesCacheField.get(lazyStyleObj);
                        if (overridesCache != null) {
                            overridesCache.remove(c);
                        }
                    }
                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private int sortWidth = -1;

    protected int getSortWidth() {
        if (sortWidth < 0) {
            sortWidth = getSortMargin()*2 + Math.max(
                    treeTable.getAscendingSortIcon().getIconWidth(),
                    treeTable.getDescendingSortIcon().getIconWidth());
        }
        return sortWidth;
    }

    protected int getSortMargin() {
        return 2;
    }

    private Border ltrBorder;

    private Border rtlBorder;

    private Border noFocusBorder;

    private Border getDefaultLTRBorder() {
        if (ltrBorder == null)
            ltrBorder = new EmptyBorder(1, 0, 1, 1);
        return ltrBorder;
    }

    private Border getDefaultRTLBorder() {
        if (rtlBorder == null)
            rtlBorder = new EmptyBorder(1, 1, 1, 0);
        return rtlBorder;
    }

    private Border getDefaultNoFocusBorder() {
        if (noFocusBorder == null) {
            noFocusBorder = UIManager.getBorder("Table.cellNoFocusBorder");
            if (noFocusBorder == null)
                noFocusBorder = new EmptyBorder(1, 1, 1, 1);
        }
        return noFocusBorder;
    }

    protected class Renderer extends CellRendererPane implements
            TreeCellRenderer, TableCellRenderer, TreeTableCellRenderer {

        public Renderer() {}

        protected Component component;

        protected boolean tableColumn = true;

        protected boolean treeColumnSelected;

        protected boolean rowSelectionAllowed;

        protected boolean columnSelectionAllowed;

        protected Object node;

        protected int row;

        protected int column;

        protected Color backgroundColor;

        protected Color alternateRowColor;

        private TableCellRenderer renderer;

        private int left, right, top, bottom;

        private int focusRow, focusColumn;

        public void loadRenderer(Class<?> cls) {
            renderer = table.getDefaultRenderer(cls);
        }

        public void prepareForTable(Graphics g) {
            prepareBackgroundColors();
            int cm = treeTable.getColumnModel().getColumnMargin();
            int rm = treeTable.getRowMargin();
            if (cm != 0 || rm != 0) {
                left = cm/2;
                right = left + cm%2;
                top = rm/2;
                bottom = top + rm%2;
            } else {
                left = right = top = bottom = 0;
            }
            if (treeTable.getFocusRenderer() != null) {
                focusRow = focusColumn = -1;
            } else {
                focusRow = tree.getLeadSelectionRow();
                if (treeTable.isColumnFocusEnabled()) {
                    focusColumn = Integer.MIN_VALUE;
                } else {
                    focusColumn = treeTable.getLeadSelectionColumn();
                }
            }
        }

        protected void prepareBackgroundColors() {
            backgroundColor = treeTable.getBackground();
            alternateRowColor = treeTable.getAlternateRowColor();
            if (alternateRowColor == null)
                alternateRowColor = backgroundColor;
        }

        public void prepareForTree() {
            tableColumn = false;
            column = treeTable.getHierarchicalColumn();
            rowSelectionAllowed = table.getRowSelectionAllowed();
            columnSelectionAllowed = table.getColumnSelectionAllowed();
            treeColumnSelected = table.isColumnSelected(column);
        }

        public void clearState() {
            renderer = null;
            component = null;
            node = null;
            backgroundColor = null;
            alternateRowColor = null;
            removeAll();

            // must be true when the state is invalid
            tableColumn = true;
        }

        // initial entry point while painting table
        @Override
        public Component getTableCellRendererComponent(JTable table,
                                                       Object value, boolean sel, boolean foc, int row, int col) {
            tableColumn = true;
            this.row = row;
            column = col;
            foc = isFocused(row, col);
            component = getTableComponent(value, sel, foc, row, col);
            return this;
        }

        private Component getTableComponent(Object value,
                                            boolean sel, boolean foc, int row, int col) {
            return treeTable.getCellRenderer(row, col).getTreeTableCellRendererComponent(
                    treeTable, value, sel, foc, row, col);
        }

        // initial entry point while painting tree
        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                      boolean sel, boolean exp, boolean leaf, int row, boolean foc) {
            if (tableColumn) {
                tableColumn = false;
                column = treeTable.getHierarchicalColumn();
            }

            node = value;
            this.row = row;

            // implement JTable's selection idioms.
            sel = isSelected(sel);

            foc = isFocused(row, column);

            TreeColumnModel model = treeTable.getTreeColumnModel();
            value = model.getValueAt(value, model.getHierarchicalColumn());
            component = getTreeComponent(value, sel, foc, row, column, exp, leaf);
            return this;
        }

        private Component getTreeComponent(Object value, boolean sel,
                                           boolean foc, int row, int col, boolean exp, boolean leaf) {
            return treeTable.getCellRenderer(row, col).getTreeTableCellRendererComponent(
                    treeTable, value, sel, foc, row, col, exp, leaf);
        }

        boolean isSelected(boolean sel) {
            if (!columnSelectionAllowed)
                return rowSelectionAllowed ? sel : false;
            return rowSelectionAllowed ?
                    sel && treeColumnSelected : treeColumnSelected;
        }

        boolean isFocused(int row, int col) {
            return row == focusRow && focusColumn < -1 ? true : col == focusColumn;
        }

        // entry point for default table column renderers
        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable,
                                                           Object value, boolean sel, boolean foc, int row, int col) {
            Component c = renderer.getTableCellRendererComponent(
                    table, value, sel, foc, row, col);
            if (c instanceof JComponent)
                ((JComponent)c).setOpaque(false);
            configureCellRenderer(c, sel, row, col);
            return c;
        }

        // entry point for default tree column renderer
        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable,
                                                           Object value, boolean sel, boolean foc, int row, int col, boolean exp, boolean leaf) {
            return defaultTreeCellRenderer.getTreeTableCellRendererComponent(
                    treeTable, value, sel, foc, row, col, exp, leaf);
        }

        private Dimension getComponentPreferredSize() {
            Dimension size = component.getPreferredSize();
            if (treeTable.isNodeSortingEnabled()) {
                switch (treeTable.getAlignment(component, row, column)) {
                    case SwingConstants.CENTER:
                        size.width += getSortWidth()*2;
                        break;
                    case SwingConstants.LEFT:
                    case SwingConstants.RIGHT:
                        size.width += getSortWidth();
                        break;
                }
            }
            return size;
        }

        @Override
        public Dimension getPreferredSize() {
            // for table column or tree column with
            // fixed row height, return the cell's size
            if (tableColumn || tree.getRowHeight() > 0)
                return getComponentPreferredSize();

            // BasicTreeUI requests node dimensions for the root
            // even if it isn't visible.
            // Since the values for the root may not be valid for
            // renderers, when the root isn't visible check for when
            // the node is the root and return 0x0
            if (!tree.isRootVisible() && row <= 0
                    && node == treeTable.getTreeModel().getRoot()) {
                return new Dimension(0, 0);
            }

            // Calculate the size for the tree column.
            // Includes the preferred height for all columns
            // in the row.
            int margin = treeTable.getRowMargin();
            Dimension size;
            int tc = column;
            if (tc >= 0) {
                size = getComponentPreferredSize();
                size.height += margin;
            } else {
                size = new Dimension(1, margin);
            }
            TableColumnModel cm = treeTable.getColumnModel();
            TreeColumnModel rm = treeTable.getTreeColumnModel();
            Object nod = node;
            // TODO (TBD) use row -1 because this can be called
            // before the row is valid in the table?
            //int row = -1;
            for (int col=cm.getColumnCount(); --col>=0;) {
                if (col == tc)
                    continue;
                Object value = rm.getValueAt(nod, cm.getColumn(col).getModelIndex());
                Component c = getTableComponent(value, false, false, row, col);
                size.height = Math.max(size.height, c.getPreferredSize().height + margin);
            }
            return size;
        }

        @Override
        public void paint(Graphics g) {
            if (tableColumn) {
                paintTableComponent(g, component);
            } else {
                paintTreeComponent(g, component);
            }
        }

        /**
         * Shifts bounds to account for TreeTable's cell margins.
         */
        void paintComponent(Graphics g, Component c, int x, int y, int w, int h) {
            x += left;
            y += top;
            w -= left + right;
            h -= top  + bottom;
            paintComponent(g, c, null, x, y, w, h, true);
        }

        void paintComponent(Graphics g, Component c, int align) {
            int w = getSortWidth();
            switch (align) {
                case SwingConstants.CENTER:
                    paintComponent(g, c, w, 0, getWidth()-w-w, getHeight());
                    break;
                case SwingConstants.LEFT:
                    paintComponent(g, c, 0, 0, getWidth()-w, getHeight());
                    break;
                case SwingConstants.RIGHT:
                    paintComponent(g, c, w, 0, getWidth()-w, getHeight());
                    break;
            }
        }

        protected void paintTableComponent(Graphics g, Component c) {
            // paint background including margins
            if (shouldPaintBackground(c)) {
                g.setColor(c.getBackground());
                g.fillRect(0, 0, getWidth(), getHeight());
            }

            if (treeTable.isNodeSortingEnabled()) {
                paintComponent(g, c, treeTable.getAlignment(c, row, column));
            } else {
                paintComponent(g, c, 0, 0, getWidth(), getHeight());
            }
        }

        protected boolean shouldPaintBackground(Component c) {
            if (c instanceof JComponent) {
                JComponent jc = (JComponent) c;
                return jc.isOpaque();
            }
            return c.isBackgroundSet();
        }

        protected void paintTreeComponent(Graphics g, Component c) {
            int x = 0;
            int y = 0;
            int w = getWidth();
            int h = getHeight();
            if (getX() + w > tree.getWidth())
                w = tree.getWidth() - getX();
            // only shift y-axis, tree's bounds are already shifted for x-axis
            y += top;
            h -= top + bottom;
            paintComponent(g, c, null, x, y, w, h, true);
        }

        /**
         * Report the TreeTable's opacity since renderers will
         * assume this component is this TreeTable.
         */
        @Override
        public boolean isOpaque() {
            Container p = getParent();
            return p != null ? p.isOpaque() : super.isOpaque();
        }

        @Override
        public void repaint() {}
        @Override
        public void repaint(long tm, int x, int y, int w, int h) {}

        @Override
        protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {}
        @Override
        public void firePropertyChange(String propertyName, byte oldValue, byte newValue) {}
        @Override
        public void firePropertyChange(String propertyName, char oldValue, char newValue) {}
        @Override
        public void firePropertyChange(String propertyName, short oldValue, short newValue) {}
        @Override
        public void firePropertyChange(String propertyName, int oldValue, int newValue) {}
        @Override
        public void firePropertyChange(String propertyName, long oldValue, long newValue) {}
        @Override
        public void firePropertyChange(String propertyName, float oldValue, float newValue) {}
        @Override
        public void firePropertyChange(String propertyName, double oldValue, double newValue) {}
        @Override
        public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {}

        /**
         * Sets the components foreground and background based on
         * selection/drop cell/alternate row color
         */
        private void configureCellRenderer(Component c, boolean sel, int row, int col) {

            Color fg = null;
            Color bg = null;

            DropLocation dropLocation = treeTable.getDropLocation();
            if (dropLocation != null
                    && !dropLocation.isInsertRow()
                    && !dropLocation.isInsertColumn()
                    && dropLocation.getRow() == row
                    && dropLocation.getColumn() == col) {

                fg = getDropCellForeground();
                bg = getDropCellBackground();

                sel = true;
            }

            if (sel) {
                c.setForeground(fg != null ? fg :
                        treeTable.getSelectionForeground());
                c.setBackground(bg != null ? bg :
                        treeTable.getSelectionBackground());
            } else {
                c.setForeground(treeTable.getForeground());
                if (backgroundColor == null)
                    prepareBackgroundColors();
                c.setBackground(row % 2 == 0 ? backgroundColor : alternateRowColor);
            }
        }

        // entry points for Configuration of DefaultTreeTableCellRenderer

        /**
         * @return the update selected status
         */
        public boolean configureCellRenderer(DefaultTreeTableCellRenderer renderer,
                                             TreeTable treeTable, Object value, boolean selected,
                                             boolean hasFocus, int row, int column) {

            renderer.setOpaque(false);

            renderer.setIcon(null);
            renderer.setDisabledIcon(null);

            configureCellRenderer(renderer, selected, row, column);

            renderer.setFont(treeTable.getFont());

            if (hasFocus) {
                Border border = null;
                if (selected) {
                    border = UIManager.getBorder("Table.focusSelectedCellHighlightBorder");
                }
                if (border == null) {
                    border = UIManager.getBorder("Table.focusCellHighlightBorder");
                }
                renderer.setBorder(border);

                if (!selected && table.isCellEditable(row, column)) {
                    Color col;
                    col = UIManager.getColor("Table.focusCellForeground");
                    if (col != null) {
                        super.setForeground(col);
                    }
                    col = UIManager.getColor("Table.focusCellBackground");
                    if (col != null) {
                        super.setBackground(col);
                    }
                }
            } else {
                renderer.setBorder(getNoFocusBorder());
            }
            return selected;
        }

        protected Border getNoFocusBorder() {
            return getDefaultNoFocusBorder();
        }

        protected Color getDropCellForeground() {
            return UIManager.getColor("Table.dropCellForeground");
        }

        protected Color getDropCellBackground() {
            return UIManager.getColor("Table.dropCellBackground");
        }

        public void configureCellRenderer(DefaultTreeTableCellRenderer renderer,
                                          TreeTable treeTable, Object value, boolean sel,
                                          boolean foc, int row, int col, boolean exp, boolean leaf) {

            renderer.setOpaque(false);

            Color fg = sel ?
                    treeTable.getSelectionForeground():
                    treeTable.getForeground();
            renderer.setForeground(fg);

            Icon icon = treeTable.getIcon(value, exp, leaf);
            if (!treeTable.isEnabled()) {
                renderer.setEnabled(false);
                renderer.setDisabledIcon(icon);
            }
            else {
                renderer.setEnabled(true);
                renderer.setIcon(icon);
            }
            ComponentOrientation o = treeTable.getComponentOrientation();
            renderer.setComponentOrientation(o);
            renderer.setBorder(o.isLeftToRight() ?
                    getLTRBorder() : getRTLBorder());
            renderer.setFont(treeTable.getFont());
        }

        protected Border getLTRBorder() {
            return getDefaultLTRBorder();
        }

        protected Border getRTLBorder() {
            return getDefaultRTLBorder();
        }
    }

    private class TreeEditorContainer extends Container {

        TreeEditorContainer() {}

        private Component component;

        private int row;

        public void setBounds(int x, int y, int w, int h) {
            Rectangle node = tree.getRowBounds(row);
            if (tree.getComponentOrientation().isLeftToRight()) {
                x = node.x;
                w -= node.x;
            } else {
                w = node.x + node.width;
            }
            component.setBounds(0, 0, w, h);
            super.setBounds(x, y, w, h);
        }

        void setState(Component c, int row) {
            add(c);
            component = c;
            this.row = row;
        }

        void clearState() {
            removeAll();
            component = null;
        }
    }

    private class TreeEditor extends CellEditorContainer<TableCellEditor> {

        TreeEditor(TableCellEditor editor) {
            super(editor);
        }

        public Component getCellEditorComponent(TreeTable treeTable, Object value,
                                                boolean sel, int row, int col, boolean exp, boolean leaf) {
            return editor.getTableCellEditorComponent(
                    table, value, sel, row, col);
        }
    }

    protected class Editor extends AbstractCellEditor implements
            TableCellEditor, CellEditorListener, TreeTableCellEditor {

        private TreeEditor defaultTreeEditor;

        private TreeEditorContainer treeEditorContainer;

        private transient TreeTableCellEditor treeTableEditor;

        private transient CellEditor defaultEditor;

        // initial entry point from JTable.getCellEditor(row, col)
        public void loadEditor(int row, int col) {
            clearState();
            treeTableEditor = treeTable.getCellEditor(row, col);
        }

        public void loadEditor(Class<?> cls, int col) {
            treeTableEditor = this;
            if (col == treeTable.getHierarchicalColumn()) {
                if (defaultTreeEditor == null)
                    defaultTreeEditor = new TreeEditor(table.getDefaultEditor(Object.class));
                defaultEditor = defaultTreeEditor;
            } else {
                defaultEditor = table.getDefaultEditor(table.getColumnClass(col));
            }
        }

        // second entry point
        @Override
        public Component getTableCellEditorComponent(JTable table,
                                                     Object value, boolean selected, int row, int column) {
            Component c;
            boolean treeColumn = column == treeTable.getHierarchicalColumn();
            if (treeColumn) {
                TreePath path = tree.getPathForRow(row);
                boolean expanded = tree.isExpanded(path);
                boolean leaf = treeTable.isLeaf(path);
                c = treeTableEditor.getTreeTableCellEditorComponent(
                        treeTable, value, selected, row, column, expanded, leaf);
            } else {
                c = treeTableEditor.getTreeTableCellEditorComponent(
                        treeTable, value, selected, row, column);
            }
            if (treeColumn) {
                if (treeEditorContainer == null)
                    treeEditorContainer = new TreeEditorContainer();
                treeEditorContainer.setState(c, row);
                c = treeEditorContainer;
            }
            getEditor().addCellEditorListener(this);
            return c;
        }

        // entry point for default table column editors
        @Override
        public Component getTreeTableCellEditorComponent(TreeTable treeTable,
                                                         Object value, boolean sel, int row, int col) {
            return ((TableCellEditor)defaultEditor).getTableCellEditorComponent(
                    table, value, sel, row, col);
        }

        // entry point for default tree column editor
        @Override
        public Component getTreeTableCellEditorComponent(TreeTable treeTable,
                                                         Object value, boolean sel, int row, int col, boolean exp, boolean leaf) {
            return defaultTreeEditor.getTreeTableCellEditorComponent(
                    treeTable, value, sel, row, col, exp, leaf);
        }

        private CellEditor getEditor() {
            return defaultEditor != null ? defaultEditor : treeTableEditor;
        }

        @Override
        public void cancelCellEditing() {
            getEditor().cancelCellEditing();
        }

        @Override
        public Object getCellEditorValue() {
            return getEditor().getCellEditorValue();
        }

        @Override
        public boolean isCellEditable(EventObject anEvent) {
            return getEditor().isCellEditable(anEvent);
        }

        @Override
        public boolean shouldSelectCell(EventObject anEvent) {
            return getEditor().shouldSelectCell(anEvent);
        }

        @Override
        public boolean stopCellEditing() {
            return getEditor().stopCellEditing();
        }

        @Override
        public void editingCanceled(ChangeEvent e) {
            fireEditingCanceled();
            clearState();
        }

        @Override
        public void editingStopped(ChangeEvent e) {
            fireEditingStopped();
            clearState();
        }

        public void clearState() {
            if (treeTableEditor != null) {
                getEditor().removeCellEditorListener(this);
                treeTableEditor = null;
                defaultEditor   = null;
                if (treeEditorContainer != null)
                    treeEditorContainer.clearState();
                if (defaultTreeEditor != null)
                    defaultTreeEditor.clearState();
            }
        }
    }

    private class FocusRenderer extends DefaultTableCellRenderer.UIResource
            implements TreeTableCellRenderer {

        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable,
                                                           Object value, boolean selected, boolean hasFocus, int row,
                                                           int column) {
            return super.getTableCellRendererComponent(
                    table, value, selected, hasFocus, row, column);
        }

        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable,
                                                           Object value, boolean selected, boolean hasFocus, int row,
                                                           int column, boolean expanded, boolean leaf) {
            return super.getTableCellRendererComponent(
                    table, value, selected, hasFocus, row, column);
        }

        @Override
        public boolean isOpaque() { return false; }
    }

    private int treeHandleWidth = -1;

    /**
     * If a negative number is returned, then all events that occur in the
     * leading margin will be forwarded to the tree and consumed.
     *
     * @return the width of the tree handle if it can be determined, else -1
     */
    protected int getTreeHandleWidth() {
        return treeHandleWidth;
    }

    protected boolean processKeyBinding(JComponent c, KeyStroke ks,
                                        KeyEvent e, int condition, boolean pressed) {
        if (c instanceof ProcessKeyBinding) {
            return ((ProcessKeyBinding)c).processKeyBinding(
                    ks, e, condition, pressed);
        } else {
            return false;
        }
    }

    protected class Handler extends MouseAdapter
            implements FocusListener, KeyListener, PropertyChangeListener {

        public Handler() {
        }

        private boolean sendDragToTable = true;

        private boolean sortPressed = false;

        private MouseEvent dndArmedEvent;

        private int motionThreshold;

        @Override
        public void keyPressed(KeyEvent e) {
            processKeyEvent(e);
        }

        @Override
        public void keyReleased(KeyEvent e) {
            processKeyEvent(e);
        }

        @Override
        public void keyTyped(KeyEvent e) {
            processKeyEvent(e);
            if (!e.isConsumed())
                changeSelection(e);
        }

        @Override
        public void mousePressed(MouseEvent e) {
            processMouseEvent(e);
            if (e.isConsumed()) {
                sendDragToTable = false;
            } else if (treeTable.getDragEnabled()) {
                motionThreshold = Math.max(10, DragSource.getDragThreshold());
                dndArmedEvent = e;
            }
        }

        @Override
        public void mouseExited(MouseEvent e) {
            setMouseOverSortIndicator(null, -1);
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            processMouseEvent(e);
            dndArmedEvent = null;
            sendDragToTable = true;
        }

        @Override
        public void mouseClicked(MouseEvent e) {
            processMouseEvent(e);
        }

        @Override
        public void mouseMoved(MouseEvent e) {
            if (!treeTable.isNodeSortingEnabled() || treeTable.getRowSorter() == null) {
                setMouseOverSortIndicator(null, -1);
            } else if (!treeTable.isEditing()) {
                Point pt = e.getPoint();
                int col = table.columnAtPoint(pt);
                if (col < 0) {
                    setMouseOverSortIndicator(null, -1);
                } else {
                    TreePath path = getSortPath(pt, col);
                    setMouseOverSortIndicator(path, path == null ? -1 : col);
                }
            }
        }

        private TreePath getSortPath(Point pt, int col) {
            int row = table.rowAtPoint(pt);
            if (row < 0)
                return null;
            TreePath path = tree.getPathForRow(row);
            if (treeTable.isLeaf(path) || treeTable.getChildCount(path) == 0)
                return null;
            Rectangle r = table.getCellRect(row, col, true);
            switch (treeTable.getAlignment(getRendererComponent(false, false, row, col), row, col)) {
                default: return null;
                case SwingConstants.CENTER:
                case SwingConstants.LEFT:
                    if (pt.x < r.x + r.width - getSortWidth())
                        return null;
                    break;
                case SwingConstants.RIGHT:
                    if (pt.x > r.x + getSortWidth())
                        return null;
                    break;
            }
            return path;
        }

        @Override
        public void mouseDragged(MouseEvent e) {
            if (e.isConsumed())
                return;
            if (treeTable.getDragEnabled()) {
                if (dndArmedEvent != null) {
                    int dx = Math.abs(e.getX() - dndArmedEvent.getX());
                    int dy = Math.abs(e.getY() - dndArmedEvent.getY());
                    if ((dx > motionThreshold) || (dy > motionThreshold)) {
                        TransferHandler th = treeTable.getTransferHandler();
                        int actions = th == null ? 0 : th.getSourceActions(treeTable);
                        if (actions != 0) {
//							isDragging = true;
                            // TODO, determine action
                            int action = TransferHandler.MOVE;
                            th.exportAsDrag(treeTable, dndArmedEvent, action);
                        }
                    }
                }
            } else if (sendDragToTable) {
                dispatchMouseEvent(e, table);
            }
        }

        /**
         * Implementation of Key Binding order:
         * 		treeTable WHEN_FOCUS
         * 		treeTable WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
         * 		table or tree WHEN_FOCUSED
         * 		table or tree WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
         * 		treeTable WHEN_IN_FOCUS_WINDOW
         *
         * KeyListeners are notified before the Key Binding mechanism.
         *
         * TODO Note: JTable's default (crude) implementation of
         * editing upon any key press is circumvented with this
         * implementation (what a shame). A similar (better) operation
         * needs to be conducted.
         */
        protected void processKeyEvent(KeyEvent e) {
            if (e.isConsumed())
                return;
            KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);
            int condition = treeTable.getConditionForKeyStroke(ks);
            if (condition == JComponent.WHEN_FOCUSED ||
                    condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
                return;
            if ((!dispatchToTable(e) || !dispatchKeyEvent(e, ks, table)))
                dispatchKeyEvent(e, ks, tree);
        }

        private boolean dispatchToTable(KeyEvent e) {
            switch (e.getKeyCode()) {
                case KeyEvent.VK_DOWN: case KeyEvent.VK_UP:
                case KeyEvent.VK_PAGE_DOWN: case KeyEvent.VK_PAGE_UP:
                    return false;
                case KeyEvent.VK_END: case KeyEvent.VK_HOME:
                case KeyEvent.VK_LEFT: case KeyEvent.VK_RIGHT:
                    return (treeTable.isColumnFocusEnabled() || treeTable.getColumnSelectionAllowed()) &&
                            table.getColumnModel().getColumnCount() > 1;
            }
            return true;
        }

        private boolean dispatchKeyEvent(KeyEvent e, KeyStroke ks, JComponent c) {
            int condition = c.getConditionForKeyStroke(ks);
            if (condition == JComponent.WHEN_FOCUSED ||
                    condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) {
                if (processKeyBinding(c, ks, e, condition, !ks.isOnKeyRelease())) {
                    e.consume();
                    return true;
                }
            }
            return false;
        }


        /**
         * Dispatches the MouseEvent to the table or tree.
         */
        protected void processMouseEvent(MouseEvent e) {
            if (e.isConsumed() || e.isPopupTrigger())
                return;
            if (sortNode(e))
                return;
            if (!dispatchToTree(e))
                dispatchMouseEvent(e, table);
        }

        private boolean sortNode(MouseEvent e) {
            switch (e.getID()) {
                default:
                    return false;
                case MouseEvent.MOUSE_RELEASED:
                    try {
                        return sortPressed;
                    } finally {
                        sortPressed = false;
                    }
                case MouseEvent.MOUSE_PRESSED:
            }
            if (mouseOverSortPath == null)
                return false;
            TreeTableSorter<?,?> sorter = treeTable.getRowSorter();
            if (sorter == null)
                return false;
            RowSorter<?> nodeSorter = sorter.getRowSorter(mouseOverSortPath);
            if (nodeSorter == null)
                return false;
            nodeSorter.toggleSortOrder(table.convertColumnIndexToModel(mouseOverSortColumn));
            paintMouseOverSortPath = false;
            sortPressed = true;
            e.consume();
            return true;
        }

        // returns `true` if event was dispatched to tree because it relates to the tree handle
        private boolean dispatchToTree(MouseEvent e) {
            switch (e.getID()) {
                case MouseEvent.MOUSE_ENTERED:
                case MouseEvent.MOUSE_EXITED:
                    return false;
            }
            int hier = treeTable.getHierarchicalColumn();
            if (hier < 0)
                return false;
            Point pt = e.getPoint();
            int col = table.columnAtPoint(pt);
            if (col != hier)
                return false;
            int row = table.rowAtPoint(pt);
            if (row < 0)
                return false;
            TreePath path = tree.getPathForRow(row);
            if (!hasTreeHandle(treeTable, path))
                return false;
            if (isOverTreeHandle(tree.getComponentOrientation().isLeftToRight(),
                    tree.getPathBounds(path), getTreeHandleWidth(), e.getX()-tree.getX())) {
                dispatchMouseEvent(e, tree);
                e.consume();
                return true;
            }
            return false;
        }

        protected void dispatchMouseEvent(MouseEvent e, JComponent c) {
            MouseEvent me = new MouseEvent(c, e.getID(), e.getWhen(), e.getModifiers(),
                    e.getX()-c.getX(), e.getY()-c.getY(), e.getXOnScreen(), e.getYOnScreen(),
                    e.getClickCount(), false, e.getButton());
            c.dispatchEvent(me);
            if (me.isConsumed())
                e.consume();
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String name = evt.getPropertyName();
            if (name.equals("rowSorter")) {
                table.setRowSorter(evt.getNewValue() == null ? null : new RowSorterAdapter());
            } else if (name.equals("enabled")) {
                boolean enabled = (Boolean)evt.getNewValue();
                table.setEnabled(enabled);
                tree.setEnabled(enabled);
            } else if (name.equals("componentOrientation")) {
                ComponentOrientation o = (ComponentOrientation)evt.getNewValue();
                table.setComponentOrientation(o);
                tree.setComponentOrientation(o);
                treeTable.revalidate();
                treeTable.repaint();
            } else if ("JTree.lineStyle".equals(name)) {
                tree.putClientProperty("JTree.lineStyle", evt.getNewValue());
            } else if ("columnModel".equals(name)) {
                table.setColumnModel(new ColumnModelAdapter((TableColumnModel)evt.getNewValue()));
            }
        }

        @Override
        public void focusGained(FocusEvent e) {
            focusChanged();
        }

        @Override
        public void focusLost(FocusEvent e) {
            focusChanged();
        }

        protected void focusChanged() {
            repaintLead();
        }

        private void repaintLead() {
            TreePath lead = tree.getLeadSelectionPath();
            if (lead == null)
                return;
            int row = tree.getRowForPath(lead);
            Rectangle rect;
            if (treeTable.isColumnFocusEnabled()) {
                int col = table.getColumnModel().getSelectionModel().getLeadSelectionIndex();
                if (col < 0)
                    return;
                rect = table.getCellRect(row, col, true);
            } else {
                rect = table.getCellRect(row, 0, true);
                rect.width = treeTable.getWidth();
            }
            treeTable.repaint(rect);
        }

        private String prefix = "";

        private String typedString = "";

        private long lastTime = 0L;

        private long timeFactor = 1000L;

        // adapted from BasicListUI.Handler.keyTyped()
        protected void changeSelection(KeyEvent e) {
            int col = -1;
            if (treeTable.isColumnFocusEnabled()) {
                col = treeTable.getLeadSelectionColumn();
                if (col >= treeTable.getColumnCount())
                    col = -1;
            }
            if (col < 0) {
                col = Math.max(0, treeTable.getHierarchicalColumn());
                if (col >= treeTable.getColumnCount())
                    return;
            }

            if (treeTable.getRowCount() == 0
                    || e.isAltDown()
                    || e.isControlDown()
                    || e.isMetaDown()
                    || isNavigationKey(table, e)) {
                // Nothing to select
                return;
            }

            boolean startingFromSelection = true;
            char c = e.getKeyChar();
            long time = e.getWhen();
            int startIndex = table.getSelectionModel().getLeadSelectionIndex();
            if (time - lastTime < timeFactor) {
                typedString += c;
                if((prefix.length() == 1) && (c == prefix.charAt(0))) {
                    // Subsequent same key presses move the keyboard focus to the next
                    // object that starts with the same letter.
                    startIndex++;
                } else {
                    prefix = typedString;
                }
            } else {
                startIndex++;
                typedString = Character.toString(c);
                prefix = typedString;
            }
            lastTime = time;

            if (startIndex < 0 || startIndex >= table.getRowCount()) {
                startingFromSelection = false;
                startIndex = 0;
            }
            int index = treeTable.getNextMatch(prefix, startIndex, col, Position.Bias.Forward);
            if (index >= 0) {
                table.changeSelection(index, col, false, false);
            } else if (startingFromSelection) { // wrap
                index = treeTable.getNextMatch(prefix, startIndex, col, Position.Bias.Forward);
                if (index >= 0) {
                    table.changeSelection(index, col, false, false);
                }
            }
        }

        private boolean isNavigationKey(JTable table, KeyEvent event) {
            InputMap inputMap = table.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
            return inputMap != null
                    && inputMap.get(KeyStroke.getKeyStroke(event.getKeyCode(), 0)) != null;
        }

    }

    // adapted from BasicTableUI.paintDropLines...
    private void paintDropLines(Graphics g) {
        TreeTable.DropLocation loc = treeTable.getDropLocation();
        if (loc == null || !(loc.isInsertRow() || loc.isInsertColumn()))
            return;

        Color color = UIManager.getColor("Table.dropLineColor");
        Color shortColor = UIManager.getColor("Table.dropLineShortColor");
        if (color == null && shortColor == null) {
            return;
        }

        Rectangle rect;

        rect = getHDropLineRect(loc);
        if (rect != null) {
            int x = rect.x;
            int w = rect.width;
            if (color != null) {
                extendRect(rect, true);
                g.setColor(color);
                g.fillRect(rect.x, rect.y, rect.width, rect.height);
            }
//			if (!loc.isInsertColumn() && shortColor != null) {
//				g.setColor(shortColor);
//				if (loc.getColumn() == treeTable.getHierarchicalColumn()) {
//					TreePath path = loc.getPath();
//					Object child = tree.getModel().getChild(
//							path.getLastPathComponent(), 0);
//					Rectangle node = tree.getPathBounds(
//							path.pathByAddingChild(child));
//					if (node != null) {
//						x += node.x;
//						w -= node.x;
//					}
//				}
//				g.fillRect(x, rect.y, w, rect.height);
//			}
        }

        rect = getVDropLineRect(loc);
        if (rect != null) {
            int y = rect.y;
            int h = rect.height;
            if (color != null) {
                extendRect(rect, false);
                g.setColor(color);
                g.fillRect(rect.x, rect.y, rect.width, rect.height);
            }
//			if (!loc.isInsertRow() && shortColor != null) {
//				g.setColor(shortColor);
//				g.fillRect(rect.x, y, rect.width, h);
//			}
        }
    }

    // BasicTableUI.getHDropLineRect...
    private Rectangle getHDropLineRect(TreeTable.DropLocation loc) {
        if (!loc.isInsertRow()) {
            return null;
        }

        int row = loc.getRow();
        int col = loc.getColumn();
        if (col >= table.getColumnCount()) {
            col--;
        }

        Rectangle rect = table.getCellRect(row, col, true);

        if (row >= table.getRowCount()) {
            row--;
            Rectangle prevRect = table.getCellRect(row, col, true);
            rect.y = prevRect.y + prevRect.height;
        }

        if (rect.y == 0) {
            rect.y = -1;
        } else {
            rect.y -= 2;
        }

        rect.height = 3;

        return rect;
    }

    // BasicTableUI.getVDropLineRect...
    private Rectangle getVDropLineRect(TreeTable.DropLocation loc) {
        if (!loc.isInsertColumn()) {
            return null;
        }

        boolean ltr = table.getComponentOrientation().isLeftToRight();
        int col = loc.getColumn();
        Rectangle rect = table.getCellRect(loc.getRow(), col, true);

        if (col >= table.getColumnCount()) {
            col--;
            rect = table.getCellRect(loc.getRow(), col, true);
            if (ltr) {
                rect.x = rect.x + rect.width;
            }
        } else if (!ltr) {
            rect.x = rect.x + rect.width;
        }

        if (rect.x == 0) {
            rect.x = -1;
        } else {
            rect.x -= 2;
        }

        rect.width = 3;

        return rect;
    }

    // BasicTableUI.extendRect...
    private Rectangle extendRect(Rectangle rect, boolean horizontal) {
        if (rect == null) {
            return rect;
        }

        if (horizontal) {
            rect.x = 0;
            rect.width = table.getWidth();
        } else {
            rect.y = 0;

            if (table.getRowCount() != 0) {
                Rectangle lastRect = table.getCellRect(table.getRowCount() - 1, 0, true);
                rect.height = lastRect.y + lastRect.height;
            } else {
                rect.height = table.getHeight();
            }
        }

        return rect;
    }
}
