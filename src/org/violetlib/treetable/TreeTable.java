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
package org.violetlib.treetable;

import java.awt.*;
import java.awt.dnd.*;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.*;
import javax.swing.*;
import javax.swing.RowSorter.SortKey;
import javax.swing.border.Border;
import javax.swing.event.*;
import javax.swing.plaf.UIResource;
import javax.swing.table.*;
import javax.swing.text.Position;
import javax.swing.tree.*;

import org.violetlib.aqua.AquaUtils;
import org.violetlib.treetable.event.*;
import org.violetlib.treetable.ui.BasicTreeTableUI;
import org.violetlib.treetable.ui.TableInterface;
import org.violetlib.treetable.ui.TreeInterface;
import org.violetlib.treetable.ui.TreeTableUI;

public class TreeTable extends JComponent implements Scrollable {

    public TreeTable() {
        this(new DefaultTreeTableNode());
    }

    public TreeTable(TreeTableNode root) {
        this(new DefaultTreeModel(root), new DefaultTreeColumnModel(root));
    }

    public TreeTable(TreeModel tm, TreeColumnModel rm) {
        this(tm, rm, null);
    }

    public TreeTable(TreeModel tm, TreeColumnModel tcm, TableColumnModel cm) {
        if (tm == null || tcm == null)
            throw new NullPointerException();
        adapter = createAdapter(tm, tcm);
        columnModel = cm;
        toolTipMap = getDefaultToolTipMap();
        setFocusable(true);
        setOpaque(true);
        updateUI();
        ToolTipManager.sharedInstance().registerComponent(this);
    }

    protected Adapter adapter;

    private TreeInterface tree;

    private TableInterface table;

    private TableColumnModel columnModel;

    private JTable rowHeader;

    private TreeTableCellRenderer focusRenderer;

    private Color alternateRowColor;

    private IconMap iconMap;

    private ToolTipMap<TreeTable> toolTipMap;

    private Icon openIcon;

    private Icon closedIcon;

    private Icon leafIcon;

    private Icon ascendingSortIcon;

    private Icon descendingSortIcon;

    private HashMap<Class<?>,TreeTableCellRenderer> defaultRenderers;

    private HashMap<Class<?>,TreeTableCellEditor> defaultEditors;

    private HashMap<TreePath,SortKey> sortedPaths = new HashMap<TreePath,SortKey>();

    private int rowMargin = 0;

    private boolean expandsSortedNodes = true;

    private boolean nodeSortingEnabled = true;

    private boolean columnFocusEnabled = true;

    private boolean autoCreateColumnHeader = true;

    private boolean autoCreateRowHeader = false;

    private boolean autoCreateRowSorter = false;

    private static final String uiClassID = "TreeTableUI";

    @Override
    public String getUIClassID() {
        return uiClassID;
    }

    public void setUI(TreeTableUI ui) {
        boolean addExpandLis = false;
        boolean addWillExpandLis = false;
        if (table != null) {
            table.removePropertyChangeListener(adapter);
            tree.removePropertyChangeListener(adapter);
            int lisCount = listenerList.getListenerCount(TreeExpansionListener.class);
            if (lisCount > 0) {
                tree.removeTreeExpansionListener(adapter);
                addExpandLis = true;
            }
            lisCount = listenerList.getListenerCount(TreeWillExpandListener.class);
            if (lisCount > 0) {
                tree.removeTreeWillExpandListener(adapter);
                addWillExpandLis = true;
            }
            getSelectionModel().removeTreeSelectionListener(adapter);
            if (getAutoCreateColumnsFromModel())
                columnModel = null;
            updateUIProperties();
        }
        super.setUI(ui);
        tree = getUI().getTreeInterface(this);
        table = getUI().getTableInterface(this);
//		if (columnModel == null) {
//			columnModel = table.getColumnModel();
//			int hc = getTreeColumnModel().getHierarchicalColumn();
//			if (hc >= 0)
//				columnModel.getColumn(hc).setPreferredWidth(150);
//		}
        getSelectionModel().addTreeSelectionListener(adapter);
        table.addPropertyChangeListener(adapter);
        tree.addPropertyChangeListener(adapter);
        if (addExpandLis)
            tree.addTreeExpansionListener(adapter);
        if (addWillExpandLis)
            tree.addTreeWillExpandListener(adapter);
        if (getAutoCreateColumnHeader() && isDisplayable())
            configureEnclosingScrollPane();
    }

    public TreeTableUI getUI() {
        return (TreeTableUI)ui;
    }

    @Override
    public void updateUI() {
        if (UIManager.get(getUIClassID()) != null) {
            setUI(UIManager.getUI(this));
        } else if (ui != null && ui.getClass() == BasicTreeTableUI.class) {
            updateUIProperties();
            ((BasicTreeTableUI)ui).updateUI();
        } else {
            setUI(new BasicTreeTableUI());
        }
    }

    protected void updateUIProperties() {
        if (leafIcon instanceof UIResource)
            leafIcon = null;
        if (openIcon instanceof UIResource)
            openIcon = null;
        if (closedIcon instanceof UIResource)
            closedIcon = null;
        if (ascendingSortIcon instanceof UIResource)
            ascendingSortIcon = null;
        if (descendingSortIcon instanceof UIResource)
            descendingSortIcon = null;
        updateUIRenderers();
        updateUIEditors();
    }

    @SuppressWarnings("unchecked")
    private void updateUIRenderers() {
        if (defaultRenderers == null && columnModel == null)
            return;
        IdentityHashMap components = new IdentityHashMap();
        if (defaultRenderers != null) {
            for (Map.Entry<Class<?>,TreeTableCellRenderer> entry : defaultRenderers.entrySet()) {
                if (entry.getValue() instanceof JComponent) {
                    components.put(entry.getValue(), null);
                } else if (!(entry.getValue() instanceof Component)) {
                    Class<?> cls = entry.getKey();
                    TreeColumnModel cm = adapter.treeColumnModel;
                    for (int col=cm.getColumnCount(); --col>=0;) {
                        if (cls.isAssignableFrom(cm.getColumnClass(col))) {
                            addComponents(components, entry.getValue(),
                                    convertColumnIndexToView(col));
                        }
                    }
                }
            }
        }
        if (columnModel != null) {
            for (int col=columnModel.getColumnCount(); --col>=0;) {
                Object r = columnModel.getColumn(col).getCellRenderer();
                if (r instanceof JComponent) {
                    components.put(r, null);
                } else if (!(r instanceof Component)
                        && r instanceof TreeTableCellRenderer) {
                    addComponents(components, (TreeTableCellRenderer)r, col);
                }
            }
        }
        for (JComponent c : (Set<JComponent>)components.keySet())
            c.updateUI();
    }

    @SuppressWarnings("unchecked")
    private void addComponents(IdentityHashMap components, TreeTableCellRenderer r, int col) {
        try {
            int row;
            Object value;
            if (getRowCount() == 0) {
                row = -1;
                value = null;
            } else {
                row = 0;
                value = adapter.getValueAt(0, col);
            }
            if (col == adapter.treeColumnModel.getHierarchicalColumn()) {
                Component c;
                if (row < 0) {
                    c = r.getTreeTableCellRendererComponent(
                            this, value, false, false, row, col, false, true);
                } else {
                    TreePath path = getPathForRow(0);
                    c = r.getTreeTableCellRendererComponent(
                            this, value, false, false, row, col,
                            isExpanded(path), isLeaf(path));
                }
                if (c instanceof JComponent)
                    components.put(c, c);
            } else {
                Component c = r.getTreeTableCellRendererComponent(
                        this, value, false, false, row, col);
                if (c instanceof JComponent)
                    components.put(c, c);
            }
        } catch (Exception e) { /* ignore */ }
    }

    private void updateUIEditors() {
        // TODO

    }

    public void processTreeExpansion(TreePath path, int rowsAdded) {
        int row = getRowForPath(path);
        adapter.fireTableRowsInserted(row+1, row+rowsAdded);
        adapter.updateSorter(path, true);
        if (getRowHeight() <= 0)
            updateTableRowHeights(row+1, row+rowsAdded+1);
    }

    public void processTreeCollapse(TreePath path, int rowsRemoved) {
        int row = getRowForPath(path);
        adapter.fireTableRowsDeleted(row+1, row+rowsRemoved);
        adapter.updateSorter(path, false);
    }

    @Override
    public void addNotify() {
        super.addNotify();
        configureEnclosingScrollPane();
    }

    @Override
    public void removeNotify() {
        super.removeNotify();
        unconfigureEnclosingScrollPane();
    }

    private JScrollPane getScrollPaneAncestor() {
        return AquaUtils.getScrollPaneAncestor(this);
    }

    // JTable.configureEnclosingScrollPane...
    protected void configureEnclosingScrollPane() {
        if (!getAutoCreateColumnHeader() && !getAutoCreateRowHeader())
            return;
        JScrollPane scrollPane = getScrollPaneAncestor();
        if (scrollPane == null)
            return;
        if (getAutoCreateColumnHeader()) {
            scrollPane.setColumnHeaderView(getTableHeader());
            //  scrollPane.getViewport().setBackingStoreEnabled(true);
            Border border = scrollPane.getBorder();
            if (border == null || border instanceof UIResource) {
                Border scrollPaneBorder =
                    UIManager.getBorder("Table.scrollPaneBorder");
                if (scrollPaneBorder != null) {
                    scrollPane.setBorder(scrollPaneBorder);
                }
            }
        }
        if (getAutoCreateRowHeader()) {
            createRowHeader();
            scrollPane.setRowHeaderView(rowHeader);
        }
    }

    // JTable.unconfigureEnclosingScrollPane...
    protected void unconfigureEnclosingScrollPane() {
        if (!getAutoCreateColumnHeader() && !getAutoCreateRowHeader())
            return;
        JScrollPane scrollPane = getScrollPaneAncestor();
        if (scrollPane == null)
            return;
        if (getAutoCreateColumnHeader())
            scrollPane.setColumnHeaderView(null);
        if (getAutoCreateRowHeader()) {
            scrollPane.setRowHeaderView(null);
            rowHeader = null;
        }
    }

    private void createRowHeader() {
        RowHeaderAdapter adapter = new RowHeaderAdapter(getTableModel());
        rowHeader = new JTable(adapter, null, getRowSelectionModel());
        rowHeader.addPropertyChangeListener("UI", adapter);
        rowHeader.setAutoCreateColumnsFromModel(false);
        rowHeader.setRowMargin(0);
        rowHeader.getColumnModel().setColumnMargin(0);
        rowHeader.setFocusable(false);
        TreeTable.updateRowHeaderRenderer(rowHeader);
        int ht = getRowHeight();
        if (ht > 0) {
            rowHeader.setRowHeight(ht);
        } else {
            updateRowHeaderHeights(0, getRowCount());
        }
    }

    private static void updateRowHeaderRenderer(JTable rowHeader) {
        TableCellRenderer r = rowHeader.getTableHeader().getDefaultRenderer();
        if (r instanceof JLabel) {
            JLabel l = (JLabel)r;
            l.setHorizontalAlignment(JLabel.CENTER);
        }
        rowHeader.getColumnModel().getColumn(0).setCellRenderer(r);
        Dimension size = r.getTableCellRendererComponent(
                rowHeader, "9999", false, false, -1, -1).getPreferredSize();
        rowHeader.setPreferredScrollableViewportSize(size);
    }

    private void updateRowHeaderHeights(int fromRow, int toRow) {
        for (int row=fromRow; row<toRow; row++) {
            rowHeader.setRowHeight(row, getRowHeight(row));
        }
    }

    private boolean hasTreeTableMouseListener = false;

    private boolean hasTreeTableMouseMotionListener = false;

    private <T extends EventListener> void addListener(Class<T> cls, T l) {
        if (l == null)
            return;
        int count = listenerList.getListenerCount(cls);
        listenerList.add(cls, l);
        if (count == 0) {
            if (cls == TreeExpansionListener.class) {
                tree.addTreeExpansionListener(adapter);
            } else if (cls == TreeWillExpandListener.class) {
                tree.addTreeWillExpandListener(adapter);
            } else if (cls == TreeTableMouseListener.class) {
                hasTreeTableMouseListener = true;
            } else if (cls == TreeTableMouseMotionListener.class) {
                hasTreeTableMouseMotionListener = true;
            }
        }
    }

    private <T extends EventListener> void removeListener(Class<T> cls, T l) {
        if (l == null)
            return;
        listenerList.remove(cls, l);
        int count = listenerList.getListenerCount(cls);
        if (count == 0) {
            if (cls == TreeExpansionListener.class) {
                tree.removeTreeExpansionListener(adapter);
            } else if (cls == TreeWillExpandListener.class) {
                tree.removeTreeWillExpandListener(adapter);
            } else if (cls == TreeTableMouseListener.class) {
                hasTreeTableMouseListener = false;
            } else if (cls == TreeTableMouseMotionListener.class) {
                hasTreeTableMouseMotionListener = false;
            }
        }
    }

    public void addTreeExpansionListener(TreeExpansionListener l) {
        addListener(TreeExpansionListener.class, l);
    }

    public void removeTreeExpansionListener(TreeExpansionListener l) {
        removeListener(TreeExpansionListener.class, l);
    }

    private void fireTreeExpansionEvent(TreePath path, boolean exp) {
        TreeExpansionEvent e = new TreeExpansionEvent(this, path);
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==TreeExpansionListener.class) {
                TreeExpansionListener lis = (TreeExpansionListener)listeners[i+1];
                if (exp) {
                    lis.treeExpanded(e);
                } else {
                    lis.treeCollapsed(e);
                }
            }
        }
    }

    public void addTreeWillExpandListener(TreeWillExpandListener l) {
        addListener(TreeWillExpandListener.class, l);
    }

    public void removeTreeWillExpandListener(TreeWillExpandListener l) {
        removeListener(TreeWillExpandListener.class, l);
    }

    private void fireTreeWillExpandEvent(TreePath path, boolean exp) throws ExpandVetoException {
        TreeExpansionEvent e = new TreeExpansionEvent(this, path);
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==TreeWillExpandListener.class) {
                TreeWillExpandListener lis = (TreeWillExpandListener)listeners[i+1];
                if (exp) {
                    lis.treeWillExpand(e);
                } else {
                    lis.treeWillCollapse(e);
                }
            }
        }
    }

    /**
     * Provides extra information about the mouse event, see
     * TreeTableMouseEvent for more details. Consuming the
     * TreeTableMouseEvent will also consume the source MouseEvent.
     * TreeTableMouseListeners are notified before MouseListeners.
     *
     * @see #addTreeTableMouseMotionListener(TreeTableMouseMotionListener)
     * @see #addMouseListener(java.awt.event.MouseListener)
     */
    public void addTreeTableMouseListener(TreeTableMouseListener l) {
        addListener(TreeTableMouseListener.class, l);
    }

    public void removeTreeTableMouseListener(TreeTableMouseListener l) {
        removeListener(TreeTableMouseListener.class, l);
    }

    /**
     * Provides extra information about the mouse event, see
     * TreeTableMouseEvent for more details. Consuming the
     * TreeTableMouseEvent will also consume the source MouseEvent.
     * TreeTableMouseMotionListeners are notified before MouseMotionListeners.
     *
     * @see #addTreeTableMouseListener(TreeTableMouseListener)
     * @see #addMouseMotionListener(java.awt.event.MouseMotionListener)
     */
    public void addTreeTableMouseMotionListener(TreeTableMouseMotionListener l) {
        addListener(TreeTableMouseMotionListener.class, l);
    }

    public void removeTreeTableMouseMotionListener(TreeTableMouseMotionListener l) {
        removeListener(TreeTableMouseMotionListener.class, l);
    }

    @Override
    protected void processMouseEvent(MouseEvent e) {
        if (hasTreeTableMouseListener)
            fireTreeTableMouseEvent(e);
        super.processMouseEvent(e);
    }

    private void fireTreeTableMouseEvent(MouseEvent e) {
        switch (e.getID()) {
        case MouseEvent.MOUSE_ENTERED: case MouseEvent.MOUSE_EXITED:
            return;
        }
        TreeTableMouseEvent evt = new TreeTableMouseEvent(this, e);
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==TreeTableMouseListener.class) {
                TreeTableMouseListener lis = (TreeTableMouseListener)listeners[i+1];
                switch (e.getID()) {
                case MouseEvent.MOUSE_PRESSED:
                    lis.mousePressed(evt); break;
                case MouseEvent.MOUSE_RELEASED:
                    lis.mouseReleased(evt); break;
                case MouseEvent.MOUSE_CLICKED:
                    lis.mouseClicked(evt); break;
                }
            }
        }
        if (evt.isConsumed())
            e.consume();
    }

    @Override
    protected void processMouseMotionEvent(MouseEvent e) {
        if (hasTreeTableMouseMotionListener)
            fireTreeTableMouseMotionEvent(e);
        super.processMouseMotionEvent(e);
    }


    private void fireTreeTableMouseMotionEvent(MouseEvent e) {
        TreeTableMouseEvent evt = new TreeTableMouseEvent(this, e);
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==TreeTableMouseMotionListener.class) {
                TreeTableMouseMotionListener lis = (TreeTableMouseMotionListener)listeners[i+1];
                switch (e.getID()) {
                case MouseEvent.MOUSE_MOVED:
                    lis.mouseMoved(evt); break;
                case MouseEvent.MOUSE_DRAGGED:
                    lis.mouseDragged(evt); break;
                }
            }
        }
        if (evt.isConsumed())
            e.consume();
    }

    @Override
    public void doLayout() {
        table.doLayout();
        tree.doLayout();
        super.doLayout();
    }

    public void invalidateAllRows() {
        TreeModel model = adapter;
        TreePath root = new TreePath(model.getRoot());
        if (tree.isRootVisible())
            adapter.invalidatePaths(root, null, null);
        Enumeration<TreePath> paths = tree.getExpandedDescendants(root);
        if (paths == null)
            return;
        while (paths.hasMoreElements()) {
            TreePath path = paths.nextElement();
            Object parent = path.getLastPathComponent();
            int count = model.getChildCount(parent);
            if (count > 0) {
                int[] childIndices = new int[count];
                Object[] childNodes = new Object[count];
                for (int i=count; --i>=0;) {
                    childIndices[i] = i;
                    childNodes[i] = model.getChild(parent, i);
                }
                adapter.invalidatePaths(path, childIndices, childNodes);
            }
        }
    }

    public void invalidatePath(TreePath path) {
        adapter.invalidatePath(path);
        if (getRowHeight() <= 0) {
            int row = getRowForPath(path);
            updateTableRowHeights(row, row+1);
        }
    }

    /**
     * For variable row heights, sync table row height to
     * corresponding tree row height.
     */
    protected void updateTableRowHeights() {
        updateTableRowHeights(0, tree.getRowCount());
    }

    /**
     * Sync table row heights to corresponding tree row height
     * for rows <code>fromRow</code> (inclusive) to
     * <code>toRow</code> exclusive.
     */
    protected void updateTableRowHeights(int fromRow, int toRow) {
        assert (tree.getRowHeight() <= 0);
        for (int row=toRow; --row>=fromRow;)
            table.setRowHeight(row, getRowBounds(row).height);
        if (rowHeader != null)
            updateRowHeaderHeights(fromRow, toRow);
    }

    public boolean getAutoCreateColumnHeader() {
        return autoCreateColumnHeader;
    }

    public void setAutoCreateColumnHeader(boolean autoCreateColumnHeader) {
        boolean oldValue = getAutoCreateColumnHeader();
        this.autoCreateColumnHeader = autoCreateColumnHeader;
        firePropertyChange("autoCreateColumnHeader", oldValue, getAutoCreateColumnHeader());
    }

    public boolean getAutoCreateRowHeader() {
        return autoCreateRowHeader;
    }

    public void setAutoCreateRowHeader(boolean autoCreateRowHeader) {
        boolean oldValue = getAutoCreateRowHeader();
        this.autoCreateRowHeader = autoCreateRowHeader;
        firePropertyChange("autoCreateRowHeader", oldValue, getAutoCreateRowHeader());
    }

    public TreeTableCellRenderer getFocusRenderer() {
        return focusRenderer;
    }

    public void setFocusRenderer(TreeTableCellRenderer renderer) {
        TreeTableCellRenderer oldValue = getFocusRenderer();
        focusRenderer = renderer;
        this.firePropertyChange("focusRenderer", oldValue, getFocusRenderer());
        if (isValid())
            repaint(getLeadSelectionPath());
    }

    public ToolTipMap<TreeTable> getDefaultToolTipMap() {
        return adapter;
    }

    public ToolTipMap<TreeTable> getToolTipMap() {
        return toolTipMap;
    }

    public void setToolTipMap(ToolTipMap<TreeTable> toolTipMap) {
        ToolTipMap<TreeTable> oldValue = getToolTipMap();
        this.toolTipMap = toolTipMap;
        firePropertyChange("toolTipMap", oldValue, getToolTipMap());
    }

    public String getToolTipText(MouseEvent e) {
        return getToolTipMap() == null ? getToolTipText() :
            getToolTipMap().getToolTipText(this, e);
    }

    public TableColumnModel getColumnModel() {
        return columnModel;
    }

    public void setColumnModel(TableColumnModel columnModel) {
        TableColumnModel oldValue = getColumnModel();
        this.columnModel = columnModel;
        firePropertyChange("columnModel", oldValue, getColumnModel());
    }

    public TreeModel getTreeModel() {
        return adapter.treeModel;
    }

    public void setTreeModel(TreeModel treeModel) {
        TreeModel oldValue = getTreeModel();
        adapter.setTreeModel(treeModel);
        firePropertyChange("treeModel", oldValue, getTreeModel());
        if (getAutoCreateRowSorter())
            autoCreateRowSorter();
    }

    public TreeColumnModel getTreeColumnModel() {
        return adapter.treeColumnModel;
    }

    public void setTreeColumnModel(TreeColumnModel treeColumnModel) {
        TreeColumnModel oldValue = getTreeColumnModel();
        adapter.treeColumnModel = treeColumnModel;
        adapter.fireTableStructureChanged();
        firePropertyChange("treeColumnModel", oldValue, getTreeColumnModel());
        if (getAutoCreateRowSorter())
            autoCreateRowSorter();
    }

    /**
     * Changes to the TreeModel/RowModel as well as row insertion/removal
     * due to tree expansion/collapse can be listened to in terms of a
     * TableModelListener by adding the TableModelListener to this TableModel.
     *
     * @return TableModel view of the TreeModel/RowModel combination.
     * @see #getTreeModel()
     * @see #getTreeColumnModel()
     */
    public TableModel getTableModel() {
        return adapter;
    }

    public TreeTableModel getTreeTableModel() {
        return adapter;
    }

    public TreeTableSorter<? extends TreeModel, ? extends TreeColumnModel> getRowSorter() {
        return adapter.rowSorter;
    }

    public void setRowSorter(TreeTableSorter<? extends TreeModel, ? extends TreeColumnModel> rowSorter) {
        TreeTableSorter<?,?> oldValue = getRowSorter();
        adapter.setRowSorter(rowSorter);
        firePropertyChange("rowSorter", oldValue, getRowSorter());
    }

    public boolean isNodeSortingEnabled() {
        return nodeSortingEnabled;
    }

    public void setNodeSortingEnabled(boolean nodeSortingEnabled) {
        boolean oldValue = isNodeSortingEnabled();
        this.nodeSortingEnabled = nodeSortingEnabled;
        firePropertyChange("nodeSortingEnabled", oldValue, isNodeSortingEnabled());
        if (!sortedPaths.isEmpty())
            repaint();
    }

    public boolean getAutoCreateRowSorter() {
        return autoCreateRowSorter;
    }

    public void setAutoCreateRowSorter(boolean autoCreateRowSorter) {
        boolean oldValue = getAutoCreateRowSorter();
        this.autoCreateRowSorter = autoCreateRowSorter;
        if (getAutoCreateRowSorter())
            autoCreateRowSorter();
        firePropertyChange("autoCreateRowSorter", oldValue, getAutoCreateRowSorter());
    }

    private void autoCreateRowSorter() {
        setRowSorter(new DefaultTreeTableSorter<TreeModel,TreeColumnModel,Object>(
                getTreeModel(), getTreeColumnModel()));
    }

    public RowSorter<?> getSorterForPath(TreePath path) {
        return adapter.getSorter(path);
    }

    /**
     * @return columnFocusEnabed
     * @see #setColumnFocusEnabled(boolean)
     */
    public boolean isColumnFocusEnabled() {
        return columnFocusEnabled;
    }

    /**
     * If false, the focus is draw around the entire focused row.
     */
    public void setColumnFocusEnabled(boolean columnFocusEnabled) {
        boolean oldValue = isColumnFocusEnabled();
        this.columnFocusEnabled = columnFocusEnabled;
        firePropertyChange("columnFocusEnabled", oldValue, isColumnFocusEnabled());
        repaint(getLeadSelectionPath());
    }

    public TableColumn getColumn(Object identifier) {
        return columnModel.getColumn(columnModel.getColumnIndex(identifier));
    }

    public Color getAlternateRowColor() {
        return alternateRowColor;
    }

    public void setAlternateRowColor(Color alternateRowColor) {
        Color oldValue = getAlternateRowColor();
        this.alternateRowColor = alternateRowColor;
        firePropertyChange("alternateRowColor", oldValue, getAlternateRowColor());
    }

    public void setDefaultRenderer(Class<?> columnClass, TreeTableCellRenderer renderer) {
        defaultRenderers = putDefault(columnClass, renderer, defaultRenderers);
    }

    public TreeTableCellRenderer getDefaultRenderer(Class<?> columnClass) {
        TreeTableCellRenderer r = defaultRenderers == null ?  null :
            getDefault(columnClass, defaultRenderers);
        return r != null ? r : ui == null ? null :
            getUI().getDefaultRenderer(this, columnClass);
    }

    public void setDefaultEditor(Class<?> columnClass, TreeTableCellEditor editor) {
        defaultEditors = putDefault(columnClass, editor, defaultEditors);
    }

    public TreeTableCellEditor getDefaultEditor(Class<?> columnClass, int column) {
        TreeTableCellEditor e = defaultEditors == null ? null :
            getDefault(columnClass, defaultEditors);
        return e != null ? e : ui == null ? null :
            getUI().getDefaultEditor(this, columnClass, column);
    }

    private static <T> HashMap<Class<?>,T> putDefault(
            Class<?> key, T value, HashMap<Class<?>,T> defaults) {
        if (defaults == null) {
            if (value == null)
                return null;
            defaults = new HashMap<Class<?>,T>(8);
        }
        if (value != null) {
            defaults.put(key, value);
        } else {
            defaults.remove(key);
        }
        return defaults;
    }

    private static <T> T getDefault(Class<?> cls, HashMap<Class<?>,T> defaults) {
        if (cls == null)
            return null;
        T def = defaults.get(cls);
        return def != null ? def : getDefault(cls.getSuperclass(), defaults);
    }

    public TreeTableCellRenderer getCellRenderer(int row, int column) {
        if (column >= 0 && column < getColumnModel().getColumnCount()) {
            TableCellRenderer renderer = getColumnModel()
                .getColumn(column).getCellRenderer();
            if (renderer instanceof TreeTableCellRenderer)
                return (TreeTableCellRenderer)renderer;
            return getDefaultRenderer(getTreeColumnModel().getColumnClass(
                    convertColumnIndexToModel(column)));
        }
        return getDefaultRenderer(Object.class);
    }

    public TreeTableCellEditor getCellEditor(int row, int column) {
        if (column >= 0 && column < getColumnModel().getColumnCount()) {
            TableCellEditor editor = getColumnModel()
                .getColumn(column).getCellEditor();
            if (editor instanceof TreeTableCellEditor)
                return (TreeTableCellEditor)editor;
            return getDefaultEditor(getTreeColumnModel().getColumnClass(
                    convertColumnIndexToModel(column)), column);
        }
        return getDefaultEditor(Object.class, column);
    }

    public boolean isCellEditable(int row, int column) {
        return adapter.isCellEditable(row, convertColumnIndexToModel(column));
    }

    public boolean startEditingAtPath(TreePath path) {
        return editCellAt(getRowForPath(path), getHierarchicalColumn());
    }

    public boolean startEditingAtRow(int row) {
        return editCellAt(row, getHierarchicalColumn());
    }

    public boolean editCellAt(int row, int column) {
        return table.editCellAt(row, column);
    }

    public boolean isEditing() {
        return table.isEditing();
    }

    public Component getEditorComponent() {
        return table.getEditorComponent();
    }

    public int getEditingColumn() {
        return table.getEditingColumn();
    }

    public int getEditingRow() {
        return table.getEditingRow();
    }

    public TreeTableCellEditor getCellEditor() {
        return (TreeTableCellEditor)table.getCellEditor();
    }

    /**
     * @return the hierarchical column in view coordinates
     */
    public int getHierarchicalColumn() {
        return convertColumnIndexToView(
                getTreeColumnModel().getHierarchicalColumn());
    }

    public int convertColumnIndexToView(int modelColumnIndex) {
        return table.convertColumnIndexToView(modelColumnIndex);
    }

    public int convertColumnIndexToModel(int viewColumnIndex) {
        return table.convertColumnIndexToModel(viewColumnIndex);
    }

    public int convertNodeIndexToView(Object parent, int modelIndex) {
        return adapter.convertIndexToView(parent, modelIndex);
    }

    public int convertNodeIndexToModel(Object parent, int viewIndex) {
        return adapter.convertIndexToModel(parent, viewIndex);
    }

    public Object getValueAt(int row, int column) {
        return adapter.getValueAt(row, convertColumnIndexToModel(column));
    }

    public Object getNode(int row) {
        return adapter.getNode(row);
    }

    public Icon getLeafIcon() {
        if (leafIcon == null)
            leafIcon = UIManager.getIcon("Tree.leafIcon");
        return leafIcon;
    }

    public void setLeafIcon(Icon leafIcon) {
        Icon oldValue = this.leafIcon;
        this.leafIcon = leafIcon;
        firePropertyChange("leafIcon", oldValue, getLeafIcon());
        repaintColumn(getHierarchicalColumn());
    }

    public Icon getOpenIcon() {
        if (openIcon == null)
            openIcon = UIManager.getIcon("Tree.openIcon");
        return openIcon;
    }

    public void setOpenIcon(Icon openIcon) {
        Icon oldValue = this.openIcon;
        this.openIcon = openIcon;
        firePropertyChange("openIcon", oldValue, getOpenIcon());
        repaintColumn(getHierarchicalColumn());
    }

    public Icon getClosedIcon() {
        if (closedIcon == null)
            closedIcon = UIManager.getIcon("Tree.closedIcon");
        return closedIcon;
    }

    public void setClosedIcon(Icon closedIcon) {
        Icon oldValue = this.closedIcon;
        this.closedIcon = closedIcon;
        firePropertyChange("closedIcon", oldValue, getClosedIcon());
        repaintColumn(getHierarchicalColumn());
    }

    private void repaintColumn(int col) {
        if (col < 0 || getRowCount() == 0)
            return;
        Rectangle r = getCellRect(0, col, true);
        r.height = getHeight();
        repaint(r);
    }

    public IconMap getIconMap() {
        return iconMap;
    }

    public void setIconMap(IconMap iconMap) {
        IconMap oldValue = getIconMap();
        this.iconMap = iconMap;
        firePropertyChange("iconMap", oldValue, getIconMap());
        if (isValid())
            repaintColumn(getHierarchicalColumn());
    }

    public Icon getAscendingSortIcon() {
        if (ascendingSortIcon == null)
            ascendingSortIcon = UIManager.getIcon("Table.ascendingSortIcon");
        return ascendingSortIcon;
    }

    public void setAscendingSortIcon(Icon ascendingSortIcon) {
        Icon oldValue = getAscendingSortIcon();
        this.ascendingSortIcon = ascendingSortIcon;
        firePropertyChange("ascendingSortIcon", oldValue, getAscendingSortIcon());
    }

    public Icon getDescendingSortIcon() {
        if (descendingSortIcon == null)
            descendingSortIcon = UIManager.getIcon("Table.descendingSortIcon");
        return descendingSortIcon;
    }

    public void setDescendingSortIcon(Icon descendingSortIcon) {
        Icon oldValue = getDescendingSortIcon();
        this.descendingSortIcon = descendingSortIcon;
        firePropertyChange("descendingSortIcon", oldValue, getDescendingSortIcon());
    }

    public Icon getIconForPath(TreePath path) {
        return getIcon(path.getLastPathComponent(), isExpanded(path), isLeaf(path));
    }

    public Icon getIconForRow(int row) {
        return getIconForPath(getPathForRow(row));
    }

    public Icon getIcon(Object node, boolean expanded, boolean leaf) {
        Icon icon = iconMap == null ? null :
            iconMap.getIcon(this, node, expanded, leaf);
        if (icon == null) {
            if (leaf) {
                icon = getLeafIcon();
            } else if (expanded) {
                icon = getOpenIcon();
            } else {
                icon = getClosedIcon();
            }
        }
        return icon;
    }

    public boolean getAutoCreateColumnsFromModel() {
        return table.getAutoCreateColumnsFromModel();
    }

    public void setAutoCreateColumnsFromModel(boolean autoCreateColumnsFromModel) {
        table.setAutoCreateColumnsFromModel(autoCreateColumnsFromModel);
    }

    public int getAutoResizeMode() {
        return table.getAutoResizeMode();
    }

    public void setAutoResizeMode(int mode) {
        table.setAutoResizeMode(mode);
    }

    public boolean getCellSelectionEnabled() {
        return table.getCellSelectionEnabled();
    }

    public void setCellSelectionEnabled(boolean cellSelectionEnabled) {
        table.setCellSelectionEnabled(cellSelectionEnabled);
        if (getDragEnabled())
            updateDropMode();
    }

    public boolean getColumnSelectionAllowed() {
        return table.getColumnSelectionAllowed();
    }

    public void setColumnSelectionAllowed(boolean columnSelectionAllowed) {
        table.setColumnSelectionAllowed(columnSelectionAllowed);
        if (getDragEnabled())
            updateDropMode();
    }

    public Color getGridColor() {
        return table.getGridColor();
    }

    public void setGridColor(Color gridColor) {
        table.setGridColor(gridColor);
    }

    public Dimension getIntercellSpacing() {
        return table.getIntercellSpacing();
    }

    public void setIntercellSpacing(Dimension intercellSpacing) {
        table.setIntercellSpacing(intercellSpacing);
    }

    public int getRowMargin() {
        return rowMargin;
    }

    public void setRowMargin(int rowMargin) {
        int oldValue = getRowMargin();
        this.rowMargin = rowMargin;
        firePropertyChange("rowMargin", oldValue, getRowMargin());
        if (isValid())
            repaint();
    }

    public boolean getRowSelectionAllowed() {
        return table.getRowSelectionAllowed();
    }

    public void setRowSelectionAllowed(boolean rowSelectionAllowed) {
        table.setRowSelectionAllowed(rowSelectionAllowed);
        if (getDragEnabled())
            updateDropMode();
    }

    public boolean getShowHorizontalLines() {
        return table.getShowHorizontalLines();
    }

    public void setShowHorizontalLines(boolean showHorizontalLines) {
        table.setShowHorizontalLines(showHorizontalLines);
    }

    public boolean getShowVerticalLines() {
        return table.getShowVerticalLines();
    }

    public void setShowVerticalLines(boolean showVerticalLines) {
        table.setShowVerticalLines(showVerticalLines);
    }

    public void setShowGrid(boolean showGrid) {
        table.setShowGrid(showGrid);
    }

    public JTableHeader getTableHeader() {
        return table.getTableHeader();
    }

    public void setTableHeader(JTableHeader tableHeader) {
        table.setTableHeader(tableHeader);
    }

    public Enumeration<TreePath> getExpandedDescendants(TreePath parent) {
        return tree.getExpandedDescendants(parent);
    }

    public void collapsePath(TreePath path) {
        tree.collapsePath(path);
    }

    public void collapseRow(int row) {
        collapsePath(getPathForRow(row));
    }

    public void expandPath(TreePath path) {
        tree.expandPath(path);
    }

    public void expandRow(int row) {
        expandPath(getPathForRow(row));
    }

    public void makeVisible(TreePath path) {
        tree.makeVisible(path);
    }

    public void scrollPathToVisible(TreePath path) {
        tree.scrollPathToVisible(path);
    }

    public void scrollRowToVisible(int row) {
        scrollPathToVisible(getPathForRow(row));
    }

    public Rectangle getPathBounds(TreePath path) {
        return ui == null ? null : getUI().getPathBounds(this, path);
    }

    public TreePath getPathForLocation(int x, int y) {
        return ui == null ? null : getUI().getPathForLocation(this, x, y);
    }

    public TreePath getClosestPathForLocation(int x, int y) {
        return ui == null ? null : getUI().getClosestPathForLocation(this, x, y);
    }

    public TreePath getPathForRow(int row) {
        return tree.getPathForRow(row);
    }

    public Rectangle getRowBounds(int row) {
        return getPathBounds(getPathForRow(row));
    }

    public int getRowCount() {
        return tree.getRowCount();
    }

    public int getColumnCount() {
        return columnModel.getColumnCount();
    }

    public int getRowForLocation(int x, int y) {
        TreePath path = getPathForLocation(x, y);
        return path == null ? -1 : getRowForPath(path);
    }

    public int getClosestRowForLocation(int x, int y) {
        return tree.getClosestRowForLocation(x, y);
    }

    public int getRowForPath(TreePath path) {
        return tree.getRowForPath(path);
    }

    public boolean isCollapsed(int row) {
        return isCollapsed(getPathForRow(row));
    }

    public boolean isCollapsed(TreePath path) {
        return tree.isCollapsed(path);
    }

    public boolean isExpanded(int row) {
        return isExpanded(getPathForRow(row));
    }

    public boolean isExpanded(TreePath path) {
        return tree.isExpanded(path);
    }

    public boolean hasBeenExpanded(TreePath path) {
        return tree.hasBeenExpanded(path);
    }

    public boolean isFixedRowHeight() {
        return tree.isFixedRowHeight();
    }

    public boolean isLargeModel() {
        return tree.isLargeModel();
    }

    public void setLargeModel(boolean largeModel) {
        tree.setLargeModel(largeModel);
    }

    public boolean isRootVisible() {
        return tree.isRootVisible();
    }

    public void setRootVisible(boolean rootVisible) {
        tree.setRootVisible(rootVisible);
    }

    public boolean getScrollsOnExpand() {
        return tree.getScrollsOnExpand();
    }

    public void setScrollsOnExpand(boolean scrollsOnExpand) {
        tree.setScrollsOnExpand(scrollsOnExpand);
    }

    public boolean getShowsRootHandles() {
        return tree.getShowsRootHandles();
    }

    public void setShowsRootHandles(boolean newValue) {
        tree.setShowsRootHandles(newValue);
    }

    public void setToggleClickCount(int clickCount) {
        tree.setToggleClickCount(clickCount);
    }
    public int getToggleClickCount() {
        return tree.getToggleClickCount();
    }

    public int getVisibleRowCount() {
        return tree.getVisibleRowCount();
    }

    public void setVisibleRowCount(int newCount) {
        tree.setVisibleRowCount(newCount);
    }

    public int getRowHeight() {
        return tree.getRowHeight();
    }

    public void setRowHeight(int rowHeight) {
        int oldRowHeight = getRowHeight();
        if (rowHeight > 0) {
            table.setRowHeight(rowHeight);
            if (rowHeader != null)
                rowHeader.setRowHeight(rowHeight);
        }
        tree.setRowHeight(rowHeight);
        if (rowHeight <= 0 && oldRowHeight > 0)
            updateTableRowHeights();
    }

    public int getRowHeight(int row) {
        return table.getRowHeight(row);
    }

    public TreeSelectionModel getSelectionModel() {
        return tree == null ? null : tree.getSelectionModel();
    }

    public void setSelectionModel(TreeSelectionModel selectionModel) {
        if (tree != null)
            tree.setSelectionModel(selectionModel);
    }

    /**
     * Changes to the TreeSelectionModel can be listened to
     * in terms of a ListSelectionModel by adding a
     * ListSelectionListener to this ListSelectionModel.
     *
     * @return ListSelectionModel view of the TreeSelectionModel.
     * @see #getSelectionModel()
     */
    public ListSelectionModel getRowSelectionModel() {
        return adapter;
    }

    public void clearSelection() {
        tree.clearSelection();
    }

    public boolean isSelectionEmpty() {
        return tree.isSelectionEmpty();
    }

    public int getSelectionCount() {
        return tree.getSelectionCount();
    }

    public int getMaxSelectionRow() {
        return tree.getMaxSelectionRow();
    }

    public int getMinSelectionRow() {
        return tree.getMinSelectionRow();
    }

    public boolean isPathSelected(TreePath path) {
        return tree.isPathSelected(path);
    }

    public boolean isRowSelected(int row) {
        return isPathSelected(getPathForRow(row));
    }

    /**
     * @param column the column index
     * @return true if <code>column</code> is selected
     */
    public boolean isColumnSelected(int column) {
        return columnModel.getSelectionModel().isSelectedIndex(column);
    }

    /**
     * @param row the row index
     * @param column the column index
     * @return true if the cell for<code> (row, column) </code>is selected
     */
    public boolean isCellSelected(int row, int column) {
        if (!getRowSelectionAllowed() && !getColumnSelectionAllowed()) {
            return false;
        }
        return (!getRowSelectionAllowed() || isRowSelected(row)) &&
            (!getColumnSelectionAllowed() || isColumnSelected(column));
    }

    public TreePath getAnchorSelectionPath() {
        return tree.getAnchorSelectionPath();
    }

    public void setAnchorSelectionPath(TreePath newPath) {
        tree.setAnchorSelectionPath(newPath);
    }

    public TreePath getLeadSelectionPath() {
        return tree.getLeadSelectionPath();
    }

    public void setLeadSelectionPath(TreePath newPath) {
        tree.setLeadSelectionPath(newPath);
    }

    public int getLeadSelectionRow() {
        return tree.getLeadSelectionRow();
    }

    public int getLeadSelectionColumn() {
        return columnModel.getSelectionModel().getLeadSelectionIndex();
    }

    public boolean getExpandsSelectedPaths() {
        return tree.getExpandsSelectedPaths();
    }

    public void setExpandsSelectedPaths(boolean newValue) {
        tree.setExpandsSelectedPaths(newValue);
    }

    public TreePath getSelectionPath() {
        return tree.getSelectionPath();
    }

    public void setSelectionPath(TreePath path) {
        tree.setSelectionPath(path);
    }

    public int getSelectedRow() {
        return getRowForPath(getSelectionPath());
    }

    public TreePath[] getSelectionPaths() {
        return tree.getSelectionPaths();
    }

    public void setSelectionPaths(TreePath[] paths) {
        tree.setSelectionPaths(paths);
    }

    public int[] getSelectionRows() {
        return tree.getSelectionRows();
    }

    public int[] getSelectedRows() {
        return getSelectionRows();
    }

    public void addSelectionInterval(int index0, int index1) {
        tree.addSelectionInterval(index0, index1);
    }

    public void addSelectionPath(TreePath path) {
        tree.addSelectionPath(path);
    }

    public void addSelectionPaths(TreePath[] paths) {
        tree.addSelectionPaths(paths);
    }

    public void addSelectionRow(int row) {
        addSelectionPath(getPathForRow(row));
    }

    public void addSelectionRows(int[] rows) {
        tree.addSelectionRows(rows);
    }

    public void removeSelectionInterval(int index0, int index1) {
        tree.removeSelectionInterval(index0, index1);
    }

    public void removeSelectionPath(TreePath path) {
        tree.removeSelectionPath(path);
    }

    public void removeSelectionPaths(TreePath[] paths) {
        tree.removeSelectionPaths(paths);
    }

    public void removeSelectionRow(int row) {
        removeSelectionPath(getPathForRow(row));
    }

    public void removeSelectionRows(int[] rows) {
        tree.removeSelectionRows(rows);
    }

    public void setSelectionRows(int[] rows) {
        tree.setSelectionRows(rows);
    }

    public void setSelectionRow(int row) {
        setSelectionPath(getPathForRow(row));
    }

    public void setSelectionInterval(int index0, int index1) {
        tree.setSelectionInterval(index0, index1);
    }

    public void changeSelection(int row, int column, boolean toggle, boolean extend) {
        table.changeSelection(row, column, toggle, extend);
    }

    public Color getSelectionForeground() {
        return table.getSelectionForeground();
    }

    public void setSelectionForeground(Color selectionForeground) {
        table.setSelectionForeground(selectionForeground);
    }

    public Color getSelectionBackground() {
        return table.getSelectionBackground();
    }

    public void setSelectionBackground(Color selectionBackground) {
        table.setSelectionBackground(selectionBackground);
    }

    public int getSelectedRowCount() {
        return getSelectionCount();
    }

    public int getSelectedColumn() {
        return columnModel.getSelectionModel().getMinSelectionIndex();
    }

    public int getSelectedColumnCount() {
        return columnModel.getSelectedColumnCount();
    }

    public Rectangle getCellRect(int row, int column, boolean includeSpacing) {
        return table.getCellBounds(row, column, includeSpacing);
    }

    public int columnAtPoint(Point pt) {
        return table.columnAtPoint(pt);
    }

    public int rowAtPoint(Point pt) {
        return table.rowAtPoint(pt);
    }

    // Scrollable interface

    public Dimension getPreferredScrollableViewportSize() {
        Dimension size;
        if (adapter.treeColumnModel.getHierarchicalColumn() < 0) {
            int ht = getRowHeight();
            if (ht <= 0)
                ht = 20;
            size = new Dimension(tree.getVisibleRowCount()*ht, 0);
        } else {
            size = tree.getPreferredScrollableViewportSize();
        }
        TableColumnModel cm = getColumnModel();
        int width = 0;
        for (int col=cm.getColumnCount(); --col>=0;)
            width += cm.getColumn(col).getPreferredWidth();
        size.width = width + cm.getColumnMargin() * cm.getColumnCount();
        return size;
    }

    /**
     * JTree and JTable have different scrolling behavior,
     * so scroll as if a JTree for vertical scrolls and
     * as a JTable for horizontal scrolls.
     *
     * @param orientation VERTICAL or HORIZONTAL
     * @return tree for VERTICAL, table for HORIZONTAL
     */
    private Scrollable getScrollable(int orientation) {
        return orientation == SwingConstants.VERTICAL ? tree : table;
    }

    @Override
    public int getScrollableBlockIncrement(Rectangle visibleRect,
            int orientation, int direction) {
        return getScrollable(orientation).getScrollableBlockIncrement(
                    visibleRect, orientation, direction);
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        return table.getScrollableTracksViewportHeight();
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return table.getScrollableTracksViewportWidth();
    }

    @Override
    public int getScrollableUnitIncrement(Rectangle visibleRect,
            int orientation, int direction) {
        return getScrollable(orientation).getScrollableUnitIncrement(
                visibleRect, orientation, direction);
    }

    void repaint(TreePath path, int col) {
        if (col < 0) {
            repaint(path);
        } else if (path != null) {
            repaint(getCellRect(getRowForPath(path), col, true));
        }
    }

    void repaint(TreePath path) {
        if (path == null)
            return;
        Rectangle r = tree.getPathBounds(path);
        if (r == null)
            return;
        r.x = 0;
        r.width = getWidth();
        repaint(r);
    }

    protected Adapter createAdapter(TreeModel tm, TreeColumnModel tcm) {
        return new Adapter(tm, tcm);
    }

    protected class Adapter extends AbstractTableModel implements
            TreeTableModel, TreeModelListener, TreeColumnModelListener,
            ListSelectionModel, PropertyChangeListener, TreeTableSorterListener,
            TreeExpansionListener, TreeSelectionListener, TreeWillExpandListener,
            ToolTipMap<TreeTable> {

        Adapter() {}

        public Adapter(TreeModel tm, TreeColumnModel tcm) {
            treeModel = tm;
            treeColumnModel = tcm;
            treeRoot = tm.getRoot();
            tm.addTreeModelListener(this);
            tcm.addTreeColumnModelListener(this);
        }

        protected TreeModel treeModel;

        protected TreeColumnModel treeColumnModel;

        /**
         * Used to determine when TreeModel's root has changed.
         */
        protected Object treeRoot;

        private TreeTableSorter<? extends TreeModel, ? extends TreeColumnModel> rowSorter;

        private boolean ignoreSortedChange = false;

        public void setTreeModel(TreeModel m) {
            if (treeModel != null) {
                treeModel.removeTreeModelListener(this);
            }

            treeModel = m;

            if (treeModel != null) {
                treeModel.addTreeModelListener(this);
                TreeModelEvent e = new TreeModelEvent(treeModel, new TreePath(treeModel.getRoot()));
                treeStructureChanged(e);
            }
        }

        public void setRowSorter(TreeTableSorter<? extends TreeModel, ? extends TreeColumnModel> sorter) {
            if (rowSorter != null)
                rowSorter.removeTreeTableSorterListener(this);
            rowSorter = sorter;
            if (sorter != null) {
                sorter.addTreeTableSorterListener(this);
            }
        }

        // TableModel interface

        @Override
        public int getColumnCount() {
            return treeColumnModel.getColumnCount();
        }

        @Override
        public String getColumnName(int column) {
            return treeColumnModel.getColumnName(column);
        }

        @Override
        public Class<?> getColumnClass(int column) {
            return treeColumnModel.getColumnClass(column);
        }

        @Override
        public int getRowCount() {
            return tree.getRowCount();
        }

        public Object getNode(int row) {
            return getPathForRow(row).getLastPathComponent();
        }

        @Override
        public Object getValueAt(int row, int column) {
            return treeColumnModel.getValueAt(getNode(row), column);
        }

        @Override
        public void setValueAt(Object value, int row, int column) {
            treeColumnModel.setValueAt(value, getNode(row), column);
        }

        @Override
        public boolean isCellEditable(int row, int column) {
            return treeColumnModel.isCellEditable(getNode(row), column);
        }

        @Override
        public void fireTableStructureChanged() {
            super.fireTableStructureChanged();
            if (getRowHeight() <= 0)
                updateTableRowHeights();
        }

        @Override
        public void fireTableDataChanged() {
            super.fireTableDataChanged();
            if (getRowHeight() <= 0)
                updateTableRowHeights();
        }

        // TreeColumnModelListener interface

        @Override
        public void treeColumnChanged(TreeColumnModelEvent e) {
            TreePath path = e.getTreePath();
            if (rowSorter != null) {
                RowSorter<?> sorter = path.getParentPath() != null ? rowSorter.getRowSorter(path.getParentPath().getLastPathComponent()): null;
                if (sorter != null && path.getParentPath() != null) {
                    boolean isc = ignoreSortedChange;
                    ignoreSortedChange = true;
                    try {
                        Object parent = path.getParentPath().getLastPathComponent();
                        Object node = path.getLastPathComponent();
                        int modelRow = treeModel.getIndexOfChild(parent, node);
                        int viewRow = sorter.convertRowIndexToView(modelRow);
                        sorter.rowsUpdated(modelRow, modelRow);
                        if (sorter.convertRowIndexToView(modelRow) != viewRow) {
                            if (isExpanded(path)) {
                                updatePath(path, expandedDescendants(path));
                            } else {
                                fireTreeStructureChanged(path);
                            }
                            return;
                        }
                    } finally {
                        ignoreSortedChange = isc;
                    }
                }
            }

            int row = getRowForPath(path);
            if (row < 0)
                return;
            if (e.getColumn() == TreeColumnModelEvent.ALL_COLUMNS) {
                fireTableRowsUpdated(row, row);
            } else {
                fireTableCellUpdated(row, e.getColumn());
            }
            if (getRowHeight() <= 0) {
                invalidatePath(e.getTreePath());
                updateTableRowHeights(row, row+1);
            }
        }

        // TODO, maybe add these?
        //		public void treeColumnAdded(TreeColumnModelEvent e);
        //		public void treeColumnRemoved(TreeColumnModelEvent e);

        // TreeModelListener interface

        @Override
        public void treeNodesChanged(TreeModelEvent e) {
            TreePath path = e.getTreePath();
            int[] childIndices = e.getChildIndices();
            Object[] childNodes = e.getChildren();

            if (childNodes == null) {
                fireTreeNodesChanged(path, childIndices, childNodes);
                if (isRootVisible())
                    updateTable(0, 0, TableModelEvent.UPDATE);
            } else {
                processTreeNodesChanged(path, childIndices, childNodes);
                updateTable(path, getRows(path, childNodes), TableModelEvent.UPDATE);
            }
        }

        @Override
        public void treeNodesInserted(TreeModelEvent e) {
            TreePath path = e.getTreePath();
            int[] childIndices = e.getChildIndices();
            Object[] childNodes = e.getChildren();
            if (childIndices == null || childNodes == null)
                return;

            // update the tree view first
            processTreeNodesInserted(path, childIndices, childNodes);

            // then update the table view
            updateTable(path, getRows(path, childNodes), TableModelEvent.INSERT);
        }

        @Override
        public void treeNodesRemoved(TreeModelEvent e) {
            TreePath path = e.getTreePath();
            int[] childIndices = e.getChildIndices();
            Object[] childNodes = e.getChildren();
            if (childIndices == null || childNodes == null)
                return;

            // find the row indices in the tree
            int[] rows = getRows(path, childNodes);

            // update the tree view first
            processTreeNodesRemoved(path, childIndices, childNodes);

            // then update the table view
            updateTable(path, rows, TableModelEvent.DELETE);
        }

        private int[] getRows(TreePath parent, Object[] childNodes) {
            if (!isExpanded(parent))
                return null;
            // parent could be the invisible tree root
            if (parent.getPathCount() > 1 && getRowForPath(parent) < 0) {
                return null;
            }
            int[] rows = new int[childNodes.length];
            int len = 0;
            for (Object childNode : childNodes) {
                TreePath path = parent.pathByAddingChild(childNode);
                int r = getRowForPath(path);
                if (r >= 0)
                    rows[len++] = r;
            }
            if (len == 0)
                return null;
            if (len != rows.length)
                rows = Arrays.copyOf(rows, len);
            return rows;
        }

        @Override
        public void treeStructureChanged(TreeModelEvent e) {
            TreePath path = e.getTreePath();

            boolean rootChanged = false;
            if (path.getPathCount() == 1 && path.getLastPathComponent() != treeRoot) {
                rootChanged = true;
                treeRoot = path.getLastPathComponent();
            }

            // update the tree view
            if (rowSorter != null)
                rowSorter.structureChanged(path, rootChanged);
            fireTreeStructureChanged(path);

            // then update the table view
            if (rootChanged) {
                fireTableStructureChanged();
            } else if (isExpanded(path)) {
                fireTableDataChanged();
            }
        }

        private void processTreeNodesChanged(TreePath path, int[] childIndices, Object[] childNodes) {
            if (rowSorter != null) {
                RowSorter<?> sorter = rowSorter.getRowSorter(path.getLastPathComponent());
                if (sorter != null) {
                    boolean isc = ignoreSortedChange;
                    ignoreSortedChange = true;
                    try {
                        if (childIndices.length == 1) {
                            int row = childIndices[0];
                            int viewRow = sorter.convertRowIndexToView(row);
                            sorter.rowsUpdated(row, row);
                            childIndices[0] = sorter.convertRowIndexToView(row);
                            if (childIndices[0] == viewRow) {
                                if (viewRow >= 0)
                                    fireTreeNodesChanged(path, childIndices, childNodes);
                                return;
                            }
                        } else {
                            SorterHelper help = new SorterHelper(sorter, childIndices, childNodes);
                            int[] viewIndices = null;
                            Object[] viewNodes = null;
                            if (help.computeView()) {
                                viewIndices = help.viewIndices;
                                viewNodes = help.viewNodes;
                            }
                            sorter.rowsUpdated(childIndices[0], childIndices[childIndices.length-1]);
                            if (help.computeView()) {
                                if (Arrays.equals(viewIndices, help.viewIndices)
                                        && Arrays.equals(viewNodes, help.viewNodes)) {
                                    fireTreeNodesChanged(path, viewIndices, viewNodes);
                                    return;
                                }
                            } else if (viewIndices == null) {
                                return;
                            }
                        }
                    } finally {
                        ignoreSortedChange = isc;
                    }
                    updatePath(path, expandedDescendants(path));
                    return;
                }
            }
            fireTreeNodesChanged(path, childIndices, childNodes);
        }

        private void processTreeNodesInserted(TreePath path, int[] childIndices, Object[] childNodes) {
            if (rowSorter != null) {
                RowSorter<?> sorter = rowSorter.getRowSorter(path.getLastPathComponent());
                if (sorter != null) {
                    boolean isc = ignoreSortedChange;
                    ignoreSortedChange = true;
                    try {
                        if (childIndices.length == 1) {
                            int row = childIndices[0];
                            sorter.rowsInserted(row, row);
                            childIndices[0] = sorter.convertRowIndexToView(row);
                            if (childIndices[0] >= 0)
                                fireTreeNodesInserted(path, childIndices, childNodes);
                        } else {
                            SorterHelper help = new SorterHelper(sorter, childIndices, childNodes);
                            if (help.useAllChanged()) {
                                sorter.allRowsChanged();
                            } else {
                                sorter.rowsInserted(help.firstRow, help.lastRow);
                            }
                            if (help.computeView())
                                fireTreeNodesInserted(path, help.viewIndices, help.viewNodes);
                        }
                    } finally {
                        ignoreSortedChange = isc;
                    }
                    return;
                }
            }
            fireTreeNodesInserted(path, childIndices, childNodes);
        }


        private void processTreeNodesRemoved(TreePath path, int[] childIndices, Object[] childNodes) {
            if (rowSorter != null) {
                RowSorter<?> sorter = rowSorter.getRowSorter(path.getLastPathComponent());
                if (sorter != null) {
                    boolean isc = ignoreSortedChange;
                    ignoreSortedChange = true;
                    try {
                        if (childIndices.length == 1) {
                            int row = childIndices[0];
                            childIndices[0] = sorter.convertRowIndexToView(row);
                            if (childIndices[0] >= 0)
                                fireTreeNodesRemoved(path, childIndices, childNodes);
                            sorter.rowsDeleted(row, row);
                        } else {
                            SorterHelper help = new SorterHelper(sorter, childIndices, childNodes);
                            if (help.computeView())
                                fireTreeNodesRemoved(path, help.viewIndices, help.viewNodes);
                            if (help.useAllChanged()) {
                                sorter.allRowsChanged();
                            } else {
                                sorter.rowsDeleted(help.firstRow, help.lastRow);
                            }
                        }
                    } finally {
                        ignoreSortedChange = isc;
                    }
                    rowSorter.nodesRemoved(path, childNodes);
                    return;
                }
            }
            fireTreeNodesRemoved(path, childIndices, childNodes);
        }

        private void updateTable(TreePath parent, int[] rows, int eventType) {
            if (rows == null)
                return;
            int len = rows.length;
            if (len == 1) {
                updateTable(rows[0], rows[0], eventType);
            } else {
                Arrays.sort(rows, 0, len);
                int firstRow, lastRow;
                if (eventType == TableModelEvent.DELETE) {
                    firstRow = rows[len-1];
                    lastRow = rows[len-1];
                    for (int i=len-1; --i>=0;) {
                        int idx = rows[i];
                        if (idx == firstRow - 1) {
                            firstRow = idx;
                        } else {
                            updateTable(firstRow, lastRow, eventType);
                            firstRow = lastRow = idx;
                        }
                    }
                } else {
                    firstRow = rows[0];
                    lastRow = rows[0];
                    for (int i=1; i<len; i++) {
                        int idx = rows[i];
                        if (idx == lastRow + 1) {
                            lastRow = idx;
                        } else {
                            updateTable(firstRow, lastRow, eventType);
                            lastRow = firstRow = idx;
                        }
                    }
                }
                updateTable(firstRow, lastRow, eventType);
            }
        }

        private void updateTable(int firstRow, int lastRow, int eventType) {
            fireTableChanged(new TableModelEvent(this, firstRow,
                    lastRow, TableModelEvent.ALL_COLUMNS, eventType));
            if (eventType != TableModelEvent.DELETE && getRowHeight() <= 0) {
                // tree view has already updated the node bounds
//				invalidateRows(firstRow, lastRow);
                updateTableRowHeights(firstRow, lastRow+1);
            }
        }

        // TreeExpansionListener interface

        @Override
        public void treeExpanded(TreeExpansionEvent e) {
            fireTreeExpansionEvent(e.getPath(), true);
        }

        @Override
        public void treeCollapsed(TreeExpansionEvent e) {
            fireTreeExpansionEvent(e.getPath(), false);
        }

        // TreeWillExpandListener interface

        @Override
        public void treeWillExpand(TreeExpansionEvent e)
                throws ExpandVetoException {
            fireTreeWillExpandEvent(e.getPath(), true);
        }

        @Override
        public void treeWillCollapse(TreeExpansionEvent e)
                throws ExpandVetoException {
            fireTreeWillExpandEvent(e.getPath(), false);
        }

        // PropertyChangeListener interface

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String name = evt.getPropertyName();
            if (name.equals(JTree.LEAD_SELECTION_PATH_PROPERTY) ||
                    name.equals(JTree.ANCHOR_SELECTION_PATH_PROPERTY)) {
                int col = !isColumnFocusEnabled() ? -1 :
                    getColumnModel().getSelectionModel().getLeadSelectionIndex();
                repaint((TreePath)evt.getOldValue(), col);
                repaint((TreePath)evt.getNewValue(), col);
            } else if (name.equals("rowHeight")) {
                if (evt.getSource() != tree)
                    return; // only fire once
            } else if (name.equals("componentOrientation")
                    || name.equals("enabled")
                    || name.equals("rowSorter")
                    || name.equals("model")) {
                return; // don't fire
            }
            firePropertyChange(name, evt.getOldValue(), evt.getNewValue());
        }

        // ListSelectionModel interface

        @Override
        public void addListSelectionListener(ListSelectionListener l) {
            listenerList.add(ListSelectionListener.class, l);
        }

        @Override
        public void removeListSelectionListener(ListSelectionListener l) {
            listenerList.remove(ListSelectionListener.class, l);
        }

        @Override
        public void addSelectionInterval(int index0, int index1) {
            tree.addSelectionInterval(index0, index1);
        }

        @Override
        public void clearSelection() {
            getSelectionModel().clearSelection();
        }

        @Override
        public int getAnchorSelectionIndex() {
            return getRowForPath(getAnchorSelectionPath());
        }

        @Override
        public int getLeadSelectionIndex() {
            return getRowForPath(getLeadSelectionPath());
        }

        @Override
        public int getMaxSelectionIndex() {
            return getSelectionModel().getMaxSelectionRow();
        }

        @Override
        public int getMinSelectionIndex() {
            return getSelectionModel().getMinSelectionRow();
        }

        @Override
        public boolean getValueIsAdjusting() {
            return false;
        }

        @Override
        public void insertIndexInterval(int index, int length, boolean before) {}

        @Override
        public boolean isSelectedIndex(int index) {
            return getSelectionModel().isRowSelected(index);
        }

        @Override
        public boolean isSelectionEmpty() {
            return getSelectionModel().isSelectionEmpty();
        }

        @Override
        public void removeIndexInterval(int index0, int index1) {
        }

        @Override
        public void removeSelectionInterval(int index0, int index1) {
            TreePath anchor = getAnchorSelectionPath();
            tree.removeSelectionInterval(index0, index1);
            setAnchorSelectionPath(anchor);
            setLeadSelectionPath(getPathForRow(index1));
        }

        @Override
        public void setAnchorSelectionIndex(int index) {
            setAnchorSelectionPath(getPathForRow(index));
        }

        @Override
        public void setLeadSelectionIndex(int index) {
            setLeadSelectionPath(getPathForRow(index));
        }

        @Override
        public void setSelectionInterval(int index0, int index1) {
            TreeTable.this.setSelectionInterval(index0, index1);
            setAnchorSelectionPath(getPathForRow(index0));
            setLeadSelectionPath(getPathForRow(index1));
        }

        @Override
        public int getSelectionMode() {
            switch (getSelectionModel().getSelectionMode()) {
            case TreeSelectionModel.CONTIGUOUS_TREE_SELECTION:
                return ListSelectionModel.SINGLE_INTERVAL_SELECTION;
            case TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION:
                return ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;
            case TreeSelectionModel.SINGLE_TREE_SELECTION:
                return ListSelectionModel.SINGLE_SELECTION;
            }
            return ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;
        }

        @Override
        public void setSelectionMode(int mode) {
            switch (mode) {
            default: return;
            case ListSelectionModel.SINGLE_SELECTION:
                mode = TreeSelectionModel.SINGLE_TREE_SELECTION;
                break;
            case ListSelectionModel.SINGLE_INTERVAL_SELECTION:
                mode = TreeSelectionModel.CONTIGUOUS_TREE_SELECTION;
                break;
            case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:
                mode = TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION;
                break;
            }
            getSelectionModel().setSelectionMode(mode);
        }

        @Override
        public void setValueIsAdjusting(boolean valueIsAdjusting) {}

        // TreeSelectionListener interface

        @Override
        public void valueChanged(TreeSelectionEvent e) {
            int leadRow = getRowForPath(e.getNewLeadSelectionPath());
            int oldLeadRow = getRowForPath(e.getOldLeadSelectionPath());
            int minRow = Math.min(leadRow, oldLeadRow);
            int maxRow = Math.max(leadRow, oldLeadRow);
            TreePath[] paths = e.getPaths();
            if (paths != null) {
                for (TreePath path : paths) {
                    int row = getRowForPath(path);
                    minRow = Math.min(minRow, row);
                    maxRow = Math.max(maxRow, row);
                }
            }
            fireSelectionChanged(minRow, maxRow);
        }

        private void fireSelectionChanged(int firstRow, int lastRow) {
            ListSelectionEvent e = new ListSelectionEvent(this, firstRow, lastRow, false);
            Object[] listeners = listenerList.getListenerList();
            for (int i = listeners.length-2; i>=0; i-=2)
                if (listeners[i]==ListSelectionListener.class)
                    ((ListSelectionListener)listeners[i+1]).valueChanged(e);
        }

        // TreeModel Interface

        @Override
        public void addTreeModelListener(TreeModelListener l) {
            listenerList.add(TreeModelListener.class, l);
        }

        @Override
        public void removeTreeModelListener(TreeModelListener l) {
            listenerList.remove(TreeModelListener.class, l);
        }

        @Override
        public int getChildCount(Object parent) {
            if (rowSorter != null) {
                RowSorter<?> sorter = rowSorter.getRowSorter(parent);
                if (sorter != null)
                    return sorter.getViewRowCount();
            }
            return treeModel.getChildCount(parent);
        }

        @Override
        public Object getChild(Object parent, int index) {
            return treeModel.getChild(parent, convertIndexToModel(parent, index));
        }

        @Override
        public int getIndexOfChild(Object parent, Object child) {
            int index = treeModel.getIndexOfChild(parent, child);
            if (index < 0)
                return index;
            return convertIndexToView(parent, index);
        }

        @Override
        public Object getRoot() {
            return treeRoot;
        }

        @Override
        public boolean isLeaf(Object node) {
            return treeModel.isLeaf(node);
        }

        @Override
        public void valueForPathChanged(TreePath path, Object newValue) {
            treeModel.valueForPathChanged(path, newValue);
        }

        public int convertIndexToModel(Object parent, int index) {
            if (rowSorter != null) {
                RowSorter<?> sorter = rowSorter.getRowSorter(parent);
                if (sorter != null)
                    return sorter.convertRowIndexToModel(index);
            }
            return index;
        }

        public int convertIndexToView(Object parent, int index) {
            if (rowSorter != null) {
                RowSorter<?> sorter = rowSorter.getRowSorter(parent);
                if (sorter != null)
                    return sorter.convertRowIndexToView(index);
            }
            return index;
        }

        public void invalidateRows(int firstRow, int lastRow) {
            if (firstRow == lastRow) {
                invalidatePath(getPathForRow(firstRow));
            } else {
                HashMap<TreePath,ArrayList<Object>> map = new HashMap<TreePath,ArrayList<Object>>();
                int row = firstRow;
                TreePath path = getPathForRow(row);
                TreePath parentPath = path.getParentPath();
                if (parentPath == null) {
                    invalidatePaths(path, null, null);
                    path = getPathForRow(++row);
                    parentPath = path.getParentPath();
                }
                for (;;) {
                    ArrayList<Object> list = map.get(parentPath);
                    if (list == null) {
                        list = new ArrayList<Object>();
                        map.put(parentPath, list);
                    }
                    list.add(path.getLastPathComponent());
                    if (++row > lastRow)
                        break;
                    path = getPathForRow(row);
                    parentPath = path.getParentPath();
                }
                for (Map.Entry<TreePath,ArrayList<Object>> entry : map.entrySet()) {
                    Object parentNode = entry.getKey().getLastPathComponent();
                    Object[] childNodes = entry.getValue().toArray();
                    int[] childIndices = new int[childNodes.length];
                    for (int i=childNodes.length; --i>=0;)
                        childIndices[i] = getIndexOfChild(parentNode, childNodes[i]);
                    invalidatePaths(entry.getKey(), childIndices, childNodes);
                }
            }
        }

        void _invalidateRows(int firstRow, int lastRow) {
            for (int row=lastRow; row>=firstRow; row--) {
                invalidatePath(getPathForRow(row));
            }
        }

        public void invalidatePath(TreePath path) {
            int[] children;
            Object[] childNodes;
            if (path.getParentPath() == null) {
                children = null;
                childNodes = null;
            } else {
                Object node = path.getLastPathComponent();
                path = path.getParentPath();
                Object parentNode = path.getLastPathComponent();
                int index = getIndexOfChild(
                        parentNode, node);
                if (index < 0)
                    return;
                children = new int[] { index };
                childNodes = new Object[] { node };
            }
            invalidatePaths(path, children, childNodes);
        }

        public void invalidatePaths(TreePath path,
                int[] childIndices, Object[] children) {
            fireTreeNodesChanged(path, childIndices, children);
        }

        protected void fireTreeNodesChanged(TreePath path,
                int[] childIndices, Object[] childNodes) {
            AbstractTreeModel.fireNodesChanged(listenerList,
                    this, path, childIndices, childNodes);
        }

        protected void fireTreeNodesInserted(TreePath path,
                int[] childIndices, Object[] childNodes) {
            AbstractTreeModel.fireNodesInserted(listenerList,
                    this, path, childIndices, childNodes);
        }

        protected void fireTreeNodesRemoved(TreePath path,
                int[] childIndices, Object[] childNodes) {
            AbstractTreeModel.fireNodesRemoved(listenerList,
                    this, path, childIndices, childNodes);
        }

        protected void fireTreeStructureChanged(TreePath path) {
            AbstractTreeModel.fireTreeStructureChanged(listenerList, this, path);
        }

        // TreeTableSorterListener Interface

        public void sorterChanged(TreeTableSorterEvent e) {
//			String debugMessage = e.getType().name();
            switch (e.getType()) {
            case SORT_ORDER_CHANGED:
                JTableHeader header = getTableHeader();
                if (header != null)
                    header.repaint();
                break;
            case SORTED:
                TreePath root = new TreePath(treeRoot);
                updatePath(root, expandedDescendants(root));
                if (getRowHeight() <= 0)
                    updateTableRowHeights();
                break;
            case NODE_SORTED:
//				debugMessage = debugMessage + " " + e.getTreePath() + " " + ignoreSortedChange);
                nodeSorted(e.getTreePath());
                break;
            }
//			AquaUtils.logDebug(debugMessage);
        }

        private void nodeSorted(TreePath path) {
            if (!ignoreSortedChange) {
                RowSorter<?> sorter = rowSorter.getRowSorter(path.getLastPathComponent());
                SortKey key = sorter.getSortKeys().get(0);
                if (key.getSortOrder() == SortOrder.UNSORTED) {
                    sortedPaths.remove(path);
                } else {
                    sortedPaths.put(path, key);
                }
                updatePath(path, expandedDescendants(path));
                if (getExpandsSortedNodes() && !isExpanded(path))
                    expandPath(path);
            }
        }

        private List<TreePath> expandedDescendants(TreePath path) {
            Enumeration<TreePath> e = getExpandedDescendants(path);
            return e == null ? Collections.<TreePath>emptyList() : Collections.list(e);
        }

        void updatePath(TreePath path, List<TreePath> exp) {
            TreePath[] sel = getSelectionPaths();
            int row = getRowForPath(path);
            int rowCount = getRowCount();
            fireTreeStructureChanged(path);
            int newRowCount = getRowCount();
            if (newRowCount == rowCount) {
                int count = getChildCount(path.getLastPathComponent());
                if (count > 0) {
                    fireTableRowsUpdated(row+1, row+count);
                    if (getRowHeight() <= 0)
                        updateTableRowHeights(row+1, row+count+1);
                }
            } else {
                fireTableDataChanged();
                if (getRowHeight() <= 0)
                    updateTableRowHeights();
                for (TreePath p : exp)
                    expandPath(p);
            }
            setSelectionPaths(sel);
        }

        void updateSorter(TreePath path, boolean visible) {
            if (rowSorter != null) {
                List<TreePath> subPaths = visible ?
                        expandedDescendants(path) : Collections.<TreePath>emptyList();
                rowSorter.setVisible(path, subPaths, visible);
                if (visible)
                    updatePath(path, subPaths);
            }
        }

        RowSorter<?> getSorter(TreePath path) {
            return rowSorter == null ? null : rowSorter.getRowSorter(path);
        }

        // ToolTipMap Interface

        // JTable's default tool tip by renderer...
        @Override
        public String getToolTipText(TreeTable treeTable, MouseEvent event) {
            Point pt = event.getPoint();
            int row = treeTable.rowAtPoint(pt);
            int col = treeTable.columnAtPoint(pt);
            String tip = null;
            if (row >= 0 && col >= 0) {
                TreeTableCellRenderer r = treeTable.getCellRenderer(row, col);
                boolean sel = treeTable.isCellSelected(row, col);
                boolean foc = treeTable.isColumnFocusEnabled()
                        && col == treeTable.getLeadSelectionColumn()
                        && row == treeTable.getLeadSelectionRow()
                        && treeTable.isFocusOwner();
                Component c = r.getTreeTableCellRendererComponent(
                        treeTable, treeTable.getValueAt(row, col), sel, foc, row, col);
                if (c instanceof JComponent) {
                    Rectangle cell = treeTable.getCellRect(row, col, false);
                    pt.translate(-cell.x, -cell.y);
                    MouseEvent e = new MouseEvent(c, event.getID(),
                            event.getWhen(), event.getModifiers(),
                            pt.x, pt.y, event.getXOnScreen(), event.getYOnScreen(),
                            event.getClickCount(), event.isPopupTrigger(),
                            MouseEvent.NOBUTTON);
                    tip = ((JComponent)c).getToolTipText(e);
                }
            }
            if (tip == null)
                tip = treeTable.getToolTipText();
            return tip;
        }
    }

    public Map<TreePath,SortKey> getSortedPaths() {
        return Collections.unmodifiableMap(sortedPaths);
    }

    public boolean getExpandsSortedNodes() {
        return expandsSortedNodes;
    }

    public void setExpandesSortedNodes(boolean expandsSortedNodes) {
        boolean oldValue = getExpandsSortedNodes();
        this.expandsSortedNodes = expandsSortedNodes;
        firePropertyChange("expandsSortedNodes", oldValue, getExpandsSortedNodes());
    }

    private static class SorterHelper implements Comparator<SimpleEntry<Integer,Object>> {

        SorterHelper(RowSorter<?> s, int[] i, Object[] n) {
            sorter = s;
            childIndices = i;
            childNodes = n;
        }

        private final RowSorter<?> sorter;

        private final int[] childIndices;

        private final Object[] childNodes;

        int[] viewIndices;

        Object[] viewNodes;

        int firstRow;

        int lastRow;

        @Override
        public int compare(SimpleEntry<Integer, Object> a,
                SimpleEntry<Integer, Object> b) {
            return a.getKey() - b.getKey();
        }

        boolean useAllChanged() {
            int[] childIndices = this.childIndices;
            int firstRow = -1;
            int lastRow = -1;
            for (int i=childIndices.length; --i>=0;) {
                int idx = childIndices[i];
                if (firstRow < 0) {
                    firstRow = lastRow = idx;
                } else if (idx == firstRow - 1) {
                    firstRow = idx;
                } else {
                    return true;
                }
            }
            this.firstRow = firstRow;
            this.lastRow = lastRow;
            return false;
        }

        boolean computeView() {
            int[] viewIndices = new int[childIndices.length];
            Object[] viewNodes = new Object[childIndices.length];
            int viewLen = 0;
            for (int i=0; i<childIndices.length; i++) {
                int idx = childIndices[i];
                int view = sorter.convertRowIndexToView(idx);
                if (view >= 0) {
                    viewIndices[viewLen] = view;
                    viewNodes[viewLen++] = childNodes[i];
                }
            }
            if (viewLen == 0)
                return false;
            if (viewLen != viewIndices.length) {
                viewIndices = Arrays.copyOf(viewIndices, viewLen);
                viewNodes = Arrays.copyOf(viewNodes, viewLen);
            }
            SimpleEntry<Integer,Object>[] entries = new SimpleEntry[viewLen];
            for (int i=viewLen; --i>=0;)
                entries[i] = new SimpleEntry<Integer,Object>(viewIndices[i], viewNodes[i]);
            Arrays.sort(entries, this);
            for (int i=viewLen; --i>=0;) {
                viewIndices[i] = entries[i].getKey();
                viewNodes[i] = entries[i].getValue();
            }
            this.viewIndices = viewIndices;
            this.viewNodes = viewNodes;
            return true;
        }
    }

    // Row Header

    private static class RowHeaderAdapter extends AbstractTableModel
            implements TableModelListener, PropertyChangeListener {

        RowHeaderAdapter(TableModel m) {
            model = m;
            m.addTableModelListener(this);
        }

        private TableModel model;

        @Override
        public int getColumnCount() {
            return 1;
        }

        @Override
        public int getRowCount() {
            return model.getRowCount();
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            return rowIndex;
        }

        @Override
        public void tableChanged(TableModelEvent e) {
            fireTableChanged(new TableModelEvent(this,
                    e.getFirstRow(), e.getLastRow(), 0, e.getType()));
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            TreeTable.updateRowHeaderRenderer((JTable)evt.getSource());
        }
    }

    // Drag & Drop Support

    private boolean dragEnabled;

    private boolean dropModeSet = false;

    private transient DropLocation dropLocation;

    // TransferHandler's canImport may need to query
    // the DropLocation directly from the TreeTable
    // until it is possible to create proper DropLocations
    // for custom JComponents (hopefully 1.7).
    private transient DTDE dtde;

    public boolean getDragEnabled() {
        return dragEnabled;
    }

    public void setDragEnabled(boolean dragEnabled) {
        boolean oldValue = getDragEnabled();
        this.dragEnabled = dragEnabled;
        firePropertyChange("dragEnabled", oldValue, getDragEnabled());
        if (getDragEnabled())
            updateDropMode();
    }

    public DropMode getDropMode() {
        return table.getDropMode();
    }

    public void setDropMode(DropMode dropMode) {
        if (dropMode == null) {
            dropModeSet = false;
            updateDropMode();
        } else {
            switch (dropMode) {
            default:
                throw new IllegalArgumentException("Unsupported DropMode: " + dropMode);
            case ON:
            case INSERT:
            case INSERT_ROWS:
            case INSERT_COLS:
            case ON_OR_INSERT:
            case ON_OR_INSERT_ROWS:
            case ON_OR_INSERT_COLS:
            }
            dropModeSet = true;
            table.setDropMode(dropMode);
        }
    }

    private void updateDropMode() {
        if (dropModeSet)
            return;
        if (getRowSelectionAllowed()) {
            if (getColumnSelectionAllowed()) {
                table.setDropMode(DropMode.ON);
            } else {
                table.setDropMode(DropMode.INSERT_ROWS);
            }
        } else if (getColumnSelectionAllowed()) {
            table.setDropMode(DropMode.INSERT_COLS);
        } else {
            table.setDropMode(DropMode.ON);
        }
    }

    /**
     * Queries the current drop location for this tree table.
     * Currently, a transfer handler registered with this component
     * will not receive a `TreeTable.DropLocation`, but only a
     * `JTree.DropLocation`. This is a limitation of Swing. In order
     * to get the necessary information about the drop, the handler
     * can then call this method to get a richer description of the
     * drop.
     *
     * TODO: Another future possibility is to modify the `setTransferHandler`
     * method to wrap the argument in an enrichment handler which provides
     * the full drop location.
     *
     * @return  the current location, or `null` if there is no ongoing drop.
     */
    public DropLocation getDropLocation() {
        // dtde will be the DropTargetDragEvent that is
        // currently being processed by the TransferHandler.
        if (dtde != null)
            return createDropLocation(dtde.getLocation());
        return dropLocation;
    }

    public void setTransferHandler(TransferHandler th) {
        super.setTransferHandler(th);
        if (th != null)
            setDropTarget(new DT(getDropTarget()));
    }

    private void setDropLocation(Point pt) {
        DropLocation oldLocation = dropLocation;
        dropLocation = pt == null ? null : createDropLocation(pt);
        if (oldLocation == null || dropLocation == null) {
            if (oldLocation != dropLocation)
                repaint(oldLocation, dropLocation);
        } else if (oldLocation.equals(dropLocation)) {
            maybeTogglePath(oldLocation, pt);
        } else {
            // When a node expands or collapses, dropLocation
            // may change even though the drop point is the same.
            // The "when" must be preserved in that case to
            // prevent continuous expand/collapse of the node
            if (pt.equals(oldLocation.getDropPoint()))
                dropLocation.when = oldLocation.when;
            repaint(oldLocation, dropLocation);
        }
    }

    private void repaint(DropLocation a, DropLocation b) {
        int minRow = Integer.MAX_VALUE;
        int maxRow = -1;
        if (a != null) {
            minRow = Math.min(minRow, a.getRow()-1);
            maxRow = Math.max(maxRow, a.getRow());
        }
        if (b != null) {
            minRow = Math.min(minRow, b.getRow()-1);
            maxRow = Math.max(maxRow, b.getRow());
        }
        Rectangle r = getCellRect(Math.max(0, minRow), 0, true);
        if (maxRow >= getRowCount()) {
            r.height = getHeight()-r.y;
        } else {
            r.add(getCellRect(maxRow, 0, true));
        }
        r.x = 0;
        r.width = getWidth();
        repaint(r);
    }

    private void maybeTogglePath(DropLocation oldLocation, Point pt) {
        if (!pt.equals(oldLocation.getDropPoint())) {
            dropLocation.when = 0;
            return;
        }
        long when = oldLocation.when;
        if (when == 0) {
            TreePath path = getPathForLocation(pt.x, pt.y);
            if (path == null) {
                // schedule for expand or collapse if over tree handle
                dropLocation.when = isOverTreeHandle(pt) ?
                        System.currentTimeMillis() : -1;
            } else {
                // over path bounds, schedule for expand if expandable
                dropLocation.when = isLeaf(path) || isExpanded(path) ?
                        -1 : System.currentTimeMillis();
            }
        } else {
            if (when > 0 && System.currentTimeMillis() - when > 900) {
                TreePath path = getClosestPathForLocation(pt.x, pt.y);
                if (isExpanded(path)) {
                    collapsePath(path);
                } else {
                    expandPath(path);
                }
                dropLocation.when = -1;
            } else {
                dropLocation.when = when;
            }
        }
    }

    private boolean isOverTreeHandle(Point pt) {
        int col = columnAtPoint(pt);
        if (col >= 0 && col == getHierarchicalColumn()) {
            int row = rowAtPoint(pt);
            if (row >= 0) {
                return getDistanceToTreeHandle(
                        getPathForRow(row), pt.x) == 0;
            }
        }
        return false;
    }

    private DropLocation createDropLocation(Point pt) {
        TreePath path = null;
        int index = -1;
        int row = rowAtPoint(pt);
        int col = columnAtPoint(pt);
        boolean insertRow = false;
        boolean insertColumn = false;
        DropMode mode = getDropMode();

        switch (mode) {
        case INSERT:
            // determine if INSERT_ROWS or INSERT_COLS
            if (row < 0) {
                mode = DropMode.INSERT_ROWS;
            } else if (col < 0) {
                mode = DropMode.INSERT_COLS;
            } else {
                Rectangle cell = getCellRect(row, col, true);
                int dx = Math.min(pt.x - cell.x, cell.x+cell.width - pt.x);
                int dy = Math.min(pt.y - cell.y, cell.y+cell.height - pt.y);
                mode = dy < dx ? DropMode.INSERT_ROWS : DropMode.INSERT_COLS;
            }
            break;
        case ON_OR_INSERT:
            // determine if ON or INSERT_ROWS or INSERT_COLS
            if (row < 0) {
                mode = DropMode.INSERT_ROWS;
            } else if (col < 0) {
                mode = DropMode.INSERT_COLS;
            } else {
                Rectangle cell = getCellRect(row, col, true);
                int dx = Math.min(pt.x - cell.x, cell.x+cell.width - pt.x);
                int dy = Math.min(pt.y - cell.y, cell.y+cell.height - pt.y);
                if (dy < dx) {
                    mode = dy < cell.height/4 ? DropMode.INSERT_ROWS : DropMode.ON;
                } else {
                    mode = dx < cell.width/4 ? DropMode.INSERT_COLS : DropMode.ON;
                }
            }
            break;
        case ON_OR_INSERT_ROWS:
            // determine if ON or INSERT_ROWS
            if (row < 0) {
                mode = DropMode.INSERT_ROWS;
            } else {
                Rectangle cell = getCellRect(row, col, true);
                int dy = Math.min(pt.y - cell.y, cell.y+cell.height - pt.y);
                mode = dy < cell.height/4 ? DropMode.INSERT_ROWS : DropMode.ON;
            }
            break;
        case ON_OR_INSERT_COLS:
            // determin if ON or INSERT_COLS
            if (col < 0) {
                mode = DropMode.INSERT_COLS;
            } else {
                Rectangle cell = getCellRect(row, col, true);
                int dx = Math.min(pt.x - cell.x, cell.x+cell.width - pt.x);
                mode = dx < cell.width/4 ? DropMode.INSERT_COLS : DropMode.ON;
            }
            break;
        }

        switch (mode) {
        case ON:
            if (row < 0) {
                col = -1;
            } else if (col < 0) {
                row = -1;
            } else {
                path = getPathForRow(row);
            }
            break;
        case INSERT_ROWS:
            insertRow = true;
            if (row < 0) {
                row = getRowCount();
                if (row == 0) {
                    // no rows, use root
                    path = new TreePath(adapter.treeRoot);
                } else {
                    // set path to grandparent of the last row or the root if no grandparent
                    path = getPathForRow(row-1);
                    if (path.getPathCount() < 3) {
                        path = new TreePath(adapter.treeRoot);
                    } else {
                        path = path.getParentPath().getParentPath();
                    }
                }
                index = getChildCount(path);
            } else {
                TreePath rowPath = getPathForRow(row);
                path = rowPath.getParentPath();
                index = path == null ? 0 : adapter.getIndexOfChild(
                        path.getLastPathComponent(),
                        rowPath.getLastPathComponent());
                if (path == null) {
                    path = rowPath;
                    if (isRootVisible())
                        row = 1;
                } else {
                    Rectangle cell = getCellRect(row, col, true);
                    if (pt.y > cell.y + cell.height/2) {
                        if (isExpanded(rowPath)) {
                            path = rowPath;
                            index = 0;
                        } else {
                            index++;
                        }
                        row++;
                    }
                }
            }
            break;
        case INSERT_COLS:
            insertColumn = true;
            if (row < 0) {
                col = -1;
            } else {
                path = getPathForRow(row);
                if (col < 0) {
                    col = columnModel.getColumnCount();
                } else {
                    Rectangle cell = getCellRect(row, col, true);
                    if (pt.x > cell.x + cell.width/2)
                        col++;
                }
            }
            break;
        }
        return new DropLocation(pt, path, index,
                row, col, insertRow, insertColumn);
    }

    private class DTDE extends DropTargetDragEvent {

        DTDE(DropTargetDragEvent e) {
            super(e.getDropTargetContext(), e.getLocation(),
                    e.getDropAction(), e.getSourceActions());
            dtde = e;
        }

        private DropTargetDragEvent dtde;

        @Override
        public void acceptDrag(int dragOperation) {
            setDropLocation(dtde.getLocation());
            dtde.acceptDrag(dragOperation);
        }

        @Override
        public void rejectDrag() {
            setDropLocation(null);
            dtde.rejectDrag();
        }

    }

    private class DT extends DropTarget {

        DT(DropTarget dt) {
            super(dt.getComponent(), dt.getDefaultActions(), null);
            delegate = dt;
        }

        private DropTarget delegate;

        @Override
        public void dragEnter(DropTargetDragEvent e) {
            dtde = new DTDE(e);
            try {
                delegate.dragEnter(dtde);
            } finally {
                dtde = null;
            }
        }

        @Override
        public void dragExit(DropTargetEvent dte) {
            delegate.dragExit(dte);
            setDropLocation(null);
        }

        @Override
        public void dragOver(DropTargetDragEvent e) {
            dtde = new DTDE(e);
            try {
                delegate.dragOver(dtde);
            } finally {
                dtde = null;
            }
        }

        @Override
        public void drop(DropTargetDropEvent dtde) {
            delegate.drop(dtde);
            setDropLocation(null);
        }

        @Override
        public void dropActionChanged(DropTargetDragEvent dtde) {
            delegate.dropActionChanged(dtde);
        }

        @Override
        public void addDropTargetListener(DropTargetListener dtl)
                throws TooManyListenersException {
            delegate.addDropTargetListener(dtl);
        }

        @Override
        public void removeDropTargetListener(DropTargetListener dtl) {
            delegate.removeDropTargetListener(dtl);
        }
    }

    // Fusion of JTable.DropLocation & JTree.DropLocation
    public static final class DropLocation extends TransferHandler.DropLocation {

        DropLocation(Point pt, TreePath pth, int idx,
                int row, int col, boolean insertRow, boolean insertColumn) {
            super(pt);
            path            = pth;
            index           = idx;
            this.row        = row;
            column          = col;
            isInsertRow     = insertRow;
            isInsertColumn  = insertColumn;
        }

        private final TreePath path;

        private final int index;
        private final int row;
        private final int column;

        private final boolean isInsertRow;
        private final boolean isInsertColumn;

        long when = 0;

        public TreePath getPath() {
            return path;
        }

        /**
         * Returns the child index within the last branch of the path.
         *
         * @return  the index at which a drop occurs within the children of
         *          the branch denoted by `getPath`. For example, `0` means
         *          the drop happens before the first child, `1` means it
         *          happens after the first child. For `ON` drop mode, `-1`
         *          indicates that the drop occurs above the parent node.
         */
        public int getIndex() {
            return index;
        }

        public int getRow() {
            return row;
        }

        public int getColumn() {
            return column;
        }

        public boolean isInsertRow() {
            return isInsertRow;
        }

        public boolean isInsertColumn() {
            return isInsertColumn;
        }

        public boolean equals(DropLocation o) {
            return o != null
                && row              == o.row
                && column           == o.column
                && isInsertRow      == o.isInsertRow
                && isInsertColumn   == o.isInsertColumn
                && index            == o.index
                && (path == null ? o.path == null : path.equals(o.path));
        }
    }

    // adapted from List.getNextMath()
    public int getNextMatch(String prefix, int startingRow, int column, Position.Bias bias) {
        if (prefix == null)
            throw new IllegalArgumentException();
        if (column < 0 || column >= getColumnCount())
            throw new IllegalArgumentException();

        TableModel mdl = getTableModel();
        int max = getRowCount();
        if (startingRow < 0 || startingRow >= max)
            throw new IllegalArgumentException();

        int col = convertColumnIndexToModel(column);
        int increment = bias == Position.Bias.Forward ? 1 : -1;
        prefix = prefix.toUpperCase();
        // start search from the next element after the selected element
        int index = startingRow;
        do {
            String str = convertValueToText(mdl.getValueAt(index, col), column);
            if (!str.isEmpty()) {
                str = str.toUpperCase();
                if (str.startsWith(prefix))
                    return index;
            }
            index = (index + increment + max) % max;
        } while (index != startingRow);
        return -1;
    }

    public String convertValueToText(Object value, int column) {
        if (value == null)
            return "";
        String str = value.toString();
        return str == null ? "" : str;
    }

    public boolean isLeaf(TreePath path) {
        return getTreeModel().isLeaf(path.getLastPathComponent());
    }

    public int getChildCount(TreePath path) {
        return adapter.getChildCount(path.getLastPathComponent());
    }

    /**
     * Calculates the x distance from the tree handle. If the tree
     * handle isn't present, it is the distance from the start of the
     * path bounds.
     * <p>
     * A return value of 0 means the location is over the tree handle.
     * <p>
     * The return value will be negative for x locations that
     * fall in the leading region and positive for x locations
     * that fall in the trailing region.
     *
     * @return distance from the tree handle
     */
    public int getDistanceToTreeHandle(TreePath path, int x) {
        return getUI().getDistanceToTreeHandle(this, path, x);
    }

    public int getAlignment(Component rendererComponent, int row, int column) {
        if (rendererComponent instanceof JLabel) {
            return getAlignment(((JLabel)rendererComponent).getHorizontalAlignment());
        } else if (rendererComponent instanceof AbstractButton) {
            return getAlignment(((AbstractButton)rendererComponent).getHorizontalAlignment());
        }
        return SwingConstants.CENTER;
    }

    private int getAlignment(int align) {
        switch (align) {
        case SwingConstants.LEADING:
            return getComponentOrientation().isLeftToRight() ?
                SwingConstants.LEFT : SwingConstants.RIGHT;
        case SwingConstants.TRAILING:
            return getComponentOrientation().isLeftToRight() ?
                SwingConstants.RIGHT : SwingConstants.LEFT;
        }
        return align;
    }
}
