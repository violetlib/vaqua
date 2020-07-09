/*
 * Changes copyright (c) 2014-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * @(#)JBrowser.java
 *
 * Copyright (c) 2003-2013 Werner Randelshofer, Switzerland.
 * http://www.randelshofer.ch
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 *
 * Original code is copyright (c) 2003 Steve Roy.
 * Personal homepage: <http://homepage.mac.com/sroy>
 * Projects homepage: <http://www.roydesign.net>
 * http://www.roydesign.net/aquadialogs/
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.dnd.DropTarget;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.*;
import javax.accessibility.Accessible;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import javax.swing.plaf.ListUI;
import javax.swing.plaf.UIResource;
import javax.swing.tree.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.*;

/**
 * JBrowser provides a user interface for displaying and selecting items from a list of data or from hierarchically
 * organized lists of data such as directory paths. When working with a hierarchy of data, the levels are displayed in
 * columns, which are numbered from left to right.
 * <p>
 * JBrowser is a clean-room implementation of an Aqua column view (NSBrowser), which is used by file dialogs and the
 * finder of macOS.
 * <p>
 * <b>Usage</b>
 * <p>
 * In general a JBrowser can be used whenever a JTree is suitable. JBrowsers uses a TreeModel like a JTree.
 * <p>
 * If you are using JBrowser without the Quaqua Look and Feel, you may notice that it does not fill the viewport with
 * empty columns. To work around this, you may want to use JBrowser together with the helper class JBrowserViewport.
 *
 * <b>Known bugs</b>
 * <ul>
 * <li><b>XXX</b> - Instances of this class are not serializable.</li>
 * <li><b>FIXME</b> - TreeSelectionModel.CONTIGUOUS_TREE_SELECTION does not work
 * (I need to fix method addSelectionInterval in the inner ListSelectionModel
 * class).</li>
 * <li><b>FIXME</b> - treeStructureChange events may result in an inconsistent
 * state of the JBrowser when selected path(s) are removed.</li>
 * <li><b>FIXME</b> - There is currently no functionality provided to make nodes
 * editable by the user.</li>
 * </ul>
 *
 * @author Werner Randelshofer
 */
public class JBrowser extends JComponent implements Scrollable {

    /**
     * @see #getUIClassID
     */
    private static final String uiClassID = "BrowserUI";

    /**
     * The currently expanded path.
     * <p><b>Note:</b> This path does not include the additional column being shown when this path points to a non-leaf
     * node.
     */
    private TreePath expandedPath;

    /**
     * Whether the last path component of the expanded path is a leaf.
     */
    private boolean expandedPathIsLeaf;

    /**
     * The fixed width of the list column cells.
     */
    private int fixedCellWidth = 175;

    /**
     * The minimum column width.
     */
    private int columnMinimumWidth = 150;

    /**
     * The minimum column height.
     */
    private int columnMinimumHeight = 100;

    /**
     * The model that defines the tree displayed by this object.
     */
    private TreeModel treeModel;

    /**
     * Models the set of selected nodes in this tree.
     */
    protected transient TreeSelectionModel selectionModel;

    /**
     * Updates the selection models of the columns when the selection mode
     * of the {@code selectionModel} changes.
     */
    private transient SelectionModeUpdater selectionModeUpdater = new SelectionModeUpdater();

    /**
     * Expands the columns to match the current selection in the
     * {@code selectionModel}.
     * Creates a new event and passes it off the {@code selectionListeners}.
     */
    private transient TreeSelectionUpdater treeSelectionUpdater = new TreeSelectionUpdater();

    /**
     * Handles changes in the tree root.
     * We need this special handler, because we do not display the root node
     * using one of our columns and would thus miss changes on the root node.
     */
    private TreeRootHandler treeRootHandler = new TreeRootHandler();

    /**
     * Changes the selection when mouse events occur on the columns.
     */
    private transient ColumnMouseListener columnMouseListener = new ColumnMouseListener();

    /**
     * Moves the focus to another column when key events occur on the columns.
     * <br>
     * FIXME - this listener works only, if it receives listener events
     * <b>after</b> the KeyListener of the ListUI of a column has
     * processed its events.
     */
    private transient ColumnKeyListener columnKeyListener = new ColumnKeyListener();

    /**
     * The cell used to draw nodes. If {@code null}, the UI uses a default
     * {@code cellRenderer}.
     * <p>
     * <b>FIXME</b> - the default cell renderer is provided by ListUI. We should
     * have a BrowserUI which provides a better default cell renderer (one which
     * shows an icon when a node is not a leaf).
     */
    private ListCellRenderer cellRenderer;

    /**
     * This column is only non-null, when we have a preview renderer. The previewColumn contains a SizeConstrainedPanel
     * as its child. The SizeConstrainedPanel contains the component returned by the preview renderer. The preferred
     * width of the SizeConstrainedPanel is set to the value of variable fixedCellWidth.
     */
    private PreviewColumn previewColumn;

    //
    // Bound property names
    //
    /** Bound property name for {@code cellRenderer}. */
    public final static String CELL_RENDERER_PROPERTY = "cellRenderer";
    /** Bound property name for {@code cellRenderer}. */
    public final static String PREVIEW_RENDERER_PROPERTY = "previewRenderer";
    /** Bound property name for {@code treeModel}. */
    public final static String TREE_MODEL_PROPERTY = "model";
    /** Bound property name for selectionModel. */
    public final static String SELECTION_MODEL_PROPERTY = "selectionModel";
    /** Bound property name for preferredCellWidth. */
    public final static String FIXED_CELL_WIDTH_PROPERTY = "fixedCellWidth";
    /** Bound property name for columnMinimumWidth. */
    public final static String COLUMN_MINIMUM_WIDTH = "columnMinimumWidth";
    /** Bound property name for columnMinimumHeight. */
    public final static String COLUMN_MINIMUM_HEIGHT = "columnMinimumHeight";
    /** Bound property name for columnsResizable. */
    public final static String COLUMNS_RESIZABLE_PROPERTY = "columnsResizable";

    private Object prototypeCellValue;
    /**
     * If this is set to true, JBrowser shows tooltips with the cell value
     * for cells which don't fit into the current width of a column.
     */
    private boolean isShowCellTips;
    /**
     * Origin of the cell tip relative to the origin of the cell renderer.
     */
    private Point cellTipOrigin = new Point(0, 1);
    /**
     * Holds the value of the dragEnabled property.
     */
    private boolean dragEnabled;
    /**
     * Should columns should be resizable?
     **/
    private boolean columnsResizable = true;
    /**
     * Should the preview column be filled?
     **/
    private boolean shouldFillPreviewColumn = false;
    /**
     * Should the preview column be filled?
     **/
    private boolean isPreviewColumnScrollable = false;
    /** This border is used when the Look and Feel does not specify a
     * "List.cellNoFocusBorder".
     */
    private static final Border DEFAULT_NO_FOCUS_BORDER = new EmptyBorder(1, 1, 1, 1);

    /**
     * Creates a {@code JBrowser} with a sample model.
     * The default model used by the browser defines a leaf node as any node without children.
     *
     * @see DefaultTreeModel#asksAllowsChildren
     */
    public JBrowser() {
        this(getDefaultTreeModel());
    }

    /**
     * Creates a {@code JBrowser} with each element of the specified array as the child of a new root node which is not
     * displayed. By default, the browser defines a leaf node as any node without children.
     *
     * @param value an array of {@code Object}s
     * @see DefaultTreeModel#asksAllowsChildren
     */
    public JBrowser(Object[] value) {
        this(createTreeModel(value));
        expandRoot();
    }

    /**
     * Creates a {@code JBrowser} with each element of the specified {@code Vector} as the child of a new root node
     * which is not displayed. By default, the tree defines a leaf node as any node without children.
     *
     * @param value a {@code Vector}
     * @see DefaultTreeModel#asksAllowsChildren
     */
    public JBrowser(Vector value) {
        this(createTreeModel(value));
        expandRoot();
    }

    /**
     * Creates a {@code JBrowser} created from a {@code Hashtable} which does not display with root. Each value-half of
     * the key/value pairs in the {@code HashTable} becomes a child of the new root node. By default, the tree defines a
     * leaf node as any node without children.
     *
     * @param value a {@code Hashtable}
     * @see DefaultTreeModel#asksAllowsChildren
     */
    public JBrowser(Hashtable value) {
        this(createTreeModel(value));
        expandRoot();
    }

    /**
     * Creates a {@code JBrowser} with the specified {@code TreeNode} as its root, which does not display the root node.
     * By default, the tree defines a leaf node as any node without children.
     *
     * @param root a {@code TreeNode} object
     * @see DefaultTreeModel#asksAllowsChildren
     */
    public JBrowser(TreeNode root) {
        this(root, false);
    }

    /**
     * Creates a {@code JBrowser} with the specified {@code TreeNode} as its root, which does not display the root node
     * and which decides whether a node is a leaf node in the specified manner.
     *
     * @param root a {@code TreeNode} object
     * @param asksAllowsChildren if false, any node without children is a leaf node; if true, only nodes that do not
     *              allow children are leaf nodes
     * @see DefaultTreeModel#asksAllowsChildren
     */
    public JBrowser(TreeNode root, boolean asksAllowsChildren) {
        this(new DefaultTreeModel(root, asksAllowsChildren));
    }

    /**
     * Creates an instance of {@code JBrowser} which does not display the root node -- the tree is created using the
     * specified data model.
     *
     * @param newModel the {@code TreeModel} to use as the data model
     */
    public JBrowser(TreeModel newModel) {
        super();

        initComponents();

        selectionModel = new DefaultTreeSelectionModel();
        selectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        selectionModel.addPropertyChangeListener(selectionModeUpdater);
        selectionModel.addTreeSelectionListener(treeSelectionUpdater);

        //cellRenderer = new DefaultColumnCellRenderer(this);
        setOpaque(true);
        // setFocusCycleRoot(true);
        updateUI();
        setModel(newModel);
        setFocusable(false);
    }

    /**
     * Returns the look and feel (L&amp;F) object that renders this component.
     *
     * @return the {@code ListUI} object that renders this component
     */
    public BrowserUI getUI() {
        return (BrowserUI) ui;
    }

    /**
     * Sets the look and feel (L&amp;F) object that renders this component.
     *
     * @param ui the {@code BrowserUI} L&amp;F object
     * @see UIDefaults#getUI
     */
    public void setUI(BrowserUI ui) {
        super.setUI(ui);
    }

    /**
     * Resets the UI property with the value from the current look and feel.
     *
     * @see UIManager#getUI
     */
    @Override
    public void updateUI() {
        // Try to get a browser UI from the UIManager.
        // Fall back to BasicBrowserUI, if none is available.
        if (UIManager.get(getUIClassID()) != null) {
            setUI((BrowserUI) UIManager.getUI(this));
        } else {
            setUI(new BasicBrowserUI());
        }
        invalidate();
    }

    /**
     * Returns the suffix used to construct the name of the look and feel (L&amp;F) class used to render this component.
     *
     * @return the string "BrowserUI"
     * @see JComponent#getUIClassID
     * @see UIDefaults#getUI
     */
    @Override
    public String getUIClassID() {
        return uiClassID;
    }

    public void setPrototypeCellValue(Object prototypeCellValue) {
        this.prototypeCellValue = prototypeCellValue;
    }

    /**
     * Sets the {@code dragEnabled} property, which must be {@code true} to enable automatic drag handling (the first
     * part of drag and drop) on this component. The {@code transferHandler} property needs to be set to a non-{@code
     * null} value for the drag to do anything. The default value of the {@code dragEnabled} property is {@code false}.
     *
     * <p>
     *
     * When automatic drag handling is enabled, most look and feels begin a drag-and-drop operation whenever the user
     * presses the mouse button over a selection and then moves the mouse a few pixels. Setting this property to {@code
     * true} can therefore have a subtle effect on how selections behave.
     *
     * <p>
     *
     * Some look and feels might not support automatic drag and drop; they will ignore this property. You can work
     * around such look and feels by modifying the component to directly call the {@code exportAsDrag} method of a
     * {@code TransferHandler}.
     *
     * @param b the value to set the {@code dragEnabled} property to
     * @exception HeadlessException if
     *            {@code b} is {@code true} and
     *            {@code GraphicsEnvironment.isHeadless()}
     *            returns {@code true}
     * @see java.awt.GraphicsEnvironment#isHeadless
     * @see #getDragEnabled
     * @see #setTransferHandler
     * @see TransferHandler
     */
    public void setDragEnabled(boolean b) {
        if (b && GraphicsEnvironment.isHeadless()) {
            throw new HeadlessException();
        }
        dragEnabled = b;

        for (int i = 0, n = getListColumnCount(); i < n; i++) {
            getColumnList(i).setDragEnabled(b);
        }
    }

    @Override
    public void setTransferHandler(TransferHandler newValue) {
        super.setTransferHandler(newValue);
        for (int i = 0, n = getListColumnCount(); i < n; i++) {
            getColumnList(i).setTransferHandler(newValue);
        }
    }

    /**
     * Gets the {@code dragEnabled} property.
     *
     * @return the value of the {@code dragEnabled} property
     * @see #setDragEnabled
     */
    public boolean getDragEnabled() {
        return dragEnabled;
    }

    /**
     * Creates and returns a sample {@code TreeModel}.
     * Used primarily for beanbuilders to show something interesting.
     *
     * @return the default {@code TreeModel}
     */
    protected static TreeModel getDefaultTreeModel() {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("JBrowser");
        DefaultMutableTreeNode parent;

        parent = new DefaultMutableTreeNode("colors");
        root.add(parent);
        parent.add(new DefaultMutableTreeNode("blue"));
        parent.add(new DefaultMutableTreeNode("violet"));
        parent.add(new DefaultMutableTreeNode("red"));
        parent.add(new DefaultMutableTreeNode("yellow"));

        parent = new DefaultMutableTreeNode("sports");
        root.add(parent);
        parent.add(new DefaultMutableTreeNode("basketball"));
        parent.add(new DefaultMutableTreeNode("soccer"));
        parent.add(new DefaultMutableTreeNode("football"));
        parent.add(new DefaultMutableTreeNode("hockey"));

        parent = new DefaultMutableTreeNode("food");
        root.add(parent);
        parent.add(new DefaultMutableTreeNode("hot dogs"));
        parent.add(new DefaultMutableTreeNode("pizza"));
        parent.add(new DefaultMutableTreeNode("ravioli"));
        parent.add(new DefaultMutableTreeNode("bananas"));
        return new DefaultTreeModel(root);
    }

    /**
     * Returns a {@code TreeModel} wrapping the specified object.
     * If the object is:<ul>
     * <li>an array of {@code Object}s,
     * <li>a {@code Hashtable}, or
     * <li>a {@code Vector}
     * </ul>then a new root node is created with each of the incoming
     * objects as children. Otherwise, a new root is created with the
     * specified object as its value.
     *
     * @param value the {@code Object} used as the foundation for the {@code TreeModel}
     * @return a {@code TreeModel} wrapping the specified object
     */
    protected static TreeModel createTreeModel(Object value) {
        DefaultMutableTreeNode root;

        if ((value instanceof Object[]) || (value instanceof Hashtable)
                || (value instanceof Vector)) {
            root = new DefaultMutableTreeNode("root");
            JTree.DynamicUtilTreeNode.createChildren(root, value);
        } else {
            root = new JTree.DynamicUtilTreeNode("root", value);
        }
        return new DefaultTreeModel(root, false);
    }

    /**
     * Expands the root path, assuming the current TreeModel has been set.
     */
    private void expandRoot() {
        selectionModel.clearSelection();
        expandPath(new TreePath(treeModel.getRoot()));
    }

    /**
     * Returns the path to the node that is closest to x,y. If no nodes are currently viewable, or there is no model,
     * returns {@code null}, otherwise it always returns a valid path. To test if the node is exactly at x, y, get the
     * node's bounds and test x, y against that.
     *
     * @param x an integer giving the number of pixels horizontally from the left edge of the display area, minus any
     *          left margin
     * @param y an integer giving the number of pixels vertically from the top of the display area, minus any top margin
     * @return  the {@code TreePath} for the node closest to that location,
     *
     * @see #getPathForLocation
     * @see #getPathBounds
     */
    public @Nullable TreePath getClosestPathForLocation(int x, int y) {
        Component c = getComponentAt(x, y);
        while (c != null) {
            x -= c.getX();
            y -= c.getY();
            if (c instanceof JList) {
                JList l = (JList) c;
                JBrowser.ColumnListModel m = (ColumnListModel) l.getModel();
                int index = l.locationToIndex(new Point(x, y));
                if (index != -1) {
                    return m.path.pathByAddingChild(m.getElementAt(index));
                } else {
                    return null;
                }
            }
            c = c.getComponentAt(x, y);
        }
        return null;
    }

    /**
     * Returns the {@code Rectangle} that the specified node will be drawn into. Returns {@code null} if any component
     * in the path is hidden (under a collapsed parent).
     * <p>
     * Note:<br>
     * This method returns a valid rectangle, even if the specified node is not currently displayed.
     *
     * @param path the {@code TreePath} identifying the node
     * @return the {@code Rectangle} the node is drawn in, or {@code null}
     */
    public @Nullable Rectangle getPathBounds(TreePath path) {
        if (path.getPathCount() <= getListColumnCount()) {
            JList list = getColumnList(path.getPathCount() - 1);
            int index;
            if (path.getPathCount() > 1) {
                index = treeModel.getIndexOfChild(path.getPathComponent(path.getPathCount() - 2), path.getLastPathComponent());
            } else {
                index = treeModel.getIndexOfChild(null, path.getLastPathComponent());
            }

            // A column list is presented inside of a JViewport and a JPanel in the JBrowser.
            Rectangle bounds = list.getCellBounds(index, index);
            bounds.x += list.getLocation().x + list.getParent().getLocation().x + list.getParent().getParent().getLocation().x;
            bounds.y += list.getLocation().y + list.getParent().getLocation().y + list.getParent().getParent().getLocation().y;

            return bounds;
        } else {
            return null;
        }
    }

    /**
     * Set this to true, if you want to JBrowser to display a tooltip for the cell over which the mouse is hovering. The
     * tooltip is shown only for cells which are wider than the current width of the browser column.
     * @param newValue the value
     */
    public void setShowCellTips(boolean newValue) {
        boolean oldValue = isShowCellTips;
        if (oldValue != newValue) {
            isShowCellTips = newValue;
            String tipText = (newValue) ? "cell tip" : null;
            for (int i = 0, n = getListColumnCount(); i < n; i++) {
                getColumnList(i).setToolTipText(tipText);
            }
            firePropertyChange("showCellTips", oldValue, newValue);
        }
    }

    /**
     * Returns true if the JBrowser shows cell tips for list cells that don't fit into a column.
     * @return the value
     */
    public boolean isShowCellTips() {
        return isShowCellTips;
    }

    /**
     * Sets the origin of the cell tip tooltip relative to the origin of the cell renderer.
     * @param newValue the value
     */
    public void setShowCellTipOrigin(Point newValue) {
        Point oldValue = cellTipOrigin;
        cellTipOrigin = (newValue == null) ? new Point(0, 0) : (Point) newValue.clone();
        firePropertyChange("cellTipOrigin", oldValue, newValue);
    }

    /**
     * Returns the origin of the cell tip tooltip relative to the origin of the cell renderer.
     * @return the value
     */
    public Point getCellTipOrigin() {
        return (Point) cellTipOrigin.clone();
    }

    /**
     * Returns true if the item identified by the path is currently selected.
     *
     * @param path a {@code TreePath} identifying a node
     * @return true if the node is selected
     */
    public boolean isPathSelected(TreePath path) {
        TreePath[] selectionPaths = getSelectionPaths();
        for (int i = 0; i < selectionPaths.length; i++) {
            if (selectionPaths[i].equals(path)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Adds the node identified by the specified {@code TreePath} to the current selection. If any component of the path
     * isn't viewable, and {@code getExpandsSelectedPaths} is true it is made viewable.
     * <p>
     * Note that {@code JBrowser} does not allow duplicate nodes to exist as children under the same parent -- each
     * sibling must be a unique object.
     *
     * @param path the {@code TreePath} to add
     */
    public void addSelectionPath(TreePath path) {
        for (int i = 0; i < path.getPathCount(); i++) {
            JList columnList = getColumnList(i);
            int index = treeModel.getIndexOfChild((i == 0) ? null : path.getPathComponent(i - 1), path.getPathComponent(i));
            if (!columnList.isSelectedIndex(index)) {
                if (i < path.getPathCount() - 1) {
                    columnList.setSelectionInterval(index, index);
                } else {
                    columnList.addSelectionInterval(index, index);
                }
            }
        }
    }

    /**
     * Ensures that the last node of the specified path is visible.
     * <br>
     * Nothing happens, if it is impossible to make the node visible without changing the selection of the JBrowser.
     *
     * @param path the {@code TreePath} specifying the node to make visible.
     */
    public void ensurePathIsVisible(TreePath path) {
        TreePath selectionPath = selectionModel.getSelectionPath();
        if (selectionPath != null && (path.isDescendant(selectionPath) || selectionPath.isDescendant(path))) {
            if (!isValid()) {
                setSize(getPreferredSize());
                doLayout();
            }

            for (int i = 0; i < path.getPathCount() - 1 && i < getListColumnCount(); i++) {
                JList columnList = getColumnList(i);
                int index0 = findListElement(columnList, path.getPathComponent(i+1));

                Rectangle bounds = columnList.getCellBounds(index0, index0);

                if (index0 != -1 && bounds != null) {
                    // Enlarge the bounds in case a horizontal scroll bar appears after we do this
                    bounds.height += 20;
                    columnList.scrollRectToVisible(bounds);
                }
            }

            Component component = getComponent(getColumnCount() - 1);

            if (component != null && component.getBounds() != null) {
                scrollRectToVisible(component.getBounds());
            }
        }
    }

    private int findListElement(@NotNull JList list, Object value) {
        ListModel model = list.getModel();
        int count = model.getSize();
        for (int i = 0; i < count; i++) {
            if (model.getElementAt(i).equals(value)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Selects the node identified by the specified path.
     *
     * @param path the {@code TreePath} specifying the node to select
     */
    public void setSelectionPath(TreePath path) {
        selectionModel.setSelectionPath(path);
    }

    /**
     * Removes the node identified by the specified path from the current selection.
     *
     * @param path  the {@code TreePath} identifying a node
     */
    public void removeSelectionPath(TreePath path) {
        if (path.getPathCount() <= getListColumnCount()) {
            JList columnList = getColumnList(path.getPathCount() - 1);
            int index = treeModel.getIndexOfChild(path.getPathComponent(path.getPathCount() - 2), path.getLastPathComponent());
            columnList.removeSelectionInterval(index, index);
        }
    }

    /**
     * Returns the path for the node at the specified location.
     *
     * @param x an integer giving the number of pixels horizontally from the left edge of the display area, minus any
     *          left margin
     * @param y an integer giving the number of pixels vertically from the top of the display area, minus any top margin
     * @return  the {@code TreePath} for the node at that location
     */
    public TreePath getPathForLocation(int x, int y) {
        // XXX - Check if closest path intersects x,y.
        return getClosestPathForLocation(x, y);
        /*
        TreePath closestPath = getClosestPathForLocation(x, y);
        if (closestPath != null) {
        Rectangle pathBounds = getPathBounds(closestPath);
        if(pathBounds != null &&
        x >= pathBounds.x && x < (pathBounds.x + pathBounds.width) &&
        y >= pathBounds.y && y < (pathBounds.y + pathBounds.height))
        return closestPath;
        }
        return null;
         */
    }

    /**
     * Clears the selection.
     * This collapses all columns.
     */
    public void clearSelection() {
        setSelectionPath(new TreePath(treeModel.getRoot()));
    }

    /**
     * Sets the width of every cell in the browser. If {@code width} is -1, cell widths are computed by applying {@code
     * getPreferredSize} to the {@code cellRenderer} component for each tree node.
     * <p>
     * The default value of this property is 175.
     * <p>
     * This is a JavaBeans bound property.
     *
     * @param width the width, in pixels, for all cells in this list
     * @see #setColumnMinimumWidth
     * @see JComponent#addPropertyChangeListener
     */
    public void setFixedCellWidth(int width) {
        int oldValue = fixedCellWidth;
        fixedCellWidth = width;
        for (int i = 0; i < getListColumnCount(); i++) {
            getColumnList(i).setFixedCellWidth(width);
        }
        revalidate();
        repaint();
        firePropertyChange(FIXED_CELL_WIDTH_PROPERTY, oldValue, fixedCellWidth);
    }

    public int getFixedCellWidth() {
        return fixedCellWidth;
    }

    /**
     * Sets the minimum width of browser columns, which limits the ability of the user to resize them.
     *
     * @param w The width.
     */
    public void setColumnMinimumWidth(int w) {
        if (w != columnMinimumWidth) {
            int oldValue = columnMinimumWidth;
            columnMinimumWidth = w;
            revalidate();
            repaint();
            firePropertyChange(COLUMN_MINIMUM_WIDTH, oldValue, w);
        }
    }

    public int getColumnMinimumWidth() {
        return columnMinimumWidth;
    }

    /**
     * Set the minimum height of browser columns, which limits the ability of the user to resize them.
     * @param h The height.
     */
    public void setColumnMinimumHeight(int h) {
        if (h != columnMinimumHeight) {
            int oldValue = columnMinimumHeight;
            columnMinimumHeight = h;
            revalidate();
            repaint();
            firePropertyChange(COLUMN_MINIMUM_HEIGHT, oldValue, h);
        }
    }

    public int getColumnMinimumHeight() {
        return columnMinimumHeight;
    }

    /**
     * Sets the width of a column.
     *
     * @param column Index of the column.
     * @param width The width.
     */
    public void setColumnWidth(int column, int width) {
        ListColumn c = getListColumn(column);
        if (c != null) {
            c.setFixedCellWidth(width);
        }
    }

    /**
     * Gets the width of a column.
     *
     * @param column Index of the column.
     * @return the value
     */
    public int getColumnWidth(int column) {
        ListColumn c = getListColumn(column);
        if (c != null) {
            return c.getFixedCellWidth();
        }
        return 0;
    }

    /**
     * Gets the preferred width of a column.
     *
     * @param column Index of the column.
     * @return the value
     */
    public int getColumnPreferredWidth(int column) {
        ListColumn c = getListColumn(column);
        if (c != null) {
            return c.getColumnPreferredWidth();
        }
        return 0;
    }

    /**
     * Set the preferred width of the preview column.
     *
     * @param width The width.
     **/
    public void setPreviewColumnWidth(int width) {
        if (previewColumn != null) {
            previewColumn.setDesignatedWidth(width);
        }
    }

    /**
     * Gets the width of the preview column.
     * @return the value
     */
    public int getPreviewColumnWidth() {
        if (previewColumn == null) {
            return getFixedCellWidth();
        } else {
            return previewColumn.getPreferredSize().width;
        }
    }

    /**
     * Gets whether the preview column should expand to fill available horizontal space.
     **/
    public boolean isPreviewColumnFilled() {
        return shouldFillPreviewColumn;
    }

    /**
     * Sets whether the preview column should expand to fill available horizontal space.
     *
     * @param newValue
     *            The new value.
     **/
    public void setPreviewColumnFilled(boolean newValue) {
        boolean oldValue = shouldFillPreviewColumn;
        shouldFillPreviewColumn = newValue;
        if (oldValue != newValue) {
            revalidate();
            repaint();
        }
    }

    /**
     * Gets whether the preview column should be scrollable.
     **/
    public boolean isPreviewColumnScrollable() {
        return isPreviewColumnScrollable;
    }

    /**
     * Sets whether the preview column should be scrollable.
     *
     * @param newValue
     *            The new value.
     **/
    public void setPreviewColumnScrollable(boolean newValue) {
        boolean oldValue = isPreviewColumnScrollable;
        isPreviewColumnScrollable = newValue;
        if (oldValue != newValue) {
            revalidate();
            repaint();
        }
    }

    /**
     * Sets whether columns should be resizable.
     *
     * @param newValue
     *            The new value.
     **/
    public void setColumnsResizable(boolean newValue) {
        boolean oldValue = columnsResizable;
        columnsResizable = newValue;

        for (int i = 0; i < getComponentCount(); i++) {
            Component c = getComponent(i);
            if (c instanceof Column) {
                Column column = (Column) c;
                column.setResizable(newValue);
            }
        }

        firePropertyChange(COLUMNS_RESIZABLE_PROPERTY, oldValue, newValue);
    }

    /**
     * Returns true, if the columns are resizable by the uesr.
     *
     * @return The columnsResizable-property.
     **/
    public boolean isColumnsResizable() {
        return columnsResizable;
    }

    /**
     * Sets the delegate that's used to paint a cell and the arrows in the
     * browser. If you don't want to render the arrows, use setCellRenderer
     * instead.
     * <p>
     * The default value of this property is provided by the ListUI
     * delegate, i.e. by the look and feel implementation.
     * <p>
     * This is a JavaBeans bound property.
     *
     * @param cellRenderer the {@code ListCellRenderer}
     *     that paints browser cells
     * @see #getColumnCellRenderer
     * @see #setCellRenderer
     */
    public void setColumnCellRenderer(ListCellRenderer cellRenderer) {
        ListCellRenderer oldValue = this.cellRenderer;
        this.cellRenderer = cellRenderer;

        for (int i = 0; i < getListColumnCount(); i++) {
            getColumnList(i).setCellRenderer(cellRenderer);
        }

        firePropertyChange(CELL_RENDERER_PROPERTY, oldValue, cellRenderer);
    }

    /**
     * Gets the delegate that's used to paint a column cell in the browser.
     * <p>
     * The default value of this property is provided by the BrowserUI
     * delegate, i.e. by the look and feel implementation.
     * <p>
     * This is a JavaBeans bound property.
     *
     * @return The {@code ListCellRenderer}
     *     that paints browser cells
     */
    public ListCellRenderer getColumnCellRenderer() {
        return cellRenderer;
    }

    /**
     * Sets the delegate that's used to paint each cell in the browser.
     * If you use this delegate, you don't have to render the arrows for
     * non-leaf items. If you want to render these arrows,
     * use setColumnCellRenderer instead.
     * <p>
     * The default value of this property is provided by the ListUI
     * delegate, i.e. by the look and feel implementation.
     * <p>
     * This is a JavaBeans bound property.
     *
     * @param cellRenderer the {@code ListCellRenderer}
     *     that paints browser cells
     * @see #getCellRenderer
     * @see #setColumnCellRenderer
     */
    public void setCellRenderer(BrowserCellRenderer cellRenderer) {
        BrowserCellRenderer oldValue = getCellRenderer();

        if (cellRenderer != null) {
            setColumnCellRenderer(new BrowserCellRendererWrapper(cellRenderer));
        } else {
            setColumnCellRenderer(new DefaultColumnCellRenderer(this));
        }

        firePropertyChange(CELL_RENDERER_PROPERTY, oldValue, cellRenderer);
    }

    /**
     * Gets the delegate that's used to paint each cell in the browser.
     * <p>
     * The default value of this property is provided by the BrowserUI
     * delegate, i.e. by the look and feel implementation. This can be null,
     * if the ListCellRenderer for columns does not make use of a
     * BrowserCellRenderer to render a cell.
     * <p>
     * This is a JavaBeans bound property.
     *
     * @return The {@code BrowserCellRenderer}
     *     that paints browser cells
     */
    public BrowserCellRenderer getCellRenderer() {
        return (cellRenderer instanceof BrowserCellRendererWrapper) ? ((BrowserCellRendererWrapper) cellRenderer).browserCellRenderer : null;
    }

    public @Nullable BrowserPreviewRenderer getPreviewRenderer() {
        if (previewColumn != null) {
            return previewColumn.getRenderer();
        } else {
            return null;
        }
    }

    /**
     * Sets the delegate that's used to paint the preview column in the browser.
     * <p>
     * The default value of this property is null. If null, no preview column is shown.
     * <p>
     * This is a JavaBeans bound property.
     *
     * @param renderer the renderer that determines how to display the selected items in the preview column, or null
     *                 to inhibit the preview column.
     * @see #getColumnCellRenderer
     */
    public void setPreviewRenderer(BrowserPreviewRenderer renderer) {

        BrowserPreviewRenderer oldRenderer = null;
        if (renderer == null) {
            // Remove any existing preview column
            if (previewColumn != null) {
                oldRenderer = previewColumn.getRenderer();
                repaint(previewColumn.getBounds());
                remove(previewColumn);
                previewColumn = null;
            }
        } else {
            // Ensure that a preview column exists and is properly configured
            if (previewColumn == null) {
                previewColumn = new PreviewColumn(renderer);
                if (getDropTarget() != null) {
                    new DropTarget(previewColumn, getDropTarget().getDefaultActions(), getDropTarget());
                }
            } else {
                oldRenderer = previewColumn.getRenderer();
                previewColumn.setRenderer(renderer);
            }
        }

        firePropertyChange(PREVIEW_RENDERER_PROPERTY, oldRenderer, renderer);
    }

    /**
     * Returns the number of columns this includes the count of list columns and the preview column (if it is shown).
     */
    private int getColumnCount() {
        return getComponentCount();
    }

    /**
     * Returns the number of list columns. This count does not include the preview column.
     */
    private int getListColumnCount() {
        int count = getComponentCount();
        if (previewColumn != null && previewColumn.getParent() == this) {
            return count - 1;
        } else {
            return count;
        }
    }

    @Override
    public void setDropTarget(DropTarget t) {
        super.setDropTarget(t);
        if (t != null) {
            for (int i = 0, n = getListColumnCount(); i < n; i++) {
                new DropTarget(getColumnList(i), getDropTarget().getDefaultActions(), getDropTarget());
                //getColumnList(i).setDropTarget(t);
            }
        }
        if (previewColumn != null && previewColumn.isVisible()) {
            new DropTarget(previewColumn, getDropTarget().getDefaultActions(), getDropTarget());
        }
    }

    /**
     * Convenience method for accessing the specified column.
     */
    private Column getColumn(int column) {
        return (Column) getComponent(column);
    }

    /**
     * Convenience method for accessing the specified list column.
     */
    private @Nullable ListColumn getListColumn(int column) {
        Component c = getComponent(column);
        if (c instanceof ListColumn) {
            return (ListColumn) c;
        }
        return null;
    }

    /**
     * Convenience method for accessing the JList at the specified column.
     */
    private @NotNull JList getColumnList(int column) {
        ListColumn c = getListColumn(column);
        if (c != null) {
            return c.getList();
        }
        throw new IllegalArgumentException("Invalid list column index: " + column);
    }

    /**
     * Sets the tree's selection model.
     *
     * @param selectionModel the {@code TreeSelectionModel} to use.
     * @see TreeSelectionModel
     *
     * @exception IllegalArgumentException if the selectionModel is null.
     */
    public void setSelectionModel(TreeSelectionModel selectionModel) {
        if (selectionModel == null) {
            throw new IllegalArgumentException();
        }

        TreeSelectionModel oldValue = this.selectionModel;

        if (this.selectionModel != null) {
            this.selectionModel.removeTreeSelectionListener(treeSelectionUpdater);
            this.selectionModel.removePropertyChangeListener(selectionModeUpdater);
        }
        if (accessibleContext != null) {
            this.selectionModel.removeTreeSelectionListener((TreeSelectionListener) accessibleContext);
            selectionModel.addTreeSelectionListener((TreeSelectionListener) accessibleContext);
        }

        this.selectionModel = selectionModel;
        if (selectionModel != null) {
            this.selectionModel.addTreeSelectionListener(treeSelectionUpdater);
            this.selectionModel.addPropertyChangeListener(selectionModeUpdater);
        }
        firePropertyChange(SELECTION_MODEL_PROPERTY, oldValue,
                this.selectionModel);
    }

    /**
     * Returns the model for selections. This should always return a
     * non-{@code null} value. If you don't want to allow anything
     * to be selected
     * set the selection model to {@code null}, which forces an empty
     * selection model to be used.
     *
     * @return the value
     * @see #setSelectionModel
     */
    public TreeSelectionModel getSelectionModel() {
        return selectionModel;
    }

    /**
     * Sets the selection mode, which must be one of
     * TreeSelectionModel.SINGLE_TREE_SELECTION,
     * CONTIGUOUS_TREE_SELECTION or DISCONTIGUOUS_TREE_SELECTION.
     * <p>
     * This may change the selection if the current selection is not valid
     * for the new mode. For example, if three TreePaths are
     * selected when the mode is changed to {@code SINGLE_TREE_SELECTION},
     * only one TreePath will remain selected. It is up to the particular
     * implementation to decide what TreePath remains selected.
     *
     * @param selectionMode the value
     */
    public void setSelectionMode(int selectionMode) {
        selectionModel.setSelectionMode(selectionMode);
    }

    /**
     * Returns the path to the first selected node.
     *
     * @return the {@code TreePath} for the first selected node,
     *  or {@code null} if nothing is currently selected
     */
    public TreePath getSelectionPath() {
        if (getListColumnCount() == 0) {
            return null;
        } else {
            JList list = getColumnList(getListColumnCount() - 1);
            ColumnListModel m = (ColumnListModel) list.getModel();
            int i = list.getSelectedIndex();
            Object pathComponent = (i == -1 || i >= m.getSize()) ? null : list.getSelectedValue();
            return (pathComponent == null) ? m.path : m.path.pathByAddingChild(pathComponent);
        }
    }

    /**
     * Returns the paths of all selected values.
     *
     * @return an array of {@code TreePath} objects indicating the selected
     *         nodes, or {@code null} if nothing is currently selected
     */
    public TreePath[] getSelectionPaths() {
        return getSelectionModel().getSelectionPaths();
    }

    /**
     * Selects the nodes identified by the specified array of paths.
     * All path components except the last one of the paths must be equal.
     *
     * @param paths an array of {@code TreePath} objects that specifies
     *  the nodes to select
     */
    public void setSelectionPaths(TreePath[] paths) {
        getSelectionModel().setSelectionPaths(paths);
    }

    /**
     * Returns an array of all the key listeners
     * registered on this component.
     *
     * @return all of this component's {@code KeyListener}s
     *         or an empty array if no key
     *         listeners are currently registered
     */
    @Override
    public synchronized KeyListener[] getKeyListeners() {
        return (KeyListener[]) (getListeners(KeyListener.class));
    }

    /**
     * Indicate whether the preview column is visible.
     */
    public boolean isPreviewColumnVisible() {
        return previewColumn != null && previewColumn.isVisible() && previewColumn.getParent() == this;
    }

    /**
     * Display the current selection in the preview column, if a preview column is defined and the selection is not
     * empty.
     */
    public void updatePreviewColumn() {
        if (previewColumn != null) {
            boolean wasVisible = isPreviewColumnVisible();
            TreePath[] paths = getSelectionPaths();
            switch ((paths == null) ? 0 : paths.length) {
                case 0:
                    previewColumn.setVisible(false);
                    break;
                case 1:
                    if (treeModel.isLeaf(paths[0].getLastPathComponent())) {
                        previewColumn.updateSelection(paths);
                        previewColumn.setVisible(true);
                    } else {
                        previewColumn.setVisible(false);
                    }
                    break;
                default:
                    previewColumn.updateSelection(paths);
                    previewColumn.setVisible(true);
                    break;
            }
            boolean isVisible = previewColumn.isVisible();
            if (isVisible != wasVisible) {
                if (isVisible && previewColumn.getParent() != this) {
                    add(previewColumn);
                }
                previewColumnVisibilityChanged(isVisible);
            }

            revalidate();
            repaint();
        }
    }

    protected void previewColumnVisibilityChanged(boolean isVisible) {
    }

    /**
     * Adds a listener for {@code TreeSelection} events.
     *
     * @param tsl the {@code TreeSelectionListener} that will be notified when a node is selected or deselected (a
     *            "negative selection")
     */
    public void addTreeSelectionListener(TreeSelectionListener tsl) {
        listenerList.add(TreeSelectionListener.class, tsl);
    }

    /**
     * Removes a {@code TreeSelection} listener.
     *
     * @param tsl the {@code TreeSelectionListener} to remove
     */
    public void removeTreeSelectionListener(TreeSelectionListener tsl) {
        listenerList.remove(TreeSelectionListener.class, tsl);
    }

    /**
     * Notifies all listeners that have registered interest for notification on this event type.
     *
     * @param e the {@code TreeSelectionEvent} to be fired; generated by the {@code TreeSelectionModel} when a node is
     *          selected or deselected
     * @see EventListenerList
     */
    protected void fireValueChanged(TreeSelectionEvent e) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            // TreeSelectionEvent e = null;
            if (listeners[i] == TreeSelectionListener.class) {
                // Lazily create the event:
                // if (e == null)
                // e = new ListSelectionEvent(this, firstIndex, lastIndex);
                ((TreeSelectionListener) listeners[i + 1]).valueChanged(e);
            }
        }
    }

    /**
     * Returns the {@code TreeModel} that is providing the data.
     *
     * @return the {@code TreeModel} that is providing the data
     */
    public TreeModel getModel() {
        return treeModel;
    }

    /**
     * Sets the {@code TreeModel} that will provide the data.
     *
     * @param newModel the {@code TreeModel} that is to provide the data
     */
    public void setModel(TreeModel newModel) {
        TreeModel oldModel = treeModel;

        if (oldModel != null) {
            oldModel.removeTreeModelListener(treeRootHandler);
        }
        treeModel = newModel;
        if (newModel != null) {
            newModel.addTreeModelListener(treeRootHandler);
        }

        for (int i = getListColumnCount() - 1; i >= 0; i--) {
            removeLastListColumn();
        }

        if (treeModel != null) {
            // Clear the expanded path and expand the root
            expandedPath = null;
            expandRoot();
        }

        firePropertyChange(TREE_MODEL_PROPERTY, oldModel, treeModel);
    }

    /**
     * Ensures that the node identified by the specified path is
     * expanded and viewable. If the last item in the path is not a
     * leaf, an additional column is shown.
     *
     * @param path  the {@code TreePath} identifying a node
     */
    private void expandPath(TreePath path) {
        //boolean oldPathIsLeaf = expandedPath == null || treeModel.isLeaf(expandedPath.getLastPathComponent());
        boolean newPathIsLeaf = path == null || treeModel.isLeaf(path.getLastPathComponent());
        int oldColumnCount = (expandedPath == null) ? 0 : (expandedPath.getPathCount() - ((expandedPathIsLeaf) ? 1 : 0));
        int newColumnCount = (path == null) ? 0 : (path.getPathCount() - ((newPathIsLeaf) ? 1 : 0));
        if (expandedPath == null || !expandedPath.equals(path) || expandedPathIsLeaf != newPathIsLeaf) {
            if (oldColumnCount == newColumnCount) {
                TreePath p = null;
                for (int i = 0; i < newColumnCount; i++) {
                    p = (p == null) ? new TreePath(path.getPathComponent(0)) : p.pathByAddingChild(path.getPathComponent(i));
                    JList l = getColumnList(i);
                    ColumnListModel m = (ColumnListModel) l.getModel();
                    m.setPath(p);
                }
            } else {
                // Remove all extraneous columns, avoid removing the column
                // which has focus, because Java has trouble setting the
                // focus on a JComponent, if the JComponent which is focus owner
                // was removed.
                for (int i = getListColumnCount() - 1; i >= newColumnCount; i--) {
                    if (getColumnList(i).isFocusOwner()) {
                        removeListColumn((i > 0) ? i - 1 : i);
                    } else {
                        removeListColumn(i);
                    }
                }
                // Set path of remaining columns
                if (path != null) {
                    TreePath p = null;
                    for (int i = 0, n = getListColumnCount(); i < n; i++) {
                        p = (p == null) ? new TreePath(path.getPathComponent(0)) : p.pathByAddingChild(path.getPathComponent(i));
                        JList l = getColumnList(i);
                        ColumnListModel m = (ColumnListModel) l.getModel();
                        m.setPath(p);
                    }
                }

                // Add new columns if necessary
                if (path != null) {
                    java.util.List components = Arrays.asList(path.getPath());
                    for (int i = getListColumnCount(); i < newColumnCount; i++) {
                        addColumn(new TreePath(components.subList(0, i + 1).toArray()));
                    }
                }
                if (newColumnCount > 0) {
                    revalidate();
                }
                repaint();
            }
            expandedPath = path;
            expandedPathIsLeaf = newPathIsLeaf;

            // Set the selection to match the new path
            // Note: We do not change the selection of the column from
            // getPathCount() - 1 onwards, because the selection of this column
            // must be set by the method calling us.
            if (path != null) {
                int i;
                for (i = 0; i < path.getPathCount() - 1; i++) {
                    getColumnList(i).setSelectedIndex(treeModel.getIndexOfChild(path.getPathComponent(i), path.getPathComponent(i + 1)));
                }
            }

            /*
            if (getParent() != null) {
            getParent().validate();
            } else {
            validate();
            }*/
        }
        //scrollRectToVisible(getComponent(getComponentCount() - 1).getBounds());
    }

    /**
     * Appends a new column to the browser.
     * @param path the value
     */
    protected void addColumn(@NotNull TreePath path) {
        JList l = createColumnList(new ColumnListModel(path, treeModel));
        if (isShowCellTips) {
            l.setToolTipText("cell tip");
        }

        if (getDropTarget() != null) {
            new DropTarget(l, getDropTarget().getDefaultActions(), getDropTarget());
            //l.setDropTarget(getDropTarget());
        }

        if (cellRenderer != null) {
            l.setCellRenderer(cellRenderer);
        }

        if (prototypeCellValue != null) {
            l.setPrototypeCellValue(prototypeCellValue);
        }

        l.setSelectionModel(new ColumnSelectionModel());

        int selectionMode;
        switch (selectionModel.getSelectionMode()) {
            case TreeSelectionModel.CONTIGUOUS_TREE_SELECTION:
                selectionMode = ListSelectionModel.SINGLE_INTERVAL_SELECTION;
                break;
            case TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION:
                selectionMode = ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;
                break;
            case TreeSelectionModel.SINGLE_TREE_SELECTION:
            default:
                selectionMode = ListSelectionModel.SINGLE_SELECTION;
                break;
        }

        l.setSelectionMode(selectionMode);
        l.addMouseListener(columnMouseListener);
        l.addKeyListener(columnKeyListener);
        l.setFixedCellWidth(fixedCellWidth);
        l.setDragEnabled(getDragEnabled());
        l.setTransferHandler(getTransferHandler());
        ListColumn column = new ListColumn(l, path.getPathCount() - 1);

        add(column, getListColumnCount());
    }

    protected @NotNull JList createColumnList(@NotNull ColumnListModel m) {
        JList list = new ColumnList(m);
        if (OSXSystemProperties.OSVersion >= 1016) {
            list.putClientProperty("JList.viewStyle", "inset");
        }
        return list;
    }

    protected class ColumnList extends JList {

        public ColumnList(ColumnListModel m) {
            super(m);
        }

        @Override
        public void updateUI() {
            // Allow the column list UI to be customized for use in a browser
            ListUI basicUI = (ListUI)UIManager.getUI(this);
            setUI(getColumnListUI(basicUI));

            ListCellRenderer renderer = getCellRenderer();
            if (renderer instanceof Component) {
                SwingUtilities.updateComponentTreeUI((Component)renderer);
            }
        }

        @Override
        public String getToolTipText(
                MouseEvent event) {
            Point mouseLocation = event.getPoint();
            int index = locationToIndex(mouseLocation);
            if (index != -1) {
                Rectangle cellBounds = getCellBounds(index, index);
                if (cellBounds.contains(mouseLocation)) {
                    Object value = getModel().getElementAt(index);
                    Component renderer = getCellRenderer().getListCellRendererComponent(this, value, index, false, false);
                    if (renderer.getPreferredSize().width > getWidth()) {
                        return convertValueToText(value, false, false, false, index, false);
                    }

                }
            }
            return null;
        }

        @Override
        public Point getToolTipLocation(
                MouseEvent event) {
            Point mouseLocation = event.getPoint();
            int index = locationToIndex(mouseLocation);
            if (index != -1) {
                Rectangle cellBounds = getCellBounds(index, index);
                if (cellBounds.contains(mouseLocation)) {
                    Object value = getModel().getElementAt(index);
                    Component renderer = getCellRenderer().getListCellRendererComponent(this, value, index, false, false);
                    if (renderer.getPreferredSize().width > getWidth()) {
                        Point location = cellBounds.getLocation();
                        location.x += cellTipOrigin.x;
                        location.y += cellTipOrigin.y;
                        return location;
                    }

                }
            }
            return null;
        }

        @Override
        public String toString() {
            String s = "ColumnList";
            ColumnListModel m = (ColumnListModel) getModel();
            return s + ": " + m;
        }
    }

    /**
     * Removes the last list column from the browser.
     */
    protected void removeLastListColumn() {
        removeListColumn(getListColumnCount() - 1);
    }

    /**
     * Removes the specified column from the browser.
     * @param columnIndex the value
     */
    protected void removeListColumn(int columnIndex) {
        JList l = getColumnList(columnIndex);

        Column sp = (Column) getComponent(columnIndex);
        sp.remove(l);
        remove(sp);

        l.removeMouseListener(columnMouseListener);
        l.removeKeyListener(columnKeyListener);

        ((ColumnListModel) l.getModel()).dispose();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents

        setLayout(new BrowserLayout());
        //setLayout(new java.awt.GridLayout(1, 0));

    }//GEN-END:initComponents

    /**
     * Returns the preferred display size of a {@code JBrowser}.
     * The height is determined from {@code getVisibleRowCount} and
     * the width is the current preferred width of two columns.
     *
     * @return a {@code Dimension} object containing the preferred size
     */
    public Dimension getPreferredScrollableViewportSize() {
        Dimension size = new Dimension();
        if (getComponentCount() == 0) {
            JScrollPane dummyColumn = new JScrollPane();
            JList list = new JList();
            list.setPrototypeCellValue(prototypeCellValue);
            dummyColumn.setViewportView(list);
            size.setSize(dummyColumn.getPreferredSize());
            size.width *= 2;
        } else {
            for (int i = 0; i < getComponentCount(); i++) {
                Dimension componentSize = getComponent(i).getPreferredSize();
                size.height = Math.max(size.height, componentSize.height);
                size.width += componentSize.width;
            }

        }
        /*
        size.height = 10;
        size.width = 10;
         */
        return size;
    }

    /**
     * Returns the amount to increment when scrolling. The amount is 10 or
     * the width of the first/last displayed column that isn't completely in
     * view or, if it is totally displayed, the width of the next column in the
     * scrolling direction.
     * <p>
     * The height is always 10.
     * <p>
     * <b>FIXME</b> - Find a better way to compute the height.
     *
     * @param visibleRect the view area visible within the viewport
     * @param orientation either {@code SwingConstants.VERTICAL}
     *  or {@code SwingConstants.HORIZONTAL}
     * @param direction less than zero to scroll up/left,
     *  greater than zero for down/right
     * @return the "unit" increment for scrolling in the specified direction
     * @see JScrollBar#setUnitIncrement(int)
     */
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        Component[] components = getComponents();
        int increment = 0;
        if (direction > 0) {
            switch (orientation) {
                case SwingConstants.HORIZONTAL:
                    for (int i = components.length - 1; i
                            >= 0; i--) {
                        Rectangle cbounds = components[i].getBounds();
                        if (cbounds.x + cbounds.width > visibleRect.x + visibleRect.width) {
                            increment = Math.min(10, cbounds.x + cbounds.width - visibleRect.x - visibleRect.width);
                        }

                    }
                    break;
                case SwingConstants.VERTICAL:
                    increment = 10;
                    break;
            }

        } else {
            switch (orientation) {
                case SwingConstants.HORIZONTAL:
                    for (int i = 0; i
                            < components.length; i++) {
                        Rectangle cbounds = components[i].getBounds();
                        if (cbounds.x < visibleRect.x) {
                            increment = Math.min(10, visibleRect.x - cbounds.x);
                        }

                    }
                    break;
                case SwingConstants.VERTICAL:
                    increment = 10;
                    break;
            }

        }
        return increment;
    }

    /**
     * Returns true to indicate that the height of the viewport
     * determines the height of the browser.
     * <p>
     * In other words: the JScrollPane must never show a vertical scroll bar,
     * because each column of the JBrowser provides one.
     *
     * @return true
     * @see Scrollable#getScrollableTracksViewportHeight
     */
    public boolean getScrollableTracksViewportHeight() {
        return true;
    }

    /**
     * Returns false to indicate that the width of the viewport does not
     * determine the width of the browser, unless the preferred width of
     * the browser is smaller than the viewports width.  In other words:
     * ensure that the browser is never smaller than its viewport.
     *
     * @return false
     * @see Scrollable#getScrollableTracksViewportWidth
     */
    public boolean getScrollableTracksViewportWidth() {
        return false;
    }

    /**
     * Returns the amount to increment when scrolling. The amount is
     * the distance to the next column in the scrolling direction which
     * is outside of the view.
     * <p>
     * The height is always 10.
     * <p>
     * <b>FIXME</b> - Find a better way to compute the height.
     *
     * @param visibleRect the view area visible within the viewport
     * @param orientation either {@code SwingConstants.VERTICAL}
     *  or {@code SwingConstants.HORIZONTAL}
     * @param direction less than zero to scroll up/left,
     *  greater than zero for down/right
     * @return the "unit" increment for scrolling in the specified direction
     * @see JScrollBar#setUnitIncrement(int)
     */
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        Component[] components = getComponents();
        int increment = 0;
        if (direction > 0) {
            switch (orientation) {
                case SwingConstants.HORIZONTAL:
                    for (int i = components.length - 1; i >= 0; i--) {
                        Rectangle cbounds = components[i].getBounds();

                        if (cbounds.x > visibleRect.x) {
                            increment = cbounds.x - visibleRect.x;
                        } else if (cbounds.x + cbounds.width > visibleRect.x + visibleRect.width) {
                            increment = cbounds.x - visibleRect.x;
                        }
                    }
                    break;
                case SwingConstants.VERTICAL:
                    increment = 10;
                    break;
            }

        } else {
            switch (orientation) {
                case SwingConstants.HORIZONTAL:
                    for (int i = 0; i
                            < components.length; i++) {
                        Rectangle cbounds = components[i].getBounds();
                        if (cbounds.x + cbounds.width < visibleRect.x) {
                            increment = visibleRect.x - cbounds.x;
                        }

                    }
                    if (increment == 0) {
                        increment = visibleRect.x - components[0].getBounds().x;
                    }
                    break;
                case SwingConstants.VERTICAL:
                    increment = 10;
                    break;
            }

        }
        return increment;
    }

    /**
     * Calls the {@code configureEnclosingScrollPane} method.
     *
     * @see #configureEnclosingScrollPane
     */
    @Override
    public void addNotify() {
        super.addNotify();
        configureEnclosingScrollPane();
    }

    /**
     * If this {@code JTable} is the {@code viewportView} of an enclosing {@code JScrollPane}
     * (the usual situation), configure this {@code ScrollPane} by, amongst other things,
     * installing the table's {@code tableHeader} as the {@code columnHeaderView} of the scroll pane.
     * When a {@code JTable} is added to a {@code JScrollPane} in the usual way,
     * using {@code new JScrollPane(myTable)}, {@code addNotify} is
     * called in the {@code JTable} (when the table is added to the viewport).
     * {@code JTable}'s {@code addNotify} method in turn calls this method,
     * which is protected so that this default installation procedure can
     * be overridden by a subclass.
     *
     * @see #addNotify
     */
    protected void configureEnclosingScrollPane() {
        Container p = getParent();
        if (p instanceof JViewport) {
            JViewport viewport = (JViewport) p;
            //viewport.setOpaque(true);
        }
    }

    /**
     * Calls the {@code unconfigureEnclosingScrollPane} method.
     *
     * @see #unconfigureEnclosingScrollPane
     */
    @Override
    public void removeNotify() {
        unconfigureEnclosingScrollPane();
        super.removeNotify();
    }

    /**
     * Reverses the effect of {@code configureEnclosingScrollPane}
     * by replacing the {@code columnHeaderView} of the enclosing
     * scroll pane with {@code null}. {@code JTable}'s
     * {@code removeNotify} method calls
     * this method, which is protected so that this default uninstallation
     * procedure can be overridden by a subclass.
     *
     * @see #removeNotify
     * @see #configureEnclosingScrollPane
     */
    protected void unconfigureEnclosingScrollPane() {
        Container p = getParent();
        if (p instanceof JViewport) {
            JViewport viewport = (JViewport) p;
        }
    }

    @Override
    public void requestFocus() {
        TreePath tp = getSelectionPath();
        if (tp != null && tp.getPathCount() > 1) {
            getColumnList(tp.getPathCount() - 2).requestFocus();
        } else {
            getColumnList(getListColumnCount() - 1).requestFocus();
        }
    }

    @Override
    public boolean requestFocusInWindow() {
        TreePath tp = getSelectionPath();
        if (tp != null && tp.getPathCount() > 1) {
            return getColumnList(tp.getPathCount() - 2).requestFocusInWindow();
        } else {
            return getColumnList(getListColumnCount() - 1).requestFocusInWindow();
        }
    }

    /**
     * Called by the renderers to convert the specified value to
     * text. This implementation returns {@code value.toString}, ignoring
     * all other arguments. To control the conversion, subclass this
     * method and use any of the arguments you need.
     *
     * @param value the {@code Object} to convert to text
     * @param selected true if the node is selected
     * @param expanded true if the node is expanded
     * @param leaf  true if the node is a leaf node
     * @param row  an integer specifying the node's display row, where 0 is
     *             the first row in the display
     * @param hasFocus true if the node has the focus
     * @return the {@code String} representation of the node's value
     */
    public String convertValueToText(
            Object value, boolean selected,
            boolean expanded, boolean leaf, int row,
            boolean hasFocus) {
        if (value != null) {
            String sValue = value.toString();
            if (sValue != null) {
                return sValue;
            }

        }
        return "";
    }

    protected static class BrowserScrollPaneLayout extends ScrollPaneLayout implements UIResource {

        // This class is a UIResource so that it will not prevent AquaScrollPaneUI from installing a special layout
        // manager when overlay scroll bars are being used.

        @Override
        public void layoutContainer(Container parent) {
            super.layoutContainer(parent);

            boolean cornerVisible = lowerRight != null && lowerRight.isVisible();
            boolean vsbVisible = vsb != null && vsb.isVisible();
            boolean hsbVisible = hsb != null && hsb.isVisible();

            if (cornerVisible && vsbVisible && !hsbVisible) {
                Dimension cornerDim = lowerRight.getPreferredSize();
                Dimension vsbDim = vsb.getSize();

                vsb.setSize(vsbDim.width, vsbDim.height - cornerDim.height);
                lowerRight.setBounds(vsb.getX(), vsb.getY() + vsb.getHeight(), vsbDim.width, cornerDim.height);
            }
        }
    }

    protected static class BrowserLayout implements LayoutManager2 {

        // The components of a browser are columns. They are stacked in row with no extra spacing.

        private int minimumWidth = 0;
        private int minimumHeight = 0;
        private int preferredWidth = 0;
        private int preferredHeight = 0;
        private boolean sizeUnknown = true;

        @Override
        public void addLayoutComponent(Component comp, Object constraints) {
        }

        @Override
        public float getLayoutAlignmentX(Container target) {
            return 0;
        }

        @Override
        public float getLayoutAlignmentY(Container target) {
            return 0;
        }

        @Override
        public void invalidateLayout(Container target) {
            sizeUnknown = true;
        }

        @Override
        public void addLayoutComponent(String name, Component comp) {
        }

        @Override
        public void removeLayoutComponent(Component comp) {
        }

        private void setSizes(Container parent) {
            int nComps = parent.getComponentCount();

            minimumWidth = 0;
            minimumHeight = 0;
            preferredWidth = 0;
            preferredHeight = 0;

            for (int i = 0; i < nComps; i++) {
                Component c = parent.getComponent(i);
                if (c.isVisible()) {
                    Dimension min = c.getMinimumSize();
                    minimumWidth += min.width;
                    minimumHeight = Math.max(minimumHeight, min.height);
                    Dimension pref = c.getPreferredSize();
                    preferredWidth += pref.width;
                    preferredHeight = Math.max(preferredHeight, pref.height);
                }
            }

            sizeUnknown = false;
        }

        @Override
        public Dimension preferredLayoutSize(Container parent) {

            setSizes(parent);

            Insets insets = parent.getInsets();
            int width = preferredWidth + insets.left + insets.right;
            int height = preferredHeight + insets.top + insets.bottom;
            return new Dimension(width, height);
        }

        @Override
        public Dimension minimumLayoutSize(Container parent) {

            setSizes(parent);

            Insets insets = parent.getInsets();
            int width = minimumWidth + insets.left + insets.right;
            int height = minimumHeight + insets.top + insets.bottom;
            return new Dimension(width, height);
        }

        @Override
        public Dimension maximumLayoutSize(Container target) {
            return new Dimension(100000, 100000);
        }

        /*
         * This is called when the panel is first displayed,
         * and every time its size changes.
         */
        @Override
        public void layoutContainer(Container parent) {

            JBrowser b = (JBrowser) parent;

            Insets insets = parent.getInsets();
            ///int maxWidth = parent.getWidth() - (insets.left + insets.right);
            int maxHeight = parent.getHeight() - (insets.top + insets.bottom);
            int nComps = parent.getComponentCount();
            int previousWidth = 0, previousHeight = 0;

            int x = insets.left, y = insets.top;
            int rowh = 0, start = 0;

            if (sizeUnknown) {
                setSizes(parent);
            }

            for (int i = 0; i < nComps; i++) {
                Component c = parent.getComponent(i);
                if (c.isVisible()) {
                    Dimension d = c.getPreferredSize();

                    x += previousWidth;
                    d.height = maxHeight;

                    // Set the component's size and position.
                    int cwidth = d.width;
                    int cheight = d.height;

                    if (c == b.previewColumn && b.shouldFillPreviewColumn) {
                        int availableWidth = parent.getWidth() - x - insets.right;
                        cwidth = Math.max(cwidth, availableWidth);
                    }

                    c.setBounds(x, y, cwidth, cheight);

                    previousWidth = cwidth;
                    previousHeight = cheight;
                }
            }
        }
    }

    /**
     * This is the list model used to map a tree node of the {@code treeModel}
     * to a JList displaying its children.
     */
    private class ColumnListModel extends AbstractListModel implements TreeModelListener {

        private TreePath path;
        private TreeModel model;
        /**
         * We need to copy the number of children of the underlying tree node
         * into this instance variable, because we have to generate a proper
         * interval added/interval removed even upon a change in the tree structure.
         */
        private int size;

        @Override
        public String toString() {
            StringBuilder buf = new StringBuilder();
            buf.append('{');
            for (int i = 0, n = getSize(); i < n; i++) {
                if (i != 0) {
                    buf.append(',');
                }
                buf.append(getElementAt(i));
            }
            buf.append("} #");
            buf.append(hashCode());
            return buf.toString();
        }

        public ColumnListModel(TreePath path, TreeModel model) {
            this.path = path;
            this.model = model;
            model.addTreeModelListener(this);
            updateSize();
        }

        public void setPath(TreePath newValue) {
            if (newValue != path) {
                int oldSize = getSize();
                this.path = newValue;
                updateSize();
                int newSize = getSize();
                if (Math.min(oldSize, newSize) > 0) {
                    fireContentsChanged(this, Math.min(oldSize, newSize), Math.min(oldSize, newSize));
                }
                if (newSize < oldSize) {
                    fireIntervalRemoved(this, newSize, oldSize - 1);
                } else if (newSize > oldSize) {
                    fireIntervalAdded(this, oldSize, newSize - 1);
                }
            }
        }

        public void dispose() {
            model.removeTreeModelListener(this);
        }

        public int getSize() {
            return size;
            //return model.getChildCount(path.getLastPathComponent());
        }

        private void updateSize() {
            this.size = model.getChildCount(path.getLastPathComponent());
        }

        public Object getElementAt(int row) {
            return model.getChild(path.getLastPathComponent(), row);
        }

        public void treeNodesChanged(TreeModelEvent e) {
            if (e.getTreePath().equals(path)) {
                int[] indices = e.getChildIndices();
                fireContentsChanged(this, indices[0], indices[indices.length - 1]);
            }
        }

        public void treeNodesInserted(TreeModelEvent e) {
            if (e.getTreePath().equals(path)) {
                updateSize();

                // We analyze the indices for contiguous intervals
                // and fire interval added events for all intervals we find.
                int[] indices = e.getChildIndices();

                int start = 0;
                int startIndex;
                int end;
                do {
                    startIndex = indices[start];
                    for (end = start + 1; end < indices.length; end++) {
                        if (indices[end] != startIndex + end - start) {
                            break;
                        }
                    }
                    fireIntervalAdded(this, startIndex, indices[end - 1]);
                    start = end;
                } while (start < indices.length);

            } else if (path.getPathCount() == 1) {
                if (expandedPath != null && expandedPathIsLeaf && e.getTreePath().equals(expandedPath)) {
                    // Due to the insertion, the last path component of the
                    // expanded path has been converted from a leaf into an
                    // inner node. Expand path again.
                    expandPath(expandedPath);
                }
            }

            ensureSelectionVisible();
        }

        public void treeNodesRemoved(TreeModelEvent e) {
            if (e.getTreePath().equals(path)) {
                updateSize();

                // We analyze the indices for contiguous intervals
                // and fire interval removed events for all intervals we find.
                int[] indices = e.getChildIndices();
                int start = 0;
                int startIndex;
                int end;
                int offset = 0;
                do {
                    startIndex = indices[start];
                    for (end = start + 1; end < indices.length; end++) {
                        if (indices[end] != startIndex + end - start) {
                            break;
                        }
                    }
                    fireIntervalRemoved(this, startIndex - offset, indices[end - 1] - offset);
                    offset += indices[end - 1] - startIndex + 1;
                    start = end;
                } while (start < indices.length);

                // RemovedChildren can't be selected.
                if (selectionModel.getSelectionCount() > 0) {
                    TreePath[] selectionPaths = selectionModel.getSelectionPaths();
                    Object[] removedChildren = e.getChildren();
                    for (int i = 0; i < removedChildren.length; i++) {
                        TreePath removedPath = e.getTreePath().pathByAddingChild(removedChildren[i]);
                        for (int j = 0; j < selectionPaths.length; j++) {
                            if (removedPath.isDescendant(selectionPaths[j])) {
                                selectionPaths[j] = e.getTreePath();
                            }
                        }
                        setSelectionPaths(selectionPaths);
                    }
                }

                ensureSelectionVisible();
            }
        }

        public void treeStructureChanged(TreeModelEvent e) {
            TreePath changedPath = e.getTreePath();
            if (changedPath.equals(path) || path.getPathCount() == 1 && changedPath.getPathCount() == 1) {
                int oldSize = getSize();
                path = changedPath;
                updateSize();
                int newSize = getSize();
                path = changedPath;
                int diff = newSize - oldSize;
                if (diff < 0) {
                    if (newSize > 0) {
                        fireContentsChanged(this, 0, newSize - 1);
                    }
                    fireIntervalRemoved(this, newSize, oldSize - 1);
                } else if (diff > 0) {
                    if (oldSize > 0) {
                        fireContentsChanged(this, 0, oldSize - 1);
                    }
                    fireIntervalAdded(this, oldSize, newSize - 1);
                } else {
                    fireContentsChanged(this, 0, oldSize - 1);
                }

                setSelectionPath(changedPath);
            }
        }

        /**
         * This is an attempt to keep the selected item visible after a wholesale change in the contents of the list,
         * as might happen when toggling file hiding.
         */
        private void ensureSelectionVisible() {
            JList list = getColumnList(path.getPathCount() - 1);
            if (list != null) {
                ListSelectionModel sm = list.getSelectionModel();
                if (!sm.isSelectionEmpty()) {
                    int index1 = sm.getMinSelectionIndex();
                    int index2 = sm.getMaxSelectionIndex();
                    if (index1 == index2) {
                        list.ensureIndexIsVisible(index1);
                    }
                }
            }
        }
    }

    /**
     * Expands columns of the JBrowser to ensure that selected columns
     * are visible.
     * <p>
     * Handles creating a new {@code TreeSelectionEvent} with the
     * {@code JBrowser} as the source and passing it off to all the
     * listeners.
     */
    private class TreeSelectionUpdater
            implements java.io.Serializable, TreeSelectionListener {

        /**
         * Invoked by the {@code TreeSelectionModel} when the
         * selection changes.
         *
         * @param evt the {@code TreeSelectionEvent} generated by the
         *  {@code TreeSelectionModel}
         */
        public void valueChanged(TreeSelectionEvent evt) {
            // Expand columns to match the selection
            switch (selectionModel.getSelectionCount()) {
                case 0:
                    expandPath(new TreePath(treeModel.getRoot()));
                {
                    int count = getListColumnCount();
                    if (count > 0) {
                        getColumnList(count - 1).clearSelection();
                    }
                }
                break;
                case 1: {
                    TreePath selectionPath = selectionModel.getSelectionPath();
                    expandPath(selectionPath);
                    if (!treeModel.isLeaf(selectionPath.getLastPathComponent())) {
                        int count = getListColumnCount();
                        getColumnList(count - 1).clearSelection();
                    }
                    break;
                }
                default: {
                    TreePath leadSelectionPath = selectionModel.getLeadSelectionPath();
                    TreePath parentPath = leadSelectionPath.getParentPath();
                    expandPath(parentPath);
                    if (parentPath != null) {
                        JList list = getColumnList(parentPath.getPathCount() - 1);

                        TreePath[] selectionPaths = selectionModel.getSelectionPaths();
                        int[] indices = new int[selectionPaths.length];
                        int leadPathIndex = -1;
                        for (int i = 0; i < selectionPaths.length; i++) {
                            indices[i] = treeModel.getIndexOfChild(parentPath.getLastPathComponent(), selectionPaths[i].getLastPathComponent());
                            if (selectionPaths[i].equals(leadSelectionPath)) {
                                leadPathIndex = i;
                            }
                        }
                        int swap = indices[leadPathIndex];
                        indices[leadPathIndex] = indices[indices.length - 1];
                        indices[indices.length - 1] = swap;

                        // Want to preserve the list anchor. The list anchor should respond to the list UI, not the tree model.
                        int anchorIndex = list.getAnchorSelectionIndex();
                        list.setSelectedIndices(indices);
                        if (anchorIndex >= 0 && list.isSelectedIndex(anchorIndex)) {
                            list.getSelectionModel().setAnchorSelectionIndex(anchorIndex);
                        }
                    }

                    break;
                }
            }

            JBrowser.this.requestFocusInWindow();

            updatePreviewColumn();

            //validate();
            if (getParent() != null) {
                getParent().validate();
            }
            int lastComponentIndex = getComponentCount() - 1;
            if (lastComponentIndex >= 0) {
                Rectangle bounds = getComponent(lastComponentIndex).getBounds();
                // Try to scroll two columns into view, so the list showing selection
                // is visible as well
                if (lastComponentIndex > 0) {
                    Rectangle leftBounds = getComponent(lastComponentIndex - 1).getBounds();
                    bounds.add(leftBounds);
                }
                scrollRectToVisible(bounds);
                getComponent(lastComponentIndex).repaint();
            }

            // Propagate event to listeners of the JBrowser
            if (listenerList.getListenerCount(TreeSelectionListener.class) != 0) {
                TreeSelectionEvent newE;

                newE = (TreeSelectionEvent) evt.cloneWithSource(JBrowser.this);
                fireValueChanged(newE);
            }
        }
    } // End of class JBrowser.TreeSelectionModelListener

    /**
     * Propagates selection mode changes of the TreeSelectionModel to the
     * ListSelectionModel's of the columns.
     */
    private class SelectionModeUpdater
            implements java.io.Serializable, PropertyChangeListener {

        public void propertyChange(PropertyChangeEvent evt) {
            if (evt.getPropertyName().equals("selectionMode")) {
                int selectionMode;
                switch (selectionModel.getSelectionMode()) {
                    case TreeSelectionModel.CONTIGUOUS_TREE_SELECTION:
                        selectionMode = ListSelectionModel.SINGLE_INTERVAL_SELECTION;
                        break;
                    case TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION:
                        selectionMode = ListSelectionModel.MULTIPLE_INTERVAL_SELECTION;
                        break;
                    case TreeSelectionModel.SINGLE_TREE_SELECTION:
                    default:
                        selectionMode = ListSelectionModel.SINGLE_SELECTION;
                        break;
                }

                for (int i = 0; i < getListColumnCount(); i++) {
                    getColumnList(i).getSelectionModel().setSelectionMode(selectionMode);
                }
            }
        }
    } // End of class JBrowser.SelectionModeUpdater

    /**
     * This listener listens on mouse events that occur on the columns.
     * <br>
     * FIXME - this listener works only, when it receives listener events
     * <b>after</b> the ListSelectionListener of the ListUI of a column has
     * processed its events.
     */
    private class ColumnMouseListener extends MouseAdapter {

        @Override
        public void mouseReleased(MouseEvent evt) {
            JList columnList = (JList) evt.getComponent();
            EventListener[] listeners = JBrowser.this.getListeners(MouseListener.class);
            if (listeners.length > 0) {
                int x = evt.getX();
                int y = evt.getY();
                Component c = columnList;
                while (c != JBrowser.this) {
                    x += c.getX();
                    y += c.getY();
                    c = c.getParent();
                }
                MouseEvent refiredEvent = new MouseEvent(
                        JBrowser.this, evt.getID(), evt.getWhen(), evt.getModifiers(),
                        x, y,
                        evt.getClickCount(), evt.isPopupTrigger() //, evt.getButton()
                );
                for (int i = 0; i < listeners.length; i++) {
                    ((MouseListener) listeners[i]).mouseReleased(refiredEvent);
                }
            }
            // selectionModel.
            updateExpandedState(columnList);
            //columnList.requestFocus();
        }

        private void updateExpandedState(JList columnList) {
            ColumnListModel columnModel = (ColumnListModel) columnList.getModel();
            TreePath columnPath = columnModel.path;
            int[] selectedIndices = columnList.getSelectedIndices();
            TreePath leadPath;
            if (selectedIndices.length == 0) {
                leadPath = columnModel.path;
                /*if (getListColumnCount() > 1) {
                getColumnList(getListColumnCount() - 2).requestFocus();
                }*/
                selectionModel.setSelectionPath(leadPath);
            } else if (selectedIndices.length == 1) {
                leadPath = columnPath.pathByAddingChild(columnList.getSelectedValue());
                selectionModel.setSelectionPath(leadPath);
            } else {
                int leadSelectionIndex = columnList.getLeadSelectionIndex();
                if (leadSelectionIndex < 0 || leadSelectionIndex >= columnModel.getSize()) {
                    // The lead selection index is out of sync, but we might still
                    // be able to update our selectionModel with the correct elements.
                    TreePath[] paths = new TreePath[selectedIndices.length];
                    for (int i = 0; i < selectedIndices.length; i++) {
                        paths[i] = columnModel.path.pathByAddingChild(columnModel.getElementAt(selectedIndices[i]));
                    }
                    selectionModel.setSelectionPaths(paths);
                } else {
                    // The lead selection index is okay, update our selectionModel
                    // with the correct elements, putting the lead path to the front.
                    leadPath = columnPath.pathByAddingChild(columnModel.getElementAt(leadSelectionIndex));
                    TreePath[] paths = new TreePath[selectedIndices.length];
                    int leadPathIndex = -1;
                    for (int i = 0; i < selectedIndices.length; i++) {
                        paths[i] = columnModel.path.pathByAddingChild(columnModel.getElementAt(selectedIndices[i]));
                        if (paths[i].equals(leadPath)) {
                            leadPathIndex = i;
                        }
                    }
                    if (leadPathIndex != -1) {
                        paths[leadPathIndex] = paths[paths.length - 1];
                        paths[paths.length - 1] = leadPath;
                    }
                    selectionModel.setSelectionPaths(paths);
                }
            }
        }

        @Override
        public void mouseClicked(MouseEvent evt) {
            JList columnList = (JList) evt.getComponent();
            EventListener[] listeners = JBrowser.this.getListeners(MouseListener.class);
            if (listeners.length > 0) {
                int x = evt.getX();
                int y = evt.getY();
                Component c = columnList;
                while (c != JBrowser.this) {
                    x += c.getX();
                    y += c.getY();
                    c = c.getParent();
                }
                MouseEvent refiredEvent = new MouseEvent(
                        JBrowser.this, evt.getID(), evt.getWhen(), evt.getModifiers(),
                        x, y,
                        evt.getClickCount(), evt.isPopupTrigger() //, evt.getButton()
                );
                for (int i = 0; i < listeners.length; i++) {
                    ((MouseListener) listeners[i]).mouseClicked(refiredEvent);
                }
            }
        }

        @Override
        public void mousePressed(MouseEvent evt) {
            JList columnList = (JList) evt.getComponent();
            EventListener[] listeners = JBrowser.this.getListeners(MouseListener.class);
            if (listeners.length > 0) {
                int x = evt.getX();
                int y = evt.getY();
                Component c = columnList;
                while (c != JBrowser.this) {
                    x += c.getX();
                    y += c.getY();
                    c = c.getParent();
                }
                MouseEvent refiredEvent = new MouseEvent(
                        JBrowser.this, evt.getID(), evt.getWhen(), evt.getModifiers(),
                        x, y,
                        evt.getClickCount(), evt.isPopupTrigger() /*, evt.getButton()*/);
                for (int i = 0; i < listeners.length; i++) {
                    ((MouseListener) listeners[i]).mousePressed(refiredEvent);
                }
            }
            // Do not change the expanded state while the mouse is pressed.
            // We update it only on mouse released, to be more consistent with
            // the native file chooser.
            //updateExpandedState(columnList);
        }
    }

    /**
     * This listener listens on key released events that occur on the columns.
     * <br>
     * FIXME - this listener works only, when it receives listener events
     * <b>after</b> the KeyListener of the ListUI of a column has
     * processed its events.
     */
    private class ColumnKeyListener implements KeyListener {

        public void keyReleased(KeyEvent evt) {
            JList columnList = (JList) evt.getComponent();
            ColumnListModel columnModel = (ColumnListModel) columnList.getModel();
            TreePath columnPath = columnModel.path;

            if (evt.getKeyCode() == KeyEvent.VK_LEFT) {
                if (columnPath.getPathCount() > 1) {
                    evt.consume();
                    columnList.clearSelection();
                    JList parentColumnList = getColumnList(columnPath.getPathCount() - 2);
                    selectionModel.setSelectionPath(columnPath);
                    parentColumnList.requestFocus();
                }
            } else if (evt.getKeyCode() == KeyEvent.VK_RIGHT) {
                JList childColumnList = getColumnList(getListColumnCount() - 1);
                if (childColumnList.getSelectedIndex() == -1 && childColumnList.getModel().getSize() != 0) {
                    evt.consume();
                    childColumnList.setSelectedIndex(0);
                    selectionModel.setSelectionPath(((ColumnListModel) childColumnList.getModel()).path.pathByAddingChild(childColumnList.getSelectedValue()));
                }
                childColumnList.requestFocus();
            } else {
                int[] selectedIndices = columnList.getSelectedIndices();
                TreePath leadPath;
                if (selectedIndices.length == 0) {
                    leadPath = columnModel.path;
                    selectionModel.setSelectionPath(leadPath);
                    if (getListColumnCount() > 1) {
                        getColumnList(getListColumnCount() - 2).requestFocus();
                    }
                } else if (selectedIndices.length == 1) {
                    leadPath = columnPath.pathByAddingChild(columnList.getSelectedValue());
                    selectionModel.setSelectionPath(leadPath);
                } else {
                    leadPath = columnPath.pathByAddingChild(columnModel.getElementAt(columnList.getLeadSelectionIndex()));
                    TreePath[] paths = new TreePath[selectedIndices.length];
                    int leadPathIndex = -1;
                    for (int i = 0; i < selectedIndices.length; i++) {
                        paths[i] = columnModel.path.pathByAddingChild(columnModel.getElementAt(selectedIndices[i]));
                        if (paths[i].equals(leadPath)) {
                            leadPathIndex = i;
                        }
                    }

                    if (leadPathIndex >= 0) {
                        paths[leadPathIndex] = paths[paths.length - 1];
                        paths[paths.length - 1] = leadPath;
                    }

                    selectionModel.setSelectionPaths(paths);
                }
            }
            if (!evt.isConsumed()) {
                KeyListener[] kl = getKeyListeners();
                for (int i = 0; i < kl.length; i++) {
                    kl[i].keyReleased(evt);
                }
            }
        }

        public void keyTyped(KeyEvent evt) {
            if (!evt.isConsumed()) {
                KeyListener[] kl = getKeyListeners();
                for (int i = 0; i < kl.length; i++) {
                    kl[i].keyTyped(evt);
                }
            }
        }

        public void keyPressed(KeyEvent evt) {
            if (!evt.isConsumed()) {
                KeyListener[] kl = getKeyListeners();
                for (int i = 0; i < kl.length; i++) {
                    kl[i].keyPressed(evt);
                }
            }
        }
    }

    /**
     * This is a copy of DefaultListSelectionModel.
     * I wish I could subclass it, because the only method which is differs
     * to DefaultListSelectionModel is method insertIndexInterval(...).
     * Unfortunately the instance variable "value" can not be accessed
     * by subclasses.
     */
    private static class ColumnSelectionModel implements ListSelectionModel, Cloneable, java.io.Serializable {

        private static final int MIN = -1;
        private static final int MAX = Integer.MAX_VALUE;
        private int selectionMode = MULTIPLE_INTERVAL_SELECTION;
        private int minIndex = MAX;
        private int maxIndex = MIN;
        private int anchorIndex = -1;
        private int leadIndex = -1;
        private int firstAdjustedIndex = MAX;
        private int lastAdjustedIndex = MIN;
        private boolean isAdjusting = false;
        private int firstChangedIndex = MAX;
        private int lastChangedIndex = MIN;
        private BitSet value = new BitSet(32);
        protected EventListenerList listenerList = new EventListenerList();
        protected boolean leadAnchorNotificationEnabled = true;

        // implements javax.swing.ListSelectionModel
        public int getMinSelectionIndex() {
            return isSelectionEmpty() ? -1 : minIndex;
        }

        // implements javax.swing.ListSelectionModel
        public int getMaxSelectionIndex() {
            return maxIndex;
        }

        // implements javax.swing.ListSelectionModel
        public boolean getValueIsAdjusting() {
            return isAdjusting;
        }

        // implements javax.swing.ListSelectionModel
        /**
         * Returns the selection mode.
         * @return  one of the these values:
         * <ul>
         * <li>SINGLE_SELECTION
         * <li>SINGLE_INTERVAL_SELECTION
         * <li>MULTIPLE_INTERVAL_SELECTION
         * </ul>
         * @see #getSelectionMode
         */
        public int getSelectionMode() {
            return selectionMode;
        }

        // implements javax.swing.ListSelectionModel
        /**
         * Sets the selection mode.  The default is
         * MULTIPLE_INTERVAL_SELECTION.
         * @param selectionMode  one of three values:
         * <ul>
         * <li>SINGLE_SELECTION
         * <li>SINGLE_INTERVAL_SELECTION
         * <li>MULTIPLE_INTERVAL_SELECTION
         * </ul>
         * @exception IllegalArgumentException  if {@code selectionMode}
         *  is not one of the legal values shown above
         * @see #setSelectionMode
         */
        public void setSelectionMode(int selectionMode) {
            switch (selectionMode) {
                case SINGLE_SELECTION:
                case SINGLE_INTERVAL_SELECTION:
                case MULTIPLE_INTERVAL_SELECTION:
                    this.selectionMode = selectionMode;
                    break;
                default:
                    throw new IllegalArgumentException("invalid selectionMode");
            }
        }

        // implements javax.swing.ListSelectionModel
        public boolean isSelectedIndex(int index) {
            return ((index < minIndex) || (index > maxIndex)) ? false : value.get(index);
        }

        // implements javax.swing.ListSelectionModel
        public boolean isSelectionEmpty() {
            return (minIndex > maxIndex);
        }

        // implements javax.swing.ListSelectionModel
        public void addListSelectionListener(ListSelectionListener l) {
            listenerList.add(ListSelectionListener.class, l);
        }

        // implements javax.swing.ListSelectionModel
        public void removeListSelectionListener(ListSelectionListener l) {
            listenerList.remove(ListSelectionListener.class, l);
        }

        /**
         * Returns an array of all the list selection listeners
         * registered on this {@code DefaultListSelectionModel}.
         *
         * @return all of this model's {@code ListSelectionListener}s
         *         or an empty
         *         array if no list selection listeners are currently registered
         *
         * @see #addListSelectionListener
         * @see #removeListSelectionListener
         *
         * @since 1.4
         */
        public ListSelectionListener[] getListSelectionListeners() {
            return (ListSelectionListener[]) listenerList.getListeners(
                    ListSelectionListener.class);
        }

        /**
         * Notifies listeners that we have ended a series of adjustments.
         */
        protected void fireValueChanged(boolean isAdjusting) {
            if (lastChangedIndex == MIN) {
                return;
            }
            /* Change the values before sending the event to the
             * listeners in case the event causes a listener to make
             * another change to the selection.
             */
            int oldFirstChangedIndex = firstChangedIndex;
            int oldLastChangedIndex = lastChangedIndex;
            firstChangedIndex = MAX;
            lastChangedIndex = MIN;
            fireValueChanged(oldFirstChangedIndex, oldLastChangedIndex, isAdjusting);
        }

        /**
         * Notifies {@code ListSelectionListeners} that the value
         * of the selection, in the closed interval {@code firstIndex},
         * {@code lastIndex}, has changed.
         */
        protected void fireValueChanged(int firstIndex, int lastIndex) {
            fireValueChanged(firstIndex, lastIndex, getValueIsAdjusting());
        }

        /**
         * @param firstIndex the first index in the interval
         * @param lastIndex the last index in the interval
         * @param isAdjusting true if this is the final change in a series of
         *  adjustments
         * @see EventListenerList
         */
        protected void fireValueChanged(int firstIndex, int lastIndex, boolean isAdjusting) {
            Object[] listeners = listenerList.getListenerList();
            ListSelectionEvent e = null;

            for (int i = listeners.length - 2; i >= 0; i -= 2) {
                if (listeners[i] == ListSelectionListener.class) {
                    if (e == null) {
                        e = new ListSelectionEvent(this, firstIndex, lastIndex, isAdjusting);
                    }
                    ((ListSelectionListener) listeners[i + 1]).valueChanged(e);
                }
            }
        }

        private void fireValueChanged() {
            if (lastAdjustedIndex == MIN) {
                return;
            }
            /* If getValueAdjusting() is true, (eg. during a drag opereration)
             * record the bounds of the changes so that, when the drag finishes (and
             * setValueAdjusting(false) is called) we can post a single event
             * with bounds covering all of these individual adjustments.
             */
            if (getValueIsAdjusting()) {
                firstChangedIndex = Math.min(firstChangedIndex, firstAdjustedIndex);
                lastChangedIndex = Math.max(lastChangedIndex, lastAdjustedIndex);
            }
            /* Change the values before sending the event to the
             * listeners in case the event causes a listener to make
             * another change to the selection.
             */
            int oldFirstAdjustedIndex = firstAdjustedIndex;
            int oldLastAdjustedIndex = lastAdjustedIndex;
            firstAdjustedIndex = MAX;
            lastAdjustedIndex = MIN;

            fireValueChanged(oldFirstAdjustedIndex, oldLastAdjustedIndex);
        }

        /**
         * Returns an array of all the objects currently registered as
         * <code><em>Foo</em>Listener</code>s
         * upon this model.
         * <code><em>Foo</em>Listener</code>s
         * are registered using the <code>add<em>Foo</em>Listener</code> method.
         * <p>
         * You can specify the {@code listenerType} argument
         * with a class literal, such as <code><em>Foo</em>Listener.class</code>.
         * For example, you can query a {@code DefaultListSelectionModel}
         * instance {@code m}
         * for its list selection listeners
         * with the following code:
         *
         * <pre>ListSelectionListener[] lsls = (ListSelectionListener[])(m.getListeners(ListSelectionListener.class));</pre>
         *
         * If no such listeners exist,
         * this method returns an empty array.
         *
         * @param listenerType  the type of listeners requested;
         *          this parameter should specify an interface
         *          that descends from {@code java.util.EventListener}
         * @return an array of all objects registered as
         *          <code><em>Foo</em>Listener</code>s
         *          on this model,
         *          or an empty array if no such
         *          listeners have been added
         * @exception ClassCastException if {@code listenerType} doesn't
         *          specify a class or interface that implements
         *          {@code java.util.EventListener}
         *
         * @see #getListSelectionListeners
         *
         * @since 1.3
         */
        public EventListener[] getListeners(Class listenerType) {
            return listenerList.getListeners(listenerType);
        }

        // Updates first and last change indices
        private void markAsDirty(int r) {
            firstAdjustedIndex = Math.min(firstAdjustedIndex, r);
            lastAdjustedIndex = Math.max(lastAdjustedIndex, r);
        }

        // Sets the state at this index and update all relevant state.
        private void set(int r) {
            if (value.get(r)) {
                return;
            }
            value.set(r);
            markAsDirty(r);

            // Update minimum and maximum indices
            minIndex = Math.min(minIndex, r);
            maxIndex = Math.max(maxIndex, r);
        }

        // Clears the state at this index and update all relevant state.
        private void clear(int r) {
            if (!value.get(r)) {
                return;
            }
            value.clear(r);
            markAsDirty(r);

            // Update minimum and maximum indices
        /*
            If (r > minIndex) the minimum has not changed.
            The case (r < minIndex) is not possible because r'th value was set.
            We only need to check for the case when lowest entry has been cleared,
            and in this case we need to search for the first value set above it.
             */
            if (r == minIndex) {
                for (minIndex = minIndex + 1; minIndex <= maxIndex; minIndex++) {
                    if (value.get(minIndex)) {
                        break;
                    }
                }
            }
            /*
            If (r < maxIndex) the maximum has not changed.
            The case (r > maxIndex) is not possible because r'th value was set.
            We only need to check for the case when highest entry has been cleared,
            and in this case we need to search for the first value set below it.
             */
            if (r == maxIndex) {
                for (maxIndex = maxIndex - 1; minIndex <= maxIndex; maxIndex--) {
                    if (value.get(maxIndex)) {
                        break;
                    }
                }
            }
            /* Performance note: This method is called from inside a loop in
            changeSelection() but we will only iterate in the loops
            above on the basis of one iteration per deselected cell - in total.
            Ie. the next time this method is called the work of the previous
            deselection will not be repeated.
            We also don't need to worry about the case when the min and max
            values are in their unassigned states. This cannot happen because
            this method's initial check ensures that the selection was not empty
            and therefore that the minIndex and maxIndex had 'real' values.
            If we have cleared the whole selection, set the minIndex and maxIndex
            to their cannonical values so that the next set command always works
            just by using Math.min and Math.max.
             */
            if (isSelectionEmpty()) {
                minIndex = MAX;
                maxIndex = MIN;
            }
        }

        /**
         * Sets the value of the leadAnchorNotificationEnabled flag.
         * @see  #isLeadAnchorNotificationEnabled()
         */
        public void setLeadAnchorNotificationEnabled(boolean flag) {
            leadAnchorNotificationEnabled = flag;
        }

        /**
         * Returns the value of the {@code leadAnchorNotificationEnabled} flag.
         * When {@code leadAnchorNotificationEnabled} is true the model
         * generates notification events with bounds that cover all the changes to
         * the selection plus the changes to the lead and anchor indices.
         * Setting the flag to false causes a narrowing of the event's bounds to
         * include only the elements that have been selected or deselected since
         * the last change. Either way, the model continues to maintain the lead
         * and anchor variables internally. The default is true.
         * @return  the value of the {@code leadAnchorNotificationEnabled} flag
         * @see  #setLeadAnchorNotificationEnabled(boolean)
         */
        public boolean isLeadAnchorNotificationEnabled() {
            return leadAnchorNotificationEnabled;
        }

        private void updateLeadAnchorIndices(int anchorIndex, int leadIndex) {
            if (leadAnchorNotificationEnabled) {
                if (this.anchorIndex != anchorIndex) {
                    if (this.anchorIndex != -1) { // The unassigned state.
                        markAsDirty(this.anchorIndex);
                    }
                    markAsDirty(anchorIndex);
                }

                if (this.leadIndex != leadIndex) {
                    if (this.leadIndex != -1) { // The unassigned state.
                        markAsDirty(this.leadIndex);
                    }
                    markAsDirty(leadIndex);
                }
            }
            this.anchorIndex = anchorIndex;
            this.leadIndex = leadIndex;
        }

        private boolean contains(int a, int b, int i) {
            return (i >= a) && (i <= b);
        }

        private void changeSelection(int clearMin, int clearMax,
                                     int setMin, int setMax, boolean clearFirst) {
            for (int i = Math.min(setMin, clearMin); i <= Math.max(setMax, clearMax); i++) {

                boolean shouldClear = contains(clearMin, clearMax, i);
                boolean shouldSet = contains(setMin, setMax, i);

                if (shouldSet && shouldClear) {
                    if (clearFirst) {
                        shouldClear = false;
                    } else {
                        shouldSet = false;
                    }
                }

                if (shouldSet) {
                    set(i);
                }
                if (shouldClear) {
                    clear(i);
                }
            }
            fireValueChanged();
        }

        /**   Change the selection with the effect of first clearing the values
         *   in the inclusive range [clearMin, clearMax] then setting the values
         *   in the inclusive range [setMin, setMax]. Do this in one pass so
         *   that no values are cleared if they would later be set.
         */
        private void changeSelection(int clearMin, int clearMax, int setMin, int setMax) {
            changeSelection(clearMin, clearMax, setMin, setMax, true);
        }

        // implements javax.swing.ListSelectionModel
        public void clearSelection() {
            removeSelectionInterval(minIndex, maxIndex);
        }

        // implements javax.swing.ListSelectionModel
        public void setSelectionInterval(int index0, int index1) {
            if (index0 == -1 || index1 == -1) {
                return;
            }

            if (getSelectionMode() == SINGLE_SELECTION) {
                index0 = index1;
            }

            updateLeadAnchorIndices(index0, index1);

            int clearMin = minIndex;
            int clearMax = maxIndex;
            int setMin = Math.min(index0, index1);
            int setMax = Math.max(index0, index1);

            changeSelection(clearMin, clearMax, setMin, setMax);
        }

        // implements javax.swing.ListSelectionModel
        public void addSelectionInterval(int index0, int index1) {
            if (index0 == -1 || index1 == -1) {
                return;
            }

            if (getSelectionMode() != MULTIPLE_INTERVAL_SELECTION) {
                setSelectionInterval(index0, index1);
                return;
            }

            updateLeadAnchorIndices(index0, index1);

            int clearMin = MAX;
            int clearMax = MIN;
            int setMin = Math.min(index0, index1);
            int setMax = Math.max(index0, index1);

            changeSelection(clearMin, clearMax, setMin, setMax);
        }

        // implements javax.swing.ListSelectionModel
        public void removeSelectionInterval(int index0, int index1) {
            if (index0 == -1 || index1 == -1) {
                return;
            }

            updateLeadAnchorIndices(index0, index1);

            int clearMin = Math.min(index0, index1);
            int clearMax = Math.max(index0, index1);
            int setMin = MAX;
            int setMax = MIN;

            // If the removal would produce to two disjoint selections in a mode
            // that only allows one, extend the removal to the end of the selection.
            if (getSelectionMode() != MULTIPLE_INTERVAL_SELECTION
                    && clearMin > minIndex && clearMax < maxIndex) {
                clearMax = maxIndex;
            }

            changeSelection(clearMin, clearMax, setMin, setMax);
        }

        private void setState(int index, boolean state) {
            if (state) {
                set(index);
            } else {
                clear(index);
            }
        }

        /**
         * THIS METHOD DIFFERS FROM DefaultListSelectionModel:
         * <p>
         * Insert length indices beginning before/after index. Even the value
         * at index is itself selected all of the newly inserted items
         * are marked as unselected.
         * This method is typically called to sync the selection model with a
         * corresponding change in the data model.
         */
        public void insertIndexInterval(int index, int length, boolean before) {
            /* The first new index will appear at insMinIndex and the last
             * one will appear at insMaxIndex
             */
            int insMinIndex = (before) ? index : index + 1;
            int insMaxIndex = (insMinIndex + length) - 1;

            /* Right shift the entire bitset by length, beginning with
             * index-1 if before is true, index+1 if it's false (i.e. with
             * insMinIndex).
             */
            for (int i = maxIndex; i >= insMinIndex; i--) {
                setState(i + length, value.get(i));
            }


            /* Initialize the newly inserted indices.
             */
            // This is the only difference to DefaultListSelectionModel:
            // We _never_ select newly inserted values.
            boolean setInsertedValues = false;
            for (int i = insMinIndex; i <= insMaxIndex; i++) {
                setState(i, setInsertedValues);
            }

            // Move the leadIndex
            if (index <= leadIndex) {
                leadIndex += length;
            }


            fireValueChanged();
        }

        /**
         * THIS METHOD DIFFERS FROM DefaultListSelectionModel:
         * <p>
         * Remove the indices in the interval index0,index1 (inclusive) from
         * the selection model.  This is typically called to sync the selection
         * model width a corresponding change in the data model.  Note
         * that (as always) index0 need not be <= index1.
         */
        public void removeIndexInterval(int index0, int index1) {
            int rmMinIndex = Math.min(index0, index1);
            int rmMaxIndex = Math.max(index0, index1);
            int gapLength = (rmMaxIndex - rmMinIndex) + 1;



            /* Shift the entire bitset to the left to close the index0, index1
             * gap.
             */
            for (int i = rmMinIndex; i <= maxIndex; i++) {
                setState(i, value.get(i + gapLength));
            }

            /* Shift the lead index */
            if (leadIndex >= index1) {
                leadIndex = leadIndex - (index1 - index0) - 1;
            } else if (leadIndex >= index0) {
                leadIndex = index0;
            }

            fireValueChanged();
        }

        // implements javax.swing.ListSelectionModel
        public void setValueIsAdjusting(boolean isAdjusting) {
            if (isAdjusting != this.isAdjusting) {
                this.isAdjusting = isAdjusting;
                this.fireValueChanged(isAdjusting);
            }
        }

        /**
         * Returns a string that displays and identifies this
         * object's properties.
         *
         * @return a {@code String} representation of this object
         */
        @Override
        public String toString() {
            String s = ((getValueIsAdjusting()) ? "~" : "=") + value.toString();
            return getClass().getName() + " " + Integer.toString(hashCode()) + " " + s;
        }

        /**
         * Returns a clone of this selection model with the same selection.
         * {@code listenerLists} are not duplicated.
         *
         * @exception CloneNotSupportedException if the selection model does not
         *    both (a) implement the Cloneable interface and (b) define a
         *    {@code clone} method.
         */
        @Override
        public Object clone() throws CloneNotSupportedException {
            ColumnSelectionModel clone = (ColumnSelectionModel) super.clone();
            clone.value = (BitSet) value.clone();
            clone.listenerList = new EventListenerList();
            return clone;
        }

        // implements javax.swing.ListSelectionModel
        public int getAnchorSelectionIndex() {
            return anchorIndex;
        }

        // implements javax.swing.ListSelectionModel
        public int getLeadSelectionIndex() {
            return leadIndex;
        }

        /**
         * Set the anchor selection index, leaving all selection values unchanged.
         * If leadAnchorNotificationEnabled is true, send a notification covering
         * the old and new anchor cells.
         *
         * @see #getAnchorSelectionIndex
         * @see #setLeadSelectionIndex
         */
        public void setAnchorSelectionIndex(int anchorIndex) {
            updateLeadAnchorIndices(anchorIndex, this.leadIndex);
            this.anchorIndex = anchorIndex;
            fireValueChanged();
        }

        /**
         * Sets the lead selection index, ensuring that values between the
         * anchor and the new lead are either all selected or all deselected.
         * If the value at the anchor index is selected, first clear all the
         * values in the range [anchor, oldLeadIndex], then select all the values
         * values in the range [anchor, newLeadIndex], where oldLeadIndex is the old
         * leadIndex and newLeadIndex is the new one.
         * <p>
         * If the value at the anchor index is not selected, do the same thing in
         * reverse selecting values in the old range and deslecting values in the
         * new one.
         * <p>
         * Generate a single event for this change and notify all listeners.
         * For the purposes of generating minimal bounds in this event, do the
         * operation in a single pass; that way the first and last index inside the
         * ListSelectionEvent that is broadcast will refer to cells that actually
         * changed value because of this method. If, instead, this operation were
         * done in two steps the effect on the selection state would be the same
         * but two events would be generated and the bounds around the changed
         * values would be wider, including cells that had been first cleared only
         * to later be set.
         * <p>
         * This method can be used in the {@code mouseDragged} method
         * of a UI class to extend a selection.
         *
         * @see #getLeadSelectionIndex
         * @see #setAnchorSelectionIndex
         */
        public void setLeadSelectionIndex(int leadIndex) {
            int newAnchorIndex = this.anchorIndex;

            if ((newAnchorIndex == -1) || (leadIndex == -1)) {
                return;
            }

            if (this.leadIndex == -1) {
                this.leadIndex = leadIndex;
            }

            boolean shouldSelect = value.get(this.anchorIndex);

            if (getSelectionMode() == SINGLE_SELECTION) {
                newAnchorIndex = leadIndex;
                shouldSelect = true;
            }

            int oldMin = Math.min(this.anchorIndex, this.leadIndex);
            int oldMax = Math.max(this.anchorIndex, this.leadIndex);
            int newMin = Math.min(newAnchorIndex, leadIndex);
            int newMax = Math.max(newAnchorIndex, leadIndex);

            updateLeadAnchorIndices(newAnchorIndex, leadIndex);

            if (shouldSelect) {
                changeSelection(oldMin, oldMax, newMin, newMax);
            } else {
                changeSelection(newMin, newMax, oldMin, oldMax, false);
            }
        }
    } // End of class ColumnSelectionModel.

    /**
     * Handles changes of the tree root.
     */
    private class TreeRootHandler implements TreeModelListener {

        public void treeNodesChanged(TreeModelEvent e) {
        }

        public void treeNodesInserted(TreeModelEvent e) {
        }

        public void treeNodesRemoved(TreeModelEvent e) {
        }

        public void treeStructureChanged(TreeModelEvent e) {
            // Detect if the whole tree structure has changed.
            if (e.getPath().length == 1) {

                // Remove all columns
                for (int i = getListColumnCount() - 1; i >= 0; i--) {
                    removeLastListColumn();
                }
                expandedPath = null;
                expandRoot();
                setSelectionPath(e.getTreePath());
            }
        }
    } // End of class TreeRootHandler

    /**
     * Wraps a BrowserCellRenderer into a ListCellRenderer for use in a JBrowser.
     *
     * @author Werner Randelshofer
     * @version 1.0 Mar 22, 2008 Created.
     */
    private class BrowserCellRendererWrapper extends JPanel implements ListCellRenderer, Accessible {

        private BrowserCellRenderer browserCellRenderer;
        private Component browserCellRendererComponent;
        private JLabel arrowLabel;
        protected Icon expandedIcon = null;
        protected Icon selectedExpandedIcon = null;
        protected Icon focusedSelectedExpandedIcon = null;

        public BrowserCellRendererWrapper(BrowserCellRenderer treeCellRenderer) {
            this.browserCellRenderer = treeCellRenderer;

            expandedIcon = UIManager.getIcon("Browser.expandedIcon");
            selectedExpandedIcon = UIManager.getIcon("Browser.selectedExpandedIcon");
            focusedSelectedExpandedIcon = UIManager.getIcon("Browser.focusedSelectedExpandedIcon");

            setLayout(new BorderLayout());

            arrowLabel = new ArrowLabel();

            setOpaque(true);

            arrowLabel.putClientProperty("Quaqua.Component.visualMargin",
                    new Insets(0, 0, 0, 0));

            arrowLabel.setIcon(expandedIcon);
        }

        protected class ArrowLabel extends JLabel {

            // Overridden for performance reasons.

            @Override
            public void validate() {
            }

            @Override
            public void revalidate() {
            }

            @Override
            public void repaint(
                    long tm, int x, int y, int width, int height) {
            }

            @Override
            public void repaint(Rectangle r) {
            }

            @Override
            protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
                if (propertyName != null && propertyName.equals("text")) {
                    super.firePropertyChange(propertyName, oldValue, newValue);
                }
            }

            @Override
            public void firePropertyChange(String propertyName, short oldValue, short newValue) {
            }

            @Override
            public void firePropertyChange(String propertyName, int oldValue, int newValue) {
            }

            @Override
            public void firePropertyChange(String propertyName, long oldValue, long newValue) {
            }

            @Override
            public void firePropertyChange(String propertyName, float oldValue, float newValue) {
            }

            @Override
            public void firePropertyChange(String propertyName, double oldValue, double newValue) {
            }

            @Override
            public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {
            }
        }

        public Component getListCellRendererComponent(JList list, Object value,
                                                      int index, boolean isSelected,
                                                      boolean cellHasFocus) {
            //setComponentOrientation(list.getComponentOrientation());
            boolean isFocused = AquaFocusHandler.hasFocus(list);
            boolean isLeaf = getModel().isLeaf(value);

            if (isSelected) {
                setBackground(list.getSelectionBackground());
                arrowLabel.setForeground(list.getSelectionForeground());
                arrowLabel.setIcon(isFocused ? focusedSelectedExpandedIcon : selectedExpandedIcon);
            } else {
                setBackground(AquaColors.CLEAR);
                arrowLabel.setForeground(list.getForeground());
                arrowLabel.setIcon(expandedIcon);
            }

            arrowLabel.setVisible(!getModel().isLeaf(value));

            boolean isExpanded = false;
            for (int i = 0, n = expandedPath.getPathCount(); i < n; i++) {
                if (expandedPath.getPathComponent(i) == value) {
                    isExpanded = true;
                    break;
                }
            }

            browserCellRendererComponent = browserCellRenderer.getBrowserCellRendererComponent(
                    JBrowser.this, value, isSelected, isExpanded, isLeaf, index,
                    isFocused && list.getLeadSelectionIndex() == index);

            removeAll();
            add(browserCellRendererComponent, BorderLayout.CENTER);
            add(arrowLabel, BorderLayout.EAST);

            // Get border. Handle Look and feels which don't specify a border.
            Border border = UIManager.getBorder((cellHasFocus) ? "List.focusCellHighlightBorder" : "List.cellNoFocusBorder");
            if (border == null) {
                border = DEFAULT_NO_FOCUS_BORDER;
            }
            setBorder(border);

            return this;
        }

        @Override
        public void setFont(Font newValue) {
            super.setFont(newValue);
            if (browserCellRendererComponent != null) {
                browserCellRendererComponent.setFont(newValue);
            }
        }
        // Overridden for performance reasons.
        //public void validate() {}

        @Override
        public void revalidate() {
        }

        @Override
        public void repaint(long tm, int x, int y, int width, int height) {
        }

        @Override
        public void repaint(Rectangle r) {
        }

        @Override
        protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, short oldValue, short newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, int oldValue, int newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, long oldValue, long newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, float oldValue, float newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, double oldValue, double newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {
        }
    }

    protected ListUI getColumnListUI(ListUI basicUI) {
        return basicUI;
    }

    protected abstract class Column extends JPanel {
        private JScrollPane scrollPane;
        private Component sizeHandle;
        private int columnIndex;

        protected Column(@Nullable JComponent c, int columnIndex) {
            this.columnIndex = columnIndex;
            setLayout(new BorderLayout());

            if (columnIndex >= 0 || isPreviewColumnScrollable()) {
                scrollPane = createScrollPane(c);
                add(scrollPane);
                configureFromScrollBars();
                scrollPane.addPropertyChangeListener(new PropertyChangeListener() {
                    @Override
                    public void propertyChange(PropertyChangeEvent evt) {
                        if (AquaScrollPaneUI.SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY.equals(evt.getPropertyName())) {
                            configureFromScrollBars();
                        }
                    }
                });
            } else if (c != null) {
                add(c);
            }
        }

        protected void configureFromScrollBars() {
            // The configuration is based on whether or not overlay scroll bars are being used
            boolean isOverlayScrollBars = Boolean.TRUE.equals(
                    scrollPane.getClientProperty(AquaScrollPaneUI.SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY));

            if (sizeHandle != null) {
                remove(sizeHandle);
                scrollPane.setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, null);
            }

            if (isOverlayScrollBars) {
                sizeHandle = new BorderSizeHandle(columnIndex);
                add(sizeHandle, BorderLayout.EAST);
            } else {
                sizeHandle = new IconSizeHandle(columnIndex);
                sizeHandle.setVisible(isColumnsResizable());
                scrollPane.setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, sizeHandle);
            }
        }

        protected Component getView() {
            return scrollPane != null ? scrollPane.getViewport().getView() : getComponent(0);
        }

        public void setResizable(boolean b) {
            if (sizeHandle != null) {
                sizeHandle.setVisible(b);
            }
        }
    }

    protected class ListColumn extends Column {
        public ListColumn(@NotNull JList list, int columnIndex) {
            super(list, columnIndex);
        }

        public @NotNull JList getList() {
            try {
                return (JList) getView();
            } catch (ClassCastException ex) {
                throw new AssertionError("Unexpected exception", ex);
            }
        }

        @Override
        public @NotNull Dimension getMinimumSize() {
            int width = getColumnMinimumWidth();
            int height = getColumnMinimumHeight();
            return new Dimension(width, height);
        }

        @Override
        public Dimension getPreferredSize() {
            Dimension size = super.getPreferredSize();
            int width = Math.max(size.width, getColumnMinimumWidth());
            int height = Math.max(size.height, getColumnMinimumHeight());
            return new Dimension(width, height);
        }

        public int getColumnPreferredWidth() {
            // TBD: should leave room for a scroll bar
            JList list = getList();
            int fixedCellWidth = list.getFixedCellWidth();
            list.setFixedCellWidth(-1);
            int width = list.getPreferredSize().width;
            list.setFixedCellWidth(fixedCellWidth);
            return width;
        }

        public int getFixedCellWidth() {
            JList list = getList();
            return list.getFixedCellWidth();
        }

        public void setFixedCellWidth(int width) {
            JList list = getList();
            list.setFixedCellWidth(width);
            revalidate();
            repaint();
        }
    }

    protected class PreviewColumn extends Column {
        private @NotNull BrowserPreviewRenderer renderer;
        int designatedWidth;
        @Nullable Component currentComponent;

        public PreviewColumn(@NotNull BrowserPreviewRenderer renderer) {
            super(null, -1);
            this.renderer = renderer;
        }

        public @NotNull BrowserPreviewRenderer getRenderer() {
            return renderer;
        }

        public void setRenderer(@NotNull BrowserPreviewRenderer renderer) {
            if (renderer != this.renderer) {
                this.renderer = renderer;
            }
        }

        @Override
        public @NotNull Dimension getMinimumSize() {
            if (getComponentCount() > 0) {
                Dimension d = super.getMinimumSize();
                return new Dimension(d.width, Math.max(d.height, columnMinimumHeight));
            } else {
                return new Dimension(getFixedCellWidth(), columnMinimumHeight);
            }
        }

        @Override
        public @NotNull Dimension getPreferredSize() {
            Dimension preferredSize = super.getPreferredSize();
            int width = Math.max(designatedWidth, preferredSize.width);
            int height = Math.max(columnMinimumHeight, preferredSize.height);
            return new Dimension(width, height);
        }

        public void setDesignatedWidth(int w) {
            if (w > 0) {
                Dimension minimumSize = getMinimumSize();
                int minimumWidth = minimumSize.width;
                designatedWidth = Math.max(minimumWidth, w);
            } else {
                designatedWidth = 0;
            }
            revalidate();
            repaint();
        }

        public void updateSelection(@NotNull TreePath[] paths) {
            Component c = renderer.getPreviewRendererComponent(JBrowser.this, paths);
            if (c != currentComponent) {
                removeAll();
                add(c);
                currentComponent = c;
            }
            if (designatedWidth > 0) {
                Dimension minimumSize = c.getMinimumSize();
                int minimumWidth = minimumSize.width;
                if (minimumWidth > designatedWidth) {
                    designatedWidth = minimumWidth;
                }
            }
            revalidate();
            repaint();
        }
    }

    protected JScrollPane createScrollPane(@Nullable JComponent c) {
        return new JBrowserScrollPane(c);
    }

    protected class JBrowserScrollPane extends JScrollPane {
        public JBrowserScrollPane(@Nullable JComponent c) {
            super(c, VERTICAL_SCROLLBAR_ALWAYS, HORIZONTAL_SCROLLBAR_NEVER);
            setBorder(null);
            setFocusable(false);
            getVerticalScrollBar().setFocusable(false);
            getHorizontalScrollBar().setFocusable(false);
            setLayout(new BrowserScrollPaneLayout());
        }
    }

    protected abstract class SizeHandleBase extends JComponent implements MouseListener, MouseMotionListener {

        protected int column;
        protected int startMouseX;
        protected int startWidth;

        public SizeHandleBase(int column) {
            this.column = column;
            addMouseListener(this);
            addMouseMotionListener(this);
            setCursor(Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR));
        }

        public void mouseDragged(MouseEvent e) {
            if (startMouseX >= 0) {
                int mouseX = toScreenX(e.getPoint());
                int difX = mouseX - startMouseX;
                if (column < 0) {
                    setPreviewColumnWidth(Math.max(startWidth + difX, 0));
                } else {
                    setColumnWidth(column, Math.max(startWidth + difX, getColumnMinimumWidth()));
                }
            }
        }

        public void mouseMoved(MouseEvent e) {
        }

        public void mouseClicked(MouseEvent e) {
        }

        public void mouseEntered(MouseEvent e) {
        }

        public void mouseExited(MouseEvent e) {
        }

        public void mousePressed(MouseEvent e) {
            startMouseX = toScreenX(e.getPoint());
            if (column < 0) {
                startWidth = getPreviewColumnWidth();
            } else {
                startWidth = getColumnWidth(column);
            }
        }

        public void mouseReleased(MouseEvent e) {
            if (startMouseX >= 0) {
                int mouseX = toScreenX(e.getPoint());
                int difX = mouseX - startMouseX;
                if (column < 0) {
                    setPreviewColumnWidth(Math.max(startWidth + difX, 0));
                } else {
                    setColumnWidth(column, Math.max(startWidth + difX, getColumnMinimumWidth()));
                }
            }

            startMouseX = -1;
        }

        protected int toScreenX(Point p) {
            // We need to convert to screen coordinates as the SizeHandle is moved during the drag
            SwingUtilities.convertPointToScreen(p, this);
            return p.x;
        }
    }

    protected class IconSizeHandle extends SizeHandleBase {

        private int width = 15;
        private int height = 15;

        public IconSizeHandle(int column) {
            super(column);
        }

        @Override
        public Dimension getPreferredSize() {
            return new Dimension(width, height);
        }

        @Override
        public void paintComponent(Graphics g) {
            AquaAppearance appearance = AppearanceManager.ensureAppearance(this);
            Color backgroundColor = appearance.getColor("legacyScrollBarTrack");
            Color innerBorderColor = appearance.getColor("legacyScrollBarInnerBorder");
            Color outerBorderColor = appearance.getColor("legacyScrollBarOuterBorder");
            Color grabberColor = appearance.getColor("scrollPaneGrabber");

            g.setColor(backgroundColor);
            g.fillRect(0, 0, width, height);
            if (innerBorderColor != null) {
                g.setColor(innerBorderColor);
                g.fillRect(0, 0, 1, height);
            }
            if (outerBorderColor != null) {
                g.setColor(outerBorderColor);
                g.fillRect(width - 1, 0, 1, height);
            }
            g.setColor(grabberColor);
            g.fillRect(5, 4, 1, 7);
            g.fillRect(9, 4, 1, 7);
        }

        @Override
        public void mouseClicked(MouseEvent e) {
            if (e.getClickCount() == 2 && column >= 0) {
                setColumnWidth(column, Math.max(getColumnMinimumWidth(), getColumnPreferredWidth(column)));
            }
        }
    }

    protected class BorderSizeHandle extends SizeHandleBase {

        public BorderSizeHandle(int column) {
            super(column);
        }

        @Override
        protected void paintComponent(Graphics g) {
            AquaAppearance appearance = AppearanceManager.ensureAppearance(this);
            Color background = appearance.getColor("controlBackground");
            Color handleColor = appearance.getColor("separator");
            g.setColor(background);
            g.fillRect(0, 0, getWidth(), getHeight());
            g.setColor(handleColor);
            g.fillRect(getWidth()-1, 0, 1, getHeight());
        }

        @Override
        public Dimension getMaximumSize() {
            return new Dimension(1, 100000);
        }

        @Override
        public Dimension getPreferredSize() {
            return new Dimension(1, 10);
        }
    }
}
