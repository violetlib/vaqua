/*
 * Changes Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.dnd.*;
import java.awt.event.*;
import java.beans.*;
import java.util.Enumeration;
import java.util.TooManyListenersException;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.MouseInputAdapter;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.*;

import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonState;
import org.violetlib.jnr.aqua.AquaUIPainter.UILayoutDirection;

/**
 * A tree UI based on AquaTreeUI for Yosemite. It supports filled cells. It supports the striped and sidebar styles.
 * It supports a more compatible mouse behavior. It displays using an inactive style based on focus ownership. It
 * configures cell renderers, as possible, to conform to the tree style.
 * AquaTreeUI supports the client property "value-add" system of customization See MetalTreeUI
 * This is heavily based on the 1.3.1 AquaTreeUI implementation.
 */
public class AquaTreeUI extends BasicTreeUI implements SelectionRepaintable {

    public static ComponentUI createUI(final JComponent c) {
        return new AquaTreeUI();
    }

    public static final String IS_CELL_FILLED_KEY = "JTree.isCellFilled";
    public static final String QUAQUA_IS_CELL_FILLED_KEY = "Quaqua.Tree.isCellFilled";

    public static final String TREE_STYLE_KEY = "JTree.style";
    public static final String QUAQUA_TREE_STYLE_KEY = "Quaqua.Tree.style";

    protected static final Color TRANSPARENT_COLOR = new Color(0, true);

    // Begin Line Stuff from Metal

    private static final String LINE_STYLE = "JTree.lineStyle";

    private static final String LEG_LINE_STYLE_STRING = "Angled";
    private static final String HORIZ_STYLE_STRING = "Horizontal";
    private static final String NO_STYLE_STRING = "None";

    private static final int LEG_LINE_STYLE = 2;
    private static final int HORIZ_LINE_STYLE = 1;
    private static final int NO_LINE_STYLE = 0;

    private int lineStyle = NO_LINE_STYLE;
    private final Color[] stripes;

    private final PropertyChangeListener lineStyleListener = new LineListener();
    private final FocusListener editingComponentFocusListener = new EditingComponentFocusListener();
    private Component editorFocusOwner;

    // mouse tracking state
    protected TreePath fTrackingPath;
    protected boolean fIsPressed = false;
    protected boolean fIsInBounds = false;
    protected float fAnimationTransition = -1;
    protected TreeArrowMouseInputHandler fMouseHandler;

    // Duplicates a private instance variable of BasicTreeUI
    private boolean ignoreLAChange;

    protected final AquaUIPainter painter = AquaPainting.create();

    protected boolean isCellFilled;
    protected boolean isSideBar;
    protected boolean isStriped;
    protected boolean shouldPaintSelection;

    protected boolean isActive;     // for communication between paint() and paintRow()
    protected boolean isFocused;    // ditto
    protected Color foreground;
    protected Color selectionBackground;
    protected Color selectionForeground;

    // state variables needed for cell renderer configuration
    private Font oldCellRendererFont;
    private Icon oldCellRendererIcon;
    private Icon oldCellRendererDisabledIcon;

    private static DropTargetListener defaultDropTargetListener = null;

    public AquaTreeUI() {
        stripes = new Color[] {UIManager.getColor("Tree.evenRowBackground"), UIManager.getColor("Tree.oddRowBackground")};
    }

    public boolean isSideBar() {
        return isSideBar;
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);

        final Object lineStyleFlag = c.getClientProperty(LINE_STYLE);
        decodeLineStyle(lineStyleFlag);
        c.addPropertyChangeListener(lineStyleListener);
    }

    @Override
    protected void completeUIInstall() {
        updateProperties();
        super.completeUIInstall();
    }

    @Override
    public void uninstallUI(final JComponent c) {
        c.removePropertyChangeListener(lineStyleListener);
        super.uninstallUI(c);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();

        // Unfortunately, there is no reliable way to undo this change because we will not know if the application
        // later tries to set it to false. Fortunately, swapping LAFs is unusual and displaying a root node is dumb.
        tree.setRootVisible(false);
        // Too bad this does not work:
        // LookAndFeel.installProperty(tree, JTree.ROOT_VISIBLE_PROPERTY, false);
        tree.putClientProperty(AquaCellEditorPolicy.IS_CELL_CONTAINER_PROPERTY, true);
    }

    @Override
    protected void uninstallDefaults() {
       super.uninstallDefaults();
    }

    protected void updateProperties() {
        if (tree != null) {
            isCellFilled = Boolean.TRUE.equals(AquaUtils.getBooleanProperty(tree, IS_CELL_FILLED_KEY, QUAQUA_IS_CELL_FILLED_KEY));
            String style = getStyleProperty();
            isSideBar = style != null && (style.equals("sideBar") || style.equals("sourceList"));
            isStriped = style != null && style.equals("striped");
        }
    }

    protected boolean isCellFilledProperty(String prop) {
        return AquaUtils.isProperty(prop, IS_CELL_FILLED_KEY, QUAQUA_IS_CELL_FILLED_KEY);
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, TREE_STYLE_KEY, QUAQUA_TREE_STYLE_KEY);
    }

    protected String getStyleProperty() {
        return AquaUtils.getProperty(tree, TREE_STYLE_KEY, QUAQUA_TREE_STYLE_KEY);
    }

    @Override
    protected MouseListener createMouseListener() {
        return new AquaTreeMouseBehavior(tree);
    }

    /**
     * Creates the focus listener to repaint the selection
     */
    @Override
    protected FocusListener createFocusListener() {
        return new AquaTreeUI.FocusHandler();
    }

    @Override
    protected PropertyChangeListener createPropertyChangeListener() {
        return new MacPropertyChangeHandler();
    }

    // This method made public so that AquaTreeMouseBehavior can access it.
    @Override
    public boolean startEditing(TreePath path, MouseEvent event) {
        boolean isEditing = super.startEditing(path, event);
        if (isEditing) {
            editorFocusOwner = AquaFocusHandler.getFocusableComponent(editingComponent);
            if (editorFocusOwner != null) {
                editorFocusOwner.addFocusListener(editingComponentFocusListener);
            }
        }
        return isEditing;
    }

    // This method made public so that AquaTreeMouseBehavior can access it.
    @Override
    public void completeEditing() {
        if (editorFocusOwner != null) {
            editorFocusOwner.removeFocusListener(editingComponentFocusListener);
        }
        super.completeEditing();
    }

    @Override
    public void cancelEditing(JTree tree) {
        if (editorFocusOwner != null) {
            editorFocusOwner.removeFocusListener(editingComponentFocusListener);
        }
        super.cancelEditing(tree);
    }

    // This method made public so that AquaTreeMouseBehavior can access it.
    @Override
    public boolean isLocationInExpandControl(TreePath path, int mouseX, int mouseY) {
        return super.isLocationInExpandControl(path, mouseX, mouseY);
    }

    // This method made public so that AquaTreeMouseBehavior can access it.
    @Override
    public void checkForClickInExpandControl(TreePath path, int mouseX, int mouseY) {
        super.checkForClickInExpandControl(path, mouseX, mouseY);
    }

    protected class EditingComponentFocusListener implements FocusListener {
        @Override
        public void focusGained(FocusEvent e) {

        }

        @Override
        public void focusLost(FocusEvent e) {
            AquaTreeUI.this.stopEditing(tree);
        }
    }

    //
    // The following selection methods (lead/anchor) are covers for the
    // methods in JTree.
    //
    public void setAnchorSelectionPath(TreePath newPath) {
        ignoreLAChange = true;
        try {
            tree.setAnchorSelectionPath(newPath);
        } finally {
            ignoreLAChange = false;
        }
    }

    public TreePath getAnchorSelectionPath() {
        return tree.getAnchorSelectionPath();
    }

    public void setLeadSelectionPath(TreePath newPath, boolean repaint) {
        Rectangle bounds = repaint ? getPathBounds(tree, getLeadSelectionPath()) : null;

        ignoreLAChange = true;
        try {
            tree.setLeadSelectionPath(newPath);
        } finally {
            ignoreLAChange = false;
        }

        if (repaint) {
            if (bounds != null) {
                tree.repaint(bounds);
            }
            bounds = getPathBounds(tree, newPath);
            if (bounds != null) {
                tree.repaint(bounds);
            }
        }
    }

    public TreePath getLeadSelectionPath() {
        return tree.getLeadSelectionPath();
    }

    public void setLeadSelectionPath(TreePath newPath) {
        setLeadSelectionPath(newPath, false);
    }

    @Override
    protected void setShowsRootHandles(boolean newValue) {
        super.setShowsRootHandles(true);
    }

    @Override
    protected boolean getShowsRootHandles() {
        return true;
    }

    /**
     * this function converts between the string passed into the client property and the internal representation
     * (currently an int)
     */
    protected void decodeLineStyle(final Object lineStyleFlag) {
        if (lineStyleFlag == null || NO_STYLE_STRING.equals(lineStyleFlag)) {
            lineStyle = NO_LINE_STYLE; // default case
            return;
        }

        if (LEG_LINE_STYLE_STRING.equals(lineStyleFlag)) {
            lineStyle = LEG_LINE_STYLE;
        } else if (HORIZ_STYLE_STRING.equals(lineStyleFlag)) {
            lineStyle = HORIZ_LINE_STYLE;
        }
    }

    public TreePath getClosestPathForLocation(final JTree treeLocal, final int x, final int y) {
        if (treeLocal == null || treeState == null) return null;

        Insets i = treeLocal.getInsets();
        if (i == null) i = new Insets(0, 0, 0, 0);
        return treeState.getPathClosestTo(x - i.left, y - i.top);
    }

    public void repaintSelection() {
        if (tree == null) {
            return;
        }

        // Support custom repainting, needed by TreeTable
        Object o = tree.getClientProperty("JTree.selectionRepainter");
        if (o instanceof SelectionRepaintable) {
            SelectionRepaintable sp = (SelectionRepaintable) o;
            sp.repaintSelection();
            return;
        }

        Rectangle pBounds = null;

        TreePath[] selectionPaths = tree.getSelectionPaths();
        if (selectionPaths != null) {
            for (int i = 0; i < selectionPaths.length; i++) {
                if (i == 0) {
                    pBounds = getPathBounds(tree, selectionPaths[i]);
                } else {
                    pBounds.add(getPathBounds(tree, selectionPaths[i]));
                }
            }
            if (pBounds != null) {
                tree.repaint(0, pBounds.y, tree.getWidth(), pBounds.height);
            }
        }
     }

    /**
     * A custom NodeDimensions that implements filled cells as well as special configuration of the renderer that might
     * affect node size.
     */
    protected class TreeNodeDimensions extends NodeDimensionsHandler {
        @Override
        public Rectangle getNodeDimensions(Object value, int row, int depth, boolean expanded, Rectangle size) {
            Rectangle r = getBasicNodeDimensions(value, row, depth, expanded, size);

            /*
             * Implement the filled cell option. Does not affect editing. (Should it?)
             */

            if (isCellFilled && r != null && (editingComponent == null || editingRow != row) && tree.getWidth() > 0) {
                Insets s = tree.getInsets();
                int width = tree.getWidth() - s.right - r.x;
                r.width = width;
            }

            return r;
        }

        // copied from BasicTreeUI with alterations to configure the renderer
        public Rectangle getBasicNodeDimensions(Object value, int row,
                                                int depth, boolean expanded,
                                                Rectangle size) {
            // Return size of editing component, if editing and asking for editing row.
            if(editingComponent != null && editingRow == row) {
                Dimension prefSize = editingComponent.getPreferredSize();
                int rh = getRowHeight();

                if(rh > 0 && rh != prefSize.height)
                    prefSize.height = rh;
                if(size != null) {
                    size.x = getRowX(row, depth);
                    size.width = prefSize.width;
                    size.height = prefSize.height;
                }
                else {
                    size = new Rectangle(getRowX(row, depth), 0, prefSize.width, prefSize.height);
                }
                return size;
            }
            // Not editing, use renderer.
            if(currentCellRenderer != null) {
                Component aComponent;

                /*
                 * A bit of a hack. Because the font may change to bold when selected and the bold font is probably
                 * larger, use the selected font when calculating the label size.
                 */

                boolean isSelected = true;

                aComponent = currentCellRenderer.getTreeCellRendererComponent(tree, value, isSelected,
                     expanded, treeModel.isLeaf(value), row, false);

                boolean isCategory = depth == 1;
                configureCellRenderer(true, aComponent, isCategory, row, isSelected);

                if(tree != null) {
                    // Only ever removed when UI changes, this is OK!
                    rendererPane.add(aComponent);
                    aComponent.validate();
                }
                Dimension prefSize = aComponent.getPreferredSize();

                unconfigureCellRenderer(aComponent);

                if(size != null) {
                    size.x = getRowX(row, depth);
                    size.width = prefSize.width;
                    size.height = prefSize.height;
                }
                else {
                    size = new Rectangle(getRowX(row, depth), 0, prefSize.width, prefSize.height);
                }
                return size;
            }
            return null;
        }
    }

    /**
     * Customize the X offset for the sidebar, to take into account that category rows have no icons.
     */
    @Override
    protected int getRowX(int row, int depth) {
        if (isSideBar) {
            if (depth > 1) {
                depth--;
            }
        }

        return totalChildIndent * (depth + depthOffset);
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {

        // Set the variables used by paintRow

        isActive = AquaFocusHandler.isActive(c);
        isFocused = shouldDisplayAsFocused(c);
        shouldPaintSelection = !Boolean.FALSE.equals(tree.getClientProperty("JTree.paintSelectionBackground"));

        if (isSideBar) {
            foreground = UIManager.getColor("Tree.sideBar.foreground");
            selectionForeground = UIManager.getColor(isFocused ? "Tree.sideBar.selectionForeground" : "Tree.sideBar.selectionInactiveForeground");
            selectionBackground = UIManager.getColor(isFocused ? "Tree.sideBar.selectionBackground" : "Tree.sideBar.selectionInactiveBackground");
        } else {
            foreground = UIManager.getColor("Tree.foreground");
            selectionForeground = UIManager.getColor(isFocused ? "Tree.selectionForeground" : "Tree.selectionInactiveForeground");
            selectionBackground = UIManager.getColor(isFocused ? "Tree.selectionBackground" : "Tree.selectionInactiveBackground");
        }

        paintBackground(g);

        super.paint(g, c);

        // Paint the lines
        if (lineStyle == HORIZ_LINE_STYLE && !largeModel) {
            paintHorizontalSeparators(g, c);
        }
    }

    protected void paintBackground(Graphics g) {
        Color background = getCurrentBackground();

        if (tree.isOpaque()) {
            int width = tree.getWidth();
            int height = tree.getHeight();
            g.setColor(background);
            g.fillRect(0, 0, width, height);
        }

        Rectangle paintBounds = g.getClipBounds();
        TreePath initialPath = getClosestPathForLocation(tree, 0, paintBounds.y);
        Enumeration paintingEnumerator = treeState.getVisiblePathsFrom(initialPath);
        if (initialPath != null && paintingEnumerator != null) {
            paintRowBackgrounds(g, initialPath, paintingEnumerator);
        } else if (isStriped) {
            paintEmptyTreeStripes(g);
        }
    }

    public Color getCurrentBackground() {
        if (isSideBar) {
            return UIManager.getColor(isActive ? "Tree.sideBar.background" : "Tree.sideBar.inactiveBackground");
        } else {
            return tree.getBackground();
        }
    }

    /**
     * Paint stripes (if appropriate) and selected row backgrounds
     */
    protected void paintRowBackgrounds(Graphics g, TreePath initialPath, Enumeration paintingEnumerator) {
        if (!isStriped && !shouldPaintSelection) {
            return;
        }

        int width = tree.getWidth();
        int height = tree.getHeight();
        Insets insets = tree.getInsets();

        int rwidth = width - insets.left - insets.left;
        int rheight = tree.getRowHeight();
        if (rheight <= 0) {
            // FIXME - Use the cell renderer to determine the height
            rheight = tree.getFont().getSize() + 4;
        }

        int row = treeState.getRowForPath(initialPath);
        Rectangle paintBounds = g.getClipBounds();
        int endY = paintBounds.y + paintBounds.height;

        while (paintingEnumerator.hasMoreElements()) {
            TreePath path = (TreePath) paintingEnumerator.nextElement();
            if (path == null) {
                break;
            }
            Rectangle bounds = getPathBounds(tree, path);
            if (bounds == null) {
                return; // should not happen
            }

            bounds.x += insets.left;
            bounds.y += insets.top;

            Color bc = getSpecialBackgroundForRow(row);
            if (bc != null) {
                g.setColor(bc);
                g.fillRect(insets.left, bounds.y, rwidth, bounds.height);
            }

            if ((bounds.y + bounds.height) >= endY) {
                break;
            }

            row++;
        }
    }

    protected Color getSpecialBackgroundForRow(int row) {
        if (tree.isRowSelected(row) && shouldPaintSelection) {
            return selectionBackground;
        }

        if (isStriped) {
            return stripes[row % 2];
        }

        return null;
    }

    protected boolean shouldDisplayAsFocused(Component c) {
        return tree.isEditing() || AquaFocusHandler.hasFocus(c);
    }

    /**
     * Paint stripes for an empty tree.
     */
    protected void paintEmptyTreeStripes(Graphics g) {
        int width = tree.getWidth();
        int height = tree.getHeight();
        Insets insets = tree.getInsets();

        int rwidth = width - insets.left - insets.left;
        int rheight = tree.getRowHeight();
        if (rheight <= 0) {
            // FIXME - Use the cell renderer to determine the height
            rheight = tree.getFont().getSize() + 4;
        }

        Color[] stripes = {UIManager.getColor("Tree.evenRowBackground"), UIManager.getColor("Tree.oddRowBackground")};

        int row = 0;
        for (int y = 0; y < height; y += rheight) {
            g.setColor(stripes[row % 2]);
            g.fillRect(insets.left, y, rwidth, rheight);
            row++;
        }
    }

    /**
     * Customized row painting for sidebar trees. Various client properties are set. An attempt is made to prevent the
     * tree cell renderer from drawing a background or a border or (in the case of a top level node) an icon.
     */
    @Override
    protected void paintRow(Graphics g, Rectangle clipBounds, Insets insets, Rectangle bounds, TreePath path, int row, boolean isExpanded, boolean hasBeenExpanded, boolean isLeaf) {

        if(editingComponent != null && editingRow == row)
            return;

        int leadIndex;

        if(tree.hasFocus()) {
            leadIndex = getLeadSelectionRow();
        }
        else
            leadIndex = -1;

        Component component;

        component = currentCellRenderer.getTreeCellRendererComponent
                      (tree, path.getLastPathComponent(),
                       tree.isRowSelected(row), isExpanded, isLeaf, row,
                       (leadIndex == row));

        boolean isCategory = path.getPathCount() == 2;
        boolean isRowSelected = tree.isRowSelected(row);
        configureCellRenderer(false, component, isCategory, row, isRowSelected);
        rendererPane.paintComponent(g, component, tree, bounds.x, bounds.y, bounds.width, bounds.height, true);
        unconfigureCellRenderer(component);
    }

    /**
     * Configure the cell renderer for layout or for painting. Be sure to call unconfigureCellRenderer() when the
     * configured component is no longer needed.
     * @param isLayout True if the renderer will be used for layout, false if for painting.
     * @param component The cell renderer component.
     * @param isCategory True if the row is a category row.
     * @param row The row index.
     * @param isSelected True if the row is selected.
     */
    protected void configureCellRenderer(boolean isLayout,
                                         Component component,
                                         boolean isCategory,
                                         int row,
                                         boolean isSelected) {
        // We need to do some (very ugly) modifications because DefaultTreeCellRenderers have their own paint method
        // and paint a border around each item
        if (component instanceof DefaultTreeCellRenderer) {
            DefaultTreeCellRenderer treeCellRenderer = (DefaultTreeCellRenderer) component;
            treeCellRenderer.setBorder(new EmptyBorder(0, 0, 0, 0));
            if (!isLayout) {
                treeCellRenderer.setBackgroundNonSelectionColor(TRANSPARENT_COLOR);
                treeCellRenderer.setBackgroundSelectionColor(TRANSPARENT_COLOR);
                treeCellRenderer.setBorderSelectionColor(TRANSPARENT_COLOR);
                treeCellRenderer.setTextNonSelectionColor(foreground);
                treeCellRenderer.setTextSelectionColor(selectionForeground);
            }
        }

        if (component instanceof JLabel) {
            JLabel label = (JLabel) component;

            // Ideally, we configure the same set of attributes each time. However, we cannot configure the
            // icon except in the case where we want to set it to null. Thus, we save and restore the icon in
            // every case.

            oldCellRendererFont = label.getFont();
            oldCellRendererIcon = label.getIcon();
            oldCellRendererDisabledIcon = label.getDisabledIcon();

            Color fc = isSelected ? selectionForeground : foreground;
            Font f = null;

            if (isSideBar) {
                fc = getSideBarForeground(isCategory, isSelected);

                Font sf = getSideBarFont(isCategory, isSelected);
                if (sf != null) {
                    // Trick the renderer into accepting the font
                    // It ignores instances of FontUIResource
                    f = fixFont(sf);
                }

                if (isCategory) {
                    label.setIcon(null);
                    label.setDisabledIcon(null);
                }
            }

            if (!isLayout && fc != null) {
                label.setForeground(fc);
            }

            if (f != null) {
                label.setFont(f);
            }
        }
    }

    protected void unconfigureCellRenderer(Component component) {
        if (component instanceof JLabel) {
            JLabel label = (JLabel) component;
            label.setFont(oldCellRendererFont);
            label.setIcon(oldCellRendererIcon);
            label.setDisabledIcon(oldCellRendererDisabledIcon);
        }
    }

    protected Font getSideBarFont(boolean isTopLevel, boolean isSelected) {
        if (isTopLevel) {
            return UIManager.getFont("Tree.sideBarCategory.font");
        } else if (isSelected) {
            return UIManager.getFont("Tree.sideBar.selectionFont");
        } else {
            return UIManager.getFont("Tree.sideBar.font");
        }
    }

    protected Color getSideBarForeground(boolean isTopLevel, boolean isSelected) {
        if (isTopLevel) {
            return UIManager.getColor("Tree.sideBarCategory.foreground");
        } else if (isSelected) {
            return UIManager.getColor("Tree.sideBar.selectionForeground");
        } else {
            return UIManager.getColor("Tree.sideBar.foreground");
        }
    }

    protected Font fixFont(Font f) {
        return f instanceof FontUIResource ? new MyFont(f) : f;
    }

    protected class MyFont extends Font {
        public MyFont(Font f) {
            super(f);
        }
    }

    protected void paintHorizontalSeparators(final Graphics g, final JComponent c) {
        g.setColor(UIManager.getColor("Tree.line"));

        final Rectangle clipBounds = g.getClipBounds();

        final int beginRow = getRowForPath(tree, getClosestPathForLocation(tree, 0, clipBounds.y));
        final int endRow = getRowForPath(tree, getClosestPathForLocation(tree, 0, clipBounds.y + clipBounds.height - 1));

        if (beginRow <= -1 || endRow <= -1) { return; }

        for (int i = beginRow; i <= endRow; ++i) {
            final TreePath path = getPathForRow(tree, i);

            if (path != null && path.getPathCount() == 2) {
                final Rectangle rowBounds = getPathBounds(tree, getPathForRow(tree, i));

                // Draw a line at the top
                if (rowBounds != null) g.drawLine(clipBounds.x, rowBounds.y, clipBounds.x + clipBounds.width, rowBounds.y);
            }
        }
    }

    protected void paintVerticalPartOfLeg(final Graphics g, final Rectangle clipBounds, final Insets insets, final TreePath path) {
        if (lineStyle == LEG_LINE_STYLE) {
            super.paintVerticalPartOfLeg(g, clipBounds, insets, path);
        }
    }

    protected void paintHorizontalPartOfLeg(final Graphics g, final Rectangle clipBounds, final Insets insets, final Rectangle bounds, final TreePath path, final int row, final boolean isExpanded, final boolean hasBeenExpanded, final boolean isLeaf) {
        if (lineStyle == LEG_LINE_STYLE) {
            super.paintHorizontalPartOfLeg(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
        }
    }

    /** This class listens for changes in line style */
    class LineListener implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent e) {
            final String name = e.getPropertyName();
            if (name.equals(LINE_STYLE)) {
                decodeLineStyle(e.getNewValue());
            }
        }
    }

    /**
     * Paints the expand (toggle) part of a row. The receiver should NOT modify <code>clipBounds</code>, or
     * <code>insets</code>.
     */
    protected void paintExpandControl(final Graphics g, final Rectangle clipBounds, final Insets insets, final Rectangle bounds, final TreePath path, final int row, final boolean isExpanded, final boolean hasBeenExpanded, final boolean isLeaf) {
        final Object value = path.getLastPathComponent();

        // Draw icons if not a leaf and either hasn't been loaded,
        // or the model child count is > 0.
        if (isLeaf || (hasBeenExpanded && treeModel.getChildCount(value) <= 0)) return;

        // Both icons are the same size
        final Icon icon = isExpanded ? getExpandedIcon() : getCollapsedIcon();
        if (icon != null && !(icon instanceof UIResource)) {
            super.paintExpandControl(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
            return;
        }

        int middleXOfKnob;
        final boolean isLeftToRight = AquaUtils.isLeftToRight(tree); // Basic knows, but keeps it private
        if (isLeftToRight) {
            middleXOfKnob = bounds.x - getRightChildIndent() + 1;
        } else {
            middleXOfKnob = bounds.x + bounds.width + getRightChildIndent() - 1;
        }
        final int middleYOfKnob = bounds.y + (bounds.height / 2);

        State state = getState(path);
        if (!fIsInBounds && state == State.PRESSED) {
            state = State.ACTIVE;
        }

        Configuration tg = getDisclosureTriangleConfiguration(state, isExpanded, isLeftToRight);
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo((LayoutConfiguration) tg);
        int width = (int) Math.ceil(layoutInfo.getFixedVisualWidth());
        int height = (int) Math.ceil(layoutInfo.getFixedVisualHeight());

        if (width == 0) {
            width = 20;
        }
        if (height == 0) {
            height = width;
        }

        final int x = middleXOfKnob - width / 2;
        final int y = middleYOfKnob - height / 2;
        painter.configure(width, height);
        painter.getPainter(tg).paint(g, x, y);
    }

    protected Configuration getDisclosureTriangleConfiguration(State state, boolean isExpanded, boolean isLeftToRight) {
        ButtonWidget widget = ButtonWidget.BUTTON_DISCLOSURE_TRIANGLE;
        AquaUIPainter.Size size = AquaUIPainter.Size.REGULAR;
        boolean isFocused = false;
        ButtonState bs = isExpanded ? ButtonState.ON : ButtonState.OFF;
        UILayoutDirection ld = isLeftToRight ? UILayoutDirection.LEFT_TO_RIGHT : UILayoutDirection.RIGHT_TO_LEFT;

        if (fAnimationTransition >= 0) {
            ButtonState previousButtonState = bs == ButtonState.ON ? ButtonState.OFF : ButtonState.ON;
            return new AnimatedButtonConfiguration(widget, size, state, isFocused, bs, ld, previousButtonState, fAnimationTransition);
        }

        return new ButtonConfiguration(widget, size, state, isFocused, bs, ld);
    }

    @Override
    public Icon getCollapsedIcon() {
        final Icon icon = super.getCollapsedIcon();
        if (AquaUtils.isLeftToRight(tree)) return icon;
        if (!(icon instanceof UIResource)) return icon;
        return UIManager.getIcon("Tree.rightToLeftCollapsedIcon");
    }

    protected State getState(final TreePath path) {
        if (!tree.isEnabled()) return State.DISABLED;
        if (fIsPressed) {
            if (fTrackingPath.equals(path)) return State.PRESSED;
        }
        return State.ACTIVE;
    }

    /**
     * Misnamed - this is called on mousePressed Macs shouldn't react till mouseReleased
     * We install a motion handler that gets removed after.
     * See super.MouseInputHandler & super.startEditing for why
     */
    protected void handleExpandControlClick(final TreePath path, final int mouseX, final int mouseY) {
        fMouseHandler = new TreeArrowMouseInputHandler(path);
    }

    /**
     * Returning true signifies a mouse event on the node should toggle the selection of only the row under mouse.
     */
    protected boolean isToggleSelectionEvent(final MouseEvent event) {
        return SwingUtilities.isLeftMouseButton(event) && event.isMetaDown();
    }

    protected AbstractLayoutCache.NodeDimensions createNodeDimensions() {
        return new TreeNodeDimensions();
    }

    protected void updateDropTargetListener() {
        DropTarget dropTarget = tree.getDropTarget();
        if (dropTarget instanceof UIResource) {
            if (defaultDropTargetListener == null) {
                defaultDropTargetListener = new TreeDropTargetListener();
            }
            try {
                dropTarget.addDropTargetListener(defaultDropTargetListener);
            } catch (TooManyListenersException tmle) {
                // should not happen... swing drop target is multicast
            }
        }
    }

    protected class FocusHandler extends BasicTreeUI.FocusHandler {
        public void focusGained(final FocusEvent e) {
            super.focusGained(e);
            repaintSelection();
        }

        public void focusLost(final FocusEvent e) {
            super.focusLost(e);
            repaintSelection();
        }
    }

    public class MacPropertyChangeHandler extends PropertyChangeHandler {
        public void propertyChange(final PropertyChangeEvent evt) {
            final String name = evt.getPropertyName();
            if (name == null) {
                return;
            }

            if (evt.getSource() == tree) {

                // This case duplicates BasicTreeUI because we have own ignoreLAChange variable.
                if (name.equals(JTree.LEAD_SELECTION_PATH_PROPERTY) && ignoreLAChange) {
                    return;
                }

                // This case duplicates BasicTreeUI because we have own ignoreLAChange variable.
                if (name.equals(JTree.ANCHOR_SELECTION_PATH_PROPERTY) && ignoreLAChange) {
                    return;
                }

                if (name.equals(AquaFocusHandler.FRAME_ACTIVE_PROPERTY)) {
                    AquaFocusHandler.swapSelectionColors("Tree", tree, evt.getNewValue());
                    tree.repaint();
                    // The following supports the translucent legacy scroll bar used for a sidebar tree
                    if (isSideBar) {
                        Container parent = tree.getParent();
                        if (parent instanceof JViewport) {
                            JViewport vp = (JViewport) parent;
                            Container vpp = vp.getParent();
                            if (vpp instanceof JScrollPane) {
                                JScrollPane sp = (JScrollPane) vpp;
                                sp.repaint();
                            }
                        }
                    }
                    return;
                }

                if (name.equals("transferHandler")) {
                    updateDropTargetListener();
                    return;
                }

                if (isCellFilledProperty(name)) {
                    updateProperties();
                    tree.repaint();
                    if (treeState != null) {
                        treeState.invalidateSizes();
                    }
                    return;
                }

                if (isStyleProperty(name)) {
                    updateProperties();
                    tree.repaint();
                    return;
                }

                super.propertyChange(evt);
            }
        }
    }

    /**
     * TreeArrowMouseInputHandler handles passing all mouse events the way a Mac should - hilite/dehilite on enter/exit,
     * only perform the action if released in arrow.
     *
     * Just like super.MouseInputHandler, this is removed once it's not needed, so they won't clash with each other
     */
    // The Adapters take care of defining all the empties
    class TreeArrowMouseInputHandler extends MouseInputAdapter {
        protected Rectangle fPathBounds = new Rectangle();

        // Values needed for paintOneControl
        protected boolean fIsLeaf, fIsExpanded, fHasBeenExpanded;
        protected Rectangle fBounds, fVisibleRect;
        int fTrackingRow;
        Insets fInsets;
        //Color fBackground;

        TreeArrowMouseInputHandler(final TreePath path) {
            fTrackingPath = path;
            fTrackingRow = getRowForPath(fTrackingPath);
            fIsPressed = true;
            fIsInBounds = true;
            this.fPathBounds = getPathArrowBounds(path);
            tree.addMouseListener(this);
            tree.addMouseMotionListener(this);

            // the background should be the background for the row
//            fBackground = fTrackingRow >= 0 ? getSpecialBackgroundForRow(fTrackingRow) : null;
//            if (fBackground == null) {
//                fBackground = tree.getBackground();
//                if (!tree.isOpaque()) {
//                    final Component p = tree.getParent();
//                    if (p != null) fBackground = p.getBackground();
//                }
//            }

            // Set up values needed to paint the triangle - see
            // BasicTreeUI.paint
            fVisibleRect = tree.getVisibleRect();
            fInsets = tree.getInsets();

            if (fInsets == null) fInsets = new Insets(0, 0, 0, 0);
            fIsLeaf = treeModel.isLeaf(path.getLastPathComponent());
            if (fIsLeaf) fIsExpanded = fHasBeenExpanded = false;
            else {
                fIsExpanded = treeState.getExpandedState(path);
                fHasBeenExpanded = tree.hasBeenExpanded(path);
            }
            final Rectangle boundsBuffer = new Rectangle();
            fBounds = getPathBounds(fTrackingPath, fInsets, boundsBuffer);

            paintOneControl();
        }

        public void mouseDragged(final MouseEvent e) {
            fIsInBounds = fPathBounds.contains(e.getX(), e.getY());
                paintOneControl();
            }

        @Override
        public void mouseExited(MouseEvent e) {
            fIsInBounds = fPathBounds.contains(e.getX(), e.getY());
            paintOneControl();
        }

        public void mouseReleased(final MouseEvent e) {
            if (tree == null) return;

            if (fIsPressed) {
                final boolean wasInBounds = fIsInBounds;

                fIsPressed = false;
                fIsInBounds = false;

                if (wasInBounds) {
                    fIsExpanded = !fIsExpanded;
                    paintAnimation(fIsExpanded);
                    if (e.isAltDown()) {
                        if (fIsExpanded) {
                            expandNode(fTrackingRow, true);
                        } else {
                            collapseNode(fTrackingRow, true);
                        }
                    } else {
                        toggleExpandState(fTrackingPath);
                    }
                }
            }
            fTrackingPath = null;
            removeFromSource();
        }

        protected void paintAnimation(final boolean expanding) {
                paintAnimationFrame(0);
                paintAnimationFrame(0.5f);
                paintAnimationFrame(1);
            fAnimationTransition = -1;
        }

        protected void paintAnimationFrame(float transition) {
            fAnimationTransition = transition;
            paintOneControl();
            try { Thread.sleep(20); } catch (final InterruptedException e) { }
        }

        // Utility to paint just one widget while it's being tracked
        // Just doing "repaint" runs into problems if someone does "translate" on the graphics
        // (ie, Sun's JTreeTable example, which is used by Moneydance - see Radar 2697837)
        void paintOneControl() {
            if (tree == null) return;
            final Graphics g = tree.getGraphics();
            if (g == null) {
                // i.e. source is not displayable
                return;
            }

            try {
                g.setClip(fVisibleRect);
                // If we ever wanted a callback for drawing the arrow between
                // transition stages
                // the code between here and paintExpandControl would be it
                //g.setColor(fBackground);
                //g.fillRect(fPathBounds.x, fPathBounds.y, fPathBounds.width, fPathBounds.height);

                // if there is no tracking path, we don't need to paint anything
                if (fTrackingPath == null) return;

                // draw the vertical line to the parent
                final TreePath parentPath = fTrackingPath.getParentPath();
                if (parentPath != null) {
                    paintVerticalPartOfLeg(g, fPathBounds, fInsets, parentPath);
                    paintHorizontalPartOfLeg(g, fPathBounds, fInsets, fBounds, fTrackingPath, fTrackingRow, fIsExpanded, fHasBeenExpanded, fIsLeaf);
                } else if (isRootVisible() && fTrackingRow == 0) {
                    paintHorizontalPartOfLeg(g, fPathBounds, fInsets, fBounds, fTrackingPath, fTrackingRow, fIsExpanded, fHasBeenExpanded, fIsLeaf);
                }
                paintExpandControl(g, fPathBounds, fInsets, fBounds, fTrackingPath, fTrackingRow, fIsExpanded, fHasBeenExpanded, fIsLeaf);
            } finally {
                g.dispose();
            }
        }

        protected void removeFromSource() {
            tree.removeMouseListener(this);
            tree.removeMouseMotionListener(this);
            }
        }

    protected int getRowForPath(final TreePath path) {
        return treeState.getRowForPath(path);
    }

    private Rectangle getPathBounds(TreePath path, Insets insets, Rectangle bounds) {
        bounds = treeState.getBounds(path, bounds);
        if (bounds != null) {
            if (AquaUtils.isLeftToRight(tree)) {
                bounds.x += insets.left;
            } else {
                bounds.x = tree.getWidth() - (bounds.x + bounds.width) - insets.right;
            }
            bounds.y += insets.top;
        }
        return bounds;
    }

    /**
     * see isLocationInExpandControl for bounds calc
     */
    protected Rectangle getPathArrowBounds(final TreePath path) {
        final Rectangle bounds = getPathBounds(tree, path); // Gives us the y values, but x is adjusted for the contents
        final Insets i = tree.getInsets();

        if (getExpandedIcon() != null) bounds.width = getExpandedIcon().getIconWidth();
        else bounds.width = 8;

        int boxLeftX = (i != null) ? i.left : 0;
        if (AquaUtils.isLeftToRight(tree)) {
            boxLeftX += (((path.getPathCount() + depthOffset - 2) * totalChildIndent) + getLeftChildIndent()) - bounds.width / 2;
        } else {
            boxLeftX += tree.getWidth() - 1 - ((path.getPathCount() - 2 + depthOffset) * totalChildIndent) - getLeftChildIndent() - bounds.width / 2;
        }
        bounds.x = boxLeftX;
        return bounds;
    }

    protected void installKeyboardActions() {
        super.installKeyboardActions();
        tree.getActionMap().put("aquaExpandNode", new KeyboardExpandCollapseAction(true, false));
        tree.getActionMap().put("aquaCollapseNode", new KeyboardExpandCollapseAction(false, false));
        tree.getActionMap().put("aquaFullyExpandNode", new KeyboardExpandCollapseAction(true, true));
        tree.getActionMap().put("aquaFullyCollapseNode", new KeyboardExpandCollapseAction(false, true));
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    class KeyboardExpandCollapseAction extends AbstractAction {
        /**
         * Determines direction to traverse, 1 means expand, -1 means collapse.
         */
        final boolean expand;
        final boolean recursive;

        /**
         * True if the selection is reset, false means only the lead path changes.
         */
        public KeyboardExpandCollapseAction(final boolean expand, final boolean recursive) {
            this.expand = expand;
            this.recursive = recursive;
        }

        public void actionPerformed(final ActionEvent e) {
            if (tree == null || 0 > getRowCount(tree)) return;

            final TreePath[] selectionPaths = tree.getSelectionPaths();
            if (selectionPaths == null) return;

            for (int i = selectionPaths.length - 1; i >= 0; i--) {
                final TreePath path = selectionPaths[i];

                /*
                 * Try and expand the node, otherwise go to next node.
                 */
                if (expand) {
                    expandNode(tree.getRowForPath(path), recursive);
                    continue;
                }
                // else collapse

                // in the special case where there is only one row selected,
                // we want to do what the Cocoa does, and select the parent
                if (selectionPaths.length == 1 && tree.isCollapsed(path)) {
                    final TreePath parentPath = path.getParentPath();
                    if (parentPath != null && (!(parentPath.getParentPath() == null) || tree.isRootVisible())) {
                        tree.scrollPathToVisible(parentPath);
                        tree.setSelectionPath(parentPath);
                    }
                    continue;
                }

                collapseNode(tree.getRowForPath(path), recursive);
            }
        }

        public boolean isEnabled() {
            return (tree != null && tree.isEnabled());
        }
    }

    void expandNode(final int row, final boolean recursive) {
        final TreePath path = getPathForRow(tree, row);
        if (path == null) return;

        tree.expandPath(path);
        if (!recursive) return;

        expandAllNodes(path, row + 1);
    }

    void expandAllNodes(final TreePath parent, final int initialRow) {
        for (int i = initialRow; true; i++) {
            final TreePath path = getPathForRow(tree, i);
            if (!parent.isDescendant(path)) return;

            tree.expandPath(path);
        }
    }

    void collapseNode(final int row, final boolean recursive) {
        final TreePath path = getPathForRow(tree, row);
        if (path == null) return;

        if (recursive) {
            collapseAllNodes(path, row + 1);
        }

        tree.collapsePath(path);
    }

    void collapseAllNodes(final TreePath parent, final int initialRow) {
        int lastRow = -1;
        for (int i = initialRow; lastRow == -1; i++) {
            final TreePath path = getPathForRow(tree, i);
            if (!parent.isDescendant(path)) {
                lastRow = i - 1;
            }
        }

        for (int i = lastRow; i >= initialRow; i--) {
            final TreePath path = getPathForRow(tree, i);
            tree.collapsePath(path);
        }
    }

    /**
     * A DropTargetListener to extend the default Swing handling of drop operations
     * by moving the tree selection to the nearest location to the mouse pointer.
     */
    public static class TreeDropTargetListener extends DropTargetAdapter {

        private boolean isStateSaved;
        private int[] selectedIndices;

        // This is an awkward solution. The problem is that TransferHandler does not provide any hooks for the events we
        // are interested in, and it is impossible to implement a TransferHandler via delegation. The result is that we
        // make redundant calls to the TransferHandler using a method that is no longer recommended (because we do not
        // have access to or the ability to create a TransferSupport).

        @Override
        public void dragEnter(DropTargetDragEvent e) {
            JTree c = getComponent(e);
            TransferHandler th = c.getTransferHandler();
            if (th.canImport(c, e.getCurrentDataFlavors())) {
                saveComponentState(c);
                isStateSaved = true;
            } else {
                isStateSaved = false;
            }
        }

        @Override
        public void dragOver(DropTargetDragEvent e) {
            // In theory, the drop target may no longer be active.
            JTree c = getComponent(e);
            TransferHandler th = c.getTransferHandler();
            if (th.canImport(c, e.getCurrentDataFlavors())) {
                updateInsertionLocation(c, e.getLocation());
            }
        }

        @Override
        public void dragExit(DropTargetEvent e) {
            if (isStateSaved) {
                JTree c = getComponent(e);
                restoreComponentState(c);
            }
        }

        @Override
        public void drop(DropTargetDropEvent e) {
        }

        protected void saveComponentState(JTree c) {
            selectedIndices = c.getSelectionRows();
        }

        protected void restoreComponentState(JTree c) {
            c.setSelectionRows(selectedIndices);
        }

        protected void updateInsertionLocation(JTree c, Point p) {
            BasicTreeUI ui = (BasicTreeUI) c.getUI();
            TreePath path = ui.getClosestPathForLocation(c, p.x, p.y);
            if (path != null) {
                c.setSelectionPath(path);
            }
        }
    }

    protected static JTree getComponent(DropTargetEvent e) {
        DropTargetContext context = e.getDropTargetContext();
        return (JTree) context.getComponent();
    }
}
