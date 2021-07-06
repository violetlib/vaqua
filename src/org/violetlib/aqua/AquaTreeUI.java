/*
 * Changes Copyright (c) 2015-2021 Alan Snyder.
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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.TooManyListenersException;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.MouseInputAdapter;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.AbstractLayoutCache;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.FixedHeightLayoutCache;
import javax.swing.tree.TreePath;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonState;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.UILayoutDirection;

import static org.violetlib.aqua.AquaImageFactory.LIGHTEN_FOR_DISABLED;

/**
 * A tree UI based on AquaTreeUI for Yosemite. It supports filled cells. It supports the striped and sidebar styles.
 * It supports a more compatible mouse behavior. It displays using an inactive style based on focus ownership. It
 * configures cell renderers, as possible, to conform to the tree style.
 * AquaTreeUI supports the client property "value-add" system of customization See MetalTreeUI
 * This is heavily based on the 1.3.1 AquaTreeUI implementation.
 */
public class AquaTreeUI extends BasicTreeUI implements SelectionRepaintable, AquaComponentUI, AquaViewStyleContainerUI {

    public static ComponentUI createUI(JComponent c) {
        return new AquaTreeUI();
    }

    public static final String IS_CELL_FILLED_KEY = "JTree.isCellFilled";
    public static final String QUAQUA_IS_CELL_FILLED_KEY = "Quaqua.Tree.isCellFilled";

    public static final String TREE_STYLE_KEY = "JTree.style";
    public static final String QUAQUA_TREE_STYLE_KEY = "Quaqua.Tree.style";
    public static final String TREE_VIEW_STYLE_KEY = "JTree.viewStyle";

    public static final String SELECTION_FOREGROUND_KEY = "JTree.selectionForeground";

    // Begin Line Stuff from Metal

    private static final String LINE_STYLE = "JTree.lineStyle";

    private static final String LEG_LINE_STYLE_STRING = "Angled";
    private static final String HORIZ_STYLE_STRING = "Horizontal";
    private static final String NO_STYLE_STRING = "None";

    private static final int LEG_LINE_STYLE = 2;
    private static final int HORIZ_LINE_STYLE = 1;
    private static final int NO_LINE_STYLE = 0;

    private int lineStyle = NO_LINE_STYLE;

    private final PropertyChangeListener propertyChangeListener = new AquaPropertyChangeListener();
    private final FocusListener editingComponentFocusListener = new EditingComponentFocusListener();
    private final ComponentListener componentListener = new AquaTreeComponentListener();
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
    protected boolean isInset;

    // state variables for painting, needed because we share painting implementation with the superclass
    protected boolean shouldPaintSelection;
    protected boolean isActive;     // for communication between paint() and paintRow()

    // state variables needed for cell renderer configuration
    private Font oldCellRendererFont;
    private Icon oldCellRendererIcon;
    private Icon oldCellRendererDisabledIcon;

    // support for sidebar presentation
    private SidebarVibrantEffects sidebarVibrantEffects;

    private static DropTargetListener defaultDropTargetListener = null;

    protected @NotNull ContainerContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    public AquaTreeUI() {
        this.colors = AquaColors.CONTAINER_COLORS;
    }

    public boolean isSideBar() {
        return isSideBar;
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();

        LookAndFeel.installBorder(tree, "Tree.border");
        Object lineStyleFlag = tree.getClientProperty(LINE_STYLE);
        decodeLineStyle(lineStyleFlag);

        // Unfortunately, there is no reliable way to undo this change because we will not know if the application
        // later tries to set it to false. Fortunately, swapping LAFs is unusual and displaying a root node is dumb.
        tree.setRootVisible(false);
        // Too bad this does not work:
        // LookAndFeel.installProperty(tree, JTree.ROOT_VISIBLE_PROPERTY, false);
        tree.putClientProperty(AquaCellEditorPolicy.IS_CELL_CONTAINER_PROPERTY, true);

        isStriped = getStripedValue();  // avoid an unnecessary recalculation
        updateProperties();
        configureAppearanceContext(null);
        updateInset();
    }

    @Override
    protected void uninstallDefaults() {
        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        tree.addPropertyChangeListener(propertyChangeListener);
        AppearanceManager.installListeners(tree);
        tree.addComponentListener(componentListener);
    }

    @Override
    protected void uninstallListeners() {
        tree.removeComponentListener(componentListener);
        AppearanceManager.uninstallListeners(tree);
        tree.removePropertyChangeListener(propertyChangeListener);
        super.uninstallListeners();
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        configureAppearanceContext(null);
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(tree);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        colors = determineColors();
        colors.configureForContainer();
        AquaColors.installColors(tree, appearanceContext, colors);
        LookAndFeel.installProperty(tree, "opaque", !isStriped);
        tree.repaint();
        repaintScrollPane();
    }

    protected @NotNull ContainerContextualColors determineColors() {
        if (isSideBar()) {
            return AquaColors.SIDEBAR_CONTAINER_COLORS;
        }
        if (isStriped) {
            return AquaColors.STRIPED_CONTAINER_COLORS;
        }
        return AquaColors.CONTAINER_COLORS;
    }

    protected void repaintScrollPane() {
        // The following supports the translucent legacy scroll bar used for a sidebar tree
        if (isSideBar()) {
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
    }

    protected AquaUIPainter.State getState() {
        if (!AquaFocusHandler.isActive(tree)) {
            return State.INACTIVE;
        }
        return tree.isEnabled()
                ? (shouldDisplayAsFocused() ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE)
                : AquaUIPainter.State.DISABLED;
    }

    protected boolean shouldDisplayAsFocused() {
        return AquaFocusHandler.isActive(tree) && (AquaFocusHandler.hasFocus(tree) || tree.isEditing());
    }

    protected void updateProperties() {
        if (tree != null) {
            isCellFilled = Boolean.TRUE.equals(AquaUtils.getBooleanProperty(tree, IS_CELL_FILLED_KEY, QUAQUA_IS_CELL_FILLED_KEY));
            String style = getStyleProperty();
            boolean newIsSidebar = style != null && (style.equals("sideBar") || style.equals("sourceList"));
            updateStriped();
            if (newIsSidebar != isSideBar) {
                isSideBar = newIsSidebar;
                updateSidebar();
                tree.revalidate();
                tree.repaint();
            }
        }
    }

    protected void updateRowHeight() {
        // sidebar trees have two row heights, one for ordinary items and one for category headers
        int height = isSideBar() ? 0 : 19;
        LookAndFeel.installProperty(tree, "rowHeight", height);
    }

    // Called on a change to isSideBar or the ancestry of the tree
    protected void updateSidebar() {
        if (isSideBar() && tree.isDisplayable()) {
            ensureSidebarVibrantEffects();
        } else {
            disposeSidebarVibrantEffects();
        }
        // On macOS 11+, the sidebar style implies the inset view style.
        if (AquaUtils.isInsetViewSupported()) {
            updateRowHeight();
            tree.revalidate();
            tree.repaint();
            updateSize();
        }
    }

    protected void ensureSidebarVibrantEffects() {
        JComponent top = getComponentForVisualEffectView();
        if (sidebarVibrantEffects != null && sidebarVibrantEffects.getComponent() != top) {
            disposeSidebarVibrantEffects();
        }
        if (sidebarVibrantEffects == null) {
            sidebarVibrantEffects = new SidebarVibrantEffects(top);
        }
    }

    protected void disposeSidebarVibrantEffects() {
        if (sidebarVibrantEffects != null) {
            sidebarVibrantEffects.dispose();
            sidebarVibrantEffects = null;
        }
    }

    protected @NotNull JComponent getComponentForVisualEffectView()
    {
        Container parent = tree.getParent();
        if (parent instanceof JViewport) {
            return (JViewport) parent;
        }
        return tree;
    }

    /**
     * Support for sidebar vibrant effects. An NSVisualEffectView is created for the sidebar background and for each
     * selected row background region.
     */
    protected class SidebarVibrantEffects extends VisualEffectView {
        protected TreeSelectionBoundsTracker bt;

        public SidebarVibrantEffects(JComponent top) {
            super(top, AquaVibrantSupport.SIDEBAR_STYLE, true);
            bt = new TreeSelectionBoundsTracker(tree, this::updateSelectionBackgrounds) {
                @Override
                protected int convertRowYCoordinateToSelectionDescription(int y) {
                    if (top != tree) {
                        Point p = SwingUtilities.convertPoint(tree, 0, y, top);
                        return p.y;
                    } else {
                        return y;
                    }
                }
            };
        }

        public void update() {
            if (bt != null) {
                bt.update();
            }
        }

        public void dispose() {
            super.dispose();
            if (bt != null) {
                bt.dispose();
                bt = null;
            }
        }

        @Override
        protected void windowChanged(Window newWindow) {
            super.windowChanged(newWindow);
            if (bt != null) {
                bt.reset();
            }
        }
    }

    private void updateStriped() {
        boolean value = getStripedValue();
        if (value != isStriped) {
            isStriped = value;
            configureAppearanceContext(null);
        }
    }

    private boolean getStripedValue() {
        String value = getStyleProperty();
        return "striped".equals(value) && isBackgroundClear();
    }

    @Override
    public void scrollPaneAncestorChanged(@Nullable JScrollPane sp) {
    }

    private void updateInset() {
        boolean value = getInsetValue();
        if (value != isInset) {
            isInset = value;
            tree.revalidate();
            tree.repaint();
            updateSize();
        }
    }

    private boolean getInsetValue() {
        if (AquaUtils.isInsetViewSupported()) {
            String value = getViewStyleProperty();
            return "inset".equals(value);
        }
        return false;
    }

    public boolean isStriped() {
        return isStriped;
    }

    @Override
    public boolean isInset() {
        return isInset || (isSideBar() && OSXSystemProperties.useInsetViewStyle());
    }

    private boolean isBackgroundClear() {
        Color c = tree.getBackground();
        return c == null || c.getAlpha() == 0 || c instanceof ColorUIResource;
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

    protected boolean isViewStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, TREE_VIEW_STYLE_KEY);
    }

    protected String getViewStyleProperty() {
        return AquaUtils.getProperty(tree, TREE_VIEW_STYLE_KEY);
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
        super.completeEditing();
    }

    @Override
    protected void completeEditing(boolean messageStop, boolean messageCancel, boolean messageTree) {
        if (editorFocusOwner != null) {
            editorFocusOwner.removeFocusListener(editingComponentFocusListener);
        }
        super.completeEditing(messageStop, messageCancel, messageTree);
        configureAppearanceContext(null);
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
            if (!e.isTemporary()) {
                AquaTreeUI.this.stopEditing(tree);
            }
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
    protected void decodeLineStyle(Object lineStyleFlag) {
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

    public TreePath getClosestPathForLocation(JTree treeLocal, int x, int y) {
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
            if (editingComponent != null && editingRow == row) {
                Dimension prefSize = editingComponent.getPreferredSize();
                return computeRowBounds(row, depth, prefSize, size);
            }
            // Not editing, use renderer.
            if (currentCellRenderer != null) {
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

                if (tree != null) {
                    // Only ever removed when UI changes, this is OK!
                    rendererPane.add(aComponent);
                    aComponent.validate();
                }
                Dimension prefSize = aComponent.getPreferredSize();
                unconfigureCellRenderer(aComponent);
                return computeRowBounds(row, depth, prefSize, size);
            }
            return null;
        }
    }

    /**
     * Compute the basic bounds of a row based on the preferred size of its renderer/editor component.
     */
    private @NotNull Rectangle computeRowBounds(int row, int depth, @NotNull Dimension ps, @Nullable Rectangle size) {
        int width = ps.width;
        int height = ps.height;
        int rowHeight = getRowHeight();
        if (rowHeight > 0) {
            height = rowHeight;
        } else {
            int styleHeight = getStyleRowHeight(depth);
            if (styleHeight > height) {
                height = styleHeight;
            }
        }
        int x = getRowX(row, depth);
        if (size != null) {
            size.x = x;
            size.width = width;
            size.height = height;
            return size;
        }
        return new Rectangle(x, 0, width, height);
    }

    /**
     * Customize the X offset for the sidebar, to take into account that category rows have no icons.
     */
    @Override
    protected int getRowX(int row, int depth) {
        if (isSideBar()) {
            if (depth > 1) {
                depth--;
            }
        }

        return totalChildIndent * (depth + depthOffset);
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        paint(g, c);
    }

    @Override
    public void paint(@NotNull Graphics g, @NotNull JComponent c) {

        if (treeState == null || appearanceContext == null) {
            return;
        }

        // We do not really know when the layout of the tree may have changed, so we verify the selection background
        // locations now. Updating during painting has the advantage of (approximately) coordinating updates to the
        // selection backgrounds with updates to the visible display of the tree, which minimizes artifacts when
        // scrolling.

        // An assumption is made here that scrolling a tree will cause the tree to be painted.

        if (sidebarVibrantEffects != null) {
            sidebarVibrantEffects.update();
        }

        // Set the variables used by paintRow

        isActive = AquaFocusHandler.isActive(c);
        shouldPaintSelection = !Boolean.FALSE.equals(tree.getClientProperty("JTree.paintSelectionBackground"));

        Color background = getCurrentBackground();
        paintBackground(g, background);

        super.paint(g, c);

        // Paint the lines
        if (lineStyle == HORIZ_LINE_STYLE && !largeModel) {
            paintHorizontalSeparators(g, c);
        }
    }

    protected void paintBackground(@NotNull Graphics g, @Nullable Color background) {
        if (tree.isOpaque()) {
            int width = tree.getWidth();
            int height = tree.getHeight();
            AquaUtils.fillRect(g, background, 0, 0, width, height);
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

    public @Nullable Color getCurrentBackground() {
        if (isSideBar()) {
            return null;
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

        colors.configureForContainer();

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

            boolean isRowSelected = tree.isRowSelected(row) && shouldPaintSelection;
            Color bc = getSpecialBackgroundForRow(row, isRowSelected);
            if (bc != null) {
                Graphics2D gg = (Graphics2D) g;
                gg.setColor(bc);
                int cx = insets.left;
                int cy = bounds.y;
                int cw = rwidth;
                int ch = bounds.height;
                if (isInset() && isRowSelected) {
                    boolean isSelectedAbove = row > 0 && tree.isRowSelected(row - 1);
                    boolean isSelectedBelow = row < tree.getRowCount() - 1 && tree.isRowSelected(row + 1);
                    AquaUtils.paintInsetCellSelection(gg, isSelectedAbove, isSelectedBelow, 0, cy, width, ch);
                } else if (isInset() && isStriped()) {
                    AquaUtils.paintInsetStripedRow(gg, 0, cy, width, ch);
                } else {
                    gg.fillRect(cx, cy, cw, ch);
                }
            }

            if ((bounds.y + bounds.height) >= endY) {
                break;
            }

            row++;
        }

        colors.configureForContainer();
    }

    protected @Nullable Color getSpecialBackgroundForRow(int row, boolean isRowSelected) {
        AppearanceContext ac = appearanceContext;
        if (isRowSelected) {
            if (sidebarVibrantEffects != null) {
                // sidebar selection backgrounds are managed using special views
                return null;
            }
            ac = ac.withSelected(true);
        } else if (!isStriped) {
            return null;
        }
        colors.configureForRow(row, isRowSelected);
        return colors.getBackground(ac);
    }

    /**
     * Paint stripes for an empty tree.
     */
    protected void paintEmptyTreeStripes(Graphics g) {
        assert appearanceContext != null;

        Graphics2D gg = (Graphics2D) g;
        int width = tree.getWidth();
        int height = tree.getHeight();
        Insets insets = tree.getInsets();

        int rwidth = width - insets.left - insets.left;
        int rheight = tree.getRowHeight();
        if (rheight <= 0) {
            // FIXME - Use the cell renderer to determine the height
            rheight = tree.getFont().getSize() + 4;
        }
        int row = 0;
        for (int y = 0; y < height; y += rheight) {
            boolean isRowSelected = tree.isRowSelected(row);
            colors.configureForRow(row, isRowSelected);
            Color background = colors.getBackground(appearanceContext);
            g.setColor(background);
            if (isInset()) {
                AquaUtils.paintInsetStripedRow(gg, 0, y, width, rheight);
            } else {
                g.fillRect(insets.left, y, rwidth, rheight);
            }
            row++;
        }
    }

    /**
     * Customized row painting for sidebar trees. Various client properties are set. An attempt is made to prevent the
     * tree cell renderer from drawing a background or a border or (in the case of a top level node) an icon.
     */
    @Override
    protected void paintRow(Graphics g,
                            Rectangle clipBounds,
                            Insets insets,
                            Rectangle bounds,
                            TreePath path,
                            int row,
                            boolean isExpanded,
                            boolean hasBeenExpanded,
                            boolean isLeaf) {

        assert appearanceContext != null;

        if (editingComponent != null && editingRow == row)
            return;

        int leadIndex;

        if (tree.hasFocus()) {
            leadIndex = getLeadSelectionRow();
        } else
            leadIndex = -1;

        boolean isRowSelected = tree.isRowSelected(row);
        colors.configureForRow(row, isRowSelected);

        Component component = currentCellRenderer.getTreeCellRendererComponent
                (tree, path.getLastPathComponent(),
                        isRowSelected, isExpanded, isLeaf, row,
                        (leadIndex == row));

        boolean isCategory = path.getPathCount() == 2;
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
            Border existing = treeCellRenderer.getBorder();
            if (existing instanceof UIResource && existing != AquaLookAndFeel.NOTHING_BORDER) {
                treeCellRenderer.setBorder(AquaLookAndFeel.NOTHING_BORDER);
            }
            if (!isLayout) {
                assert appearanceContext != null;
                treeCellRenderer.setBackgroundNonSelectionColor(AquaColors.CLEAR);
                treeCellRenderer.setBackgroundSelectionColor(AquaColors.CLEAR);
                treeCellRenderer.setBorderSelectionColor(AquaColors.CLEAR);
                treeCellRenderer.setTextNonSelectionColor(colors.getForeground(appearanceContext.withSelected(false)));
                treeCellRenderer.setTextSelectionColor(colors.getForeground(appearanceContext.withSelected(true)));
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

            Color fc = appearanceContext != null ? colors.getForeground(appearanceContext) : null;
            Font f = null;
            Icon icon = label.getIcon();
            Object operator = null;
            AquaAppearance appearance = appearanceContext != null ? appearanceContext.getAppearance() : null;

            if (isSideBar()) {
                fc = getSideBarForeground(isCategory, isSelected);
                Color iconColor = null;
                if (appearance != null) {
                    iconColor = appearance.getColor("sidebarIcon");
                }

                Font sf = getSideBarFont(isCategory, isSelected);
                if (sf != null) {
                    // Trick the renderer into accepting the font
                    // It ignores instances of FontUIResource
                    f = fixFont(sf);
                }

                if (isCategory) {
                    label.setIcon(null);
                    label.setDisabledIcon(null);
                    icon = null;
                } else if (icon != null) {
                    if (AquaImageFactory.isTemplateIcon(icon)) {
                        if (OSXSystemProperties.OSVersion >= 1016 && appearance != null) {
                            if (AquaFocusHandler.isActive(tree)) {
                                if (appearance.isDark()) {
                                    iconColor = appearance.getColor("controlAccent");
                                } else {
                                    iconColor = appearance.getColor("controlAccent_pressed");
                                }
                            } else {
                                iconColor = appearance.getColor("controlAccent_disabled");
                            }
                        }
                        operator = iconColor;
                    } else {
                        operator = AquaFocusHandler.isActive(tree) ? null : LIGHTEN_FOR_DISABLED;
                    }
                }
            } else {
                if (appearance != null) {
                    operator = appearance.getColor("treeIcon");
                }
            }

            if (icon != null) {
                Image image = AquaImageFactory.getProcessedImage(icon, operator);
                if (image != null) {
                    icon = new ImageIcon(image);
                    label.setIcon(icon);
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
            return AquaColors.getSystemColor(tree, "secondaryLabel");
        } else {
            AppearanceContext context = AquaTreeUI.this.appearanceContext;
            assert context != null;
            if (isSelected) {
                context = context.withSelected(true);
            }
            return colors.getForeground(context);
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

    protected void paintHorizontalSeparators(Graphics g, JComponent c) {
        g.setColor(UIManager.getColor("Tree.line"));

        Rectangle clipBounds = g.getClipBounds();

        int beginRow = getRowForPath(tree, getClosestPathForLocation(tree, 0, clipBounds.y));
        int endRow = getRowForPath(tree, getClosestPathForLocation(tree, 0, clipBounds.y + clipBounds.height - 1));

        if (beginRow <= -1 || endRow <= -1) { return; }

        for (int i = beginRow; i <= endRow; ++i) {
            TreePath path = getPathForRow(tree, i);

            if (path != null && path.getPathCount() == 2) {
                Rectangle rowBounds = getPathBounds(tree, getPathForRow(tree, i));

                // Draw a line at the top
                if (rowBounds != null) g.drawLine(clipBounds.x, rowBounds.y, clipBounds.x + clipBounds.width, rowBounds.y);
            }
        }
    }

    protected void paintVerticalPartOfLeg(Graphics g, Rectangle clipBounds, Insets insets, TreePath path) {
        if (lineStyle == LEG_LINE_STYLE) {
            super.paintVerticalPartOfLeg(g, clipBounds, insets, path);
        }
    }

    protected void paintHorizontalPartOfLeg(Graphics g, Rectangle clipBounds, Insets insets, Rectangle bounds, TreePath path, int row, boolean isExpanded, boolean hasBeenExpanded, boolean isLeaf) {
        if (lineStyle == LEG_LINE_STYLE) {
            super.paintHorizontalPartOfLeg(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
        }
    }

    class AquaPropertyChangeListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent e) {
            String pn = e.getPropertyName();
            if (pn.equals(LINE_STYLE)) {
                decodeLineStyle(e.getNewValue());
            } else if (pn.equals("enabled")) {
                configureAppearanceContext(null);
            } else if (AquaFocusHandler.DISPLAY_AS_FOCUSED_KEY.equals(pn)) {
                configureAppearanceContext(null);
            } else if (isStyleProperty(pn)) {
                updateStriped();
            } else if (isViewStyleProperty(pn)) {
                updateInset();
            }
        }
    }

    class AquaTreeComponentListener extends ComponentAdapter {
        @Override
        public void componentResized(ComponentEvent e) {
            if (isCellFilled && treeState != null) {
                treeState.invalidateSizes();
            }
        }
    }

    /**
     * Paints the expand (toggle) part of a row. The receiver should NOT modify <code>clipBounds</code>, or
     * <code>insets</code>.
     */
    protected void paintExpandControl(Graphics g, Rectangle clipBounds, Insets insets, Rectangle bounds, TreePath path, int row, boolean isExpanded, boolean hasBeenExpanded, boolean isLeaf) {
        Object value = path.getLastPathComponent();

        // Draw icons if not a leaf and either hasn't been loaded,
        // or the model child count is > 0.
        if (isLeaf || (hasBeenExpanded && treeModel.getChildCount(value) <= 0)) return;

        // Both icons are the same size
        Icon icon = isExpanded ? getExpandedIcon() : getCollapsedIcon();
        if (icon != null && !(icon instanceof UIResource)) {
            super.paintExpandControl(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
            return;
        }

        int middleXOfKnob;
        boolean isLeftToRight = AquaUtils.isLeftToRight(tree); // Basic knows, but keeps it private
        if (isLeftToRight) {
            middleXOfKnob = bounds.x - getRightChildIndent() + 1;
        } else {
            middleXOfKnob = bounds.x + bounds.width + getRightChildIndent() - 1;
        }
        int middleYOfKnob = bounds.y + (bounds.height / 2);

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

        int x = middleXOfKnob - width / 2;
        int y = middleYOfKnob - height / 2;
        AquaUtils.configure(painter, tree, width, height);
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
    protected void drawCentered(Component c, Graphics g, Icon icon, int x, int y) {
        x = findCenteredX(x, icon.getIconWidth());
        y = y - icon.getIconHeight() / 2;

        if (icon instanceof ImageIcon) {
            ImageIcon ii = (ImageIcon) icon;
            Image image = ii.getImage();
            if (AquaImageFactory.isTemplateImage(image)) {
                Color iconColor = getIconColor();
                Image im = AquaImageFactory.getProcessedImage(image, iconColor);
                boolean isComplete = g.drawImage(im, x, y, c);
                if (!isComplete) {
                    new ImageIcon(im);
                    if (!g.drawImage(im, x, y, c)) {
                        Utils.logError("Button icon not drawn!");
                    }
                }
                return;
            }
        }

        icon.paintIcon(c, g, x, y);
    }

    protected @NotNull Color getIconColor() {
        return AquaColors.getSystemColor(tree, "expandControl");
    }

    private int findCenteredX(int x, int iconWidth) {
        return tree.getComponentOrientation().isLeftToRight()
                ? x - (int)Math.ceil(iconWidth / 2.0)
                : x - (int)Math.floor(iconWidth / 2.0);
    }

    @Override
    public Icon getCollapsedIcon() {
        Icon icon = super.getCollapsedIcon();
        if (AquaUtils.isLeftToRight(tree)) {
            return icon;
        }
        if (!(icon instanceof UIResource)) {
            return icon;
        }
        return UIManager.getIcon("Tree.rightToLeftCollapsedIcon");
    }

    protected State getState(TreePath path) {
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
    protected void handleExpandControlClick(TreePath path, int mouseX, int mouseY) {
        fMouseHandler = new TreeArrowMouseInputHandler(path);
    }

    public void handleExpandControlClick(@NotNull TreePath path) {
        fMouseHandler = new TreeArrowMouseInputHandler(path);
    }

    /**
     * Returning true signifies a mouse event on the node should toggle the selection of only the row under mouse.
     */
    protected boolean isToggleSelectionEvent(MouseEvent event) {
        return SwingUtilities.isLeftMouseButton(event) && event.isMetaDown();
    }

    protected AbstractLayoutCache.NodeDimensions createNodeDimensions() {
        return new TreeNodeDimensions();
    }

    protected AbstractLayoutCache createLayoutCache() {
        if(isLargeModel() && getRowHeight() > 0) {
            return new FixedHeightLayoutCache();
        }
        return new MyVariableHeightLayoutCache();
    }

    protected class MyVariableHeightLayoutCache
            extends ExtendedVariableHeightLayoutCache
    {
        @Override
        protected int getRowSpacingAbove(int row) {
            return getStyleRowSpacingAbove(row);
        }
    }

    private int getStyleRowSpacingAbove(int row) {
        if (isSideBar()) {
            TreePath path = getPathForRow(tree, row);
            if (path != null && path.getPathCount() == 2) {
                if (OSXSystemProperties.OSVersion >= 1016) {
                    return 14;
                }
                return 9;
            }
        }
        return 0;
    }

    private int getStyleRowHeight(int depth) {
        if (isSideBar()) {
            boolean isCategory = depth == 1;
            if (isInset()) {
                return isCategory ? 18 : 28;
            } else {
                return isCategory ? 22 : 24;
            }
        }
        return 0;
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
        public void focusGained(FocusEvent e) {
            super.focusGained(e);
            focusChanged();
        }

        public void focusLost(FocusEvent e) {
            super.focusLost(e);
            focusChanged();
        }

        private void focusChanged() {
            configureAppearanceContext(null);
        }
    }

    public class MacPropertyChangeHandler extends PropertyChangeHandler {
        public void propertyChange(PropertyChangeEvent evt) {
            String name = evt.getPropertyName();
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

                if (name.equals("ancestor")) {
                    updateSidebar();
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

        TreeArrowMouseInputHandler(TreePath path) {
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
//                    Component p = tree.getParent();
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
            Rectangle boundsBuffer = new Rectangle();
            fBounds = getPathBounds(fTrackingPath, fInsets, boundsBuffer);

            paintOneControl();
        }

        public void mouseDragged(MouseEvent e) {
            fIsInBounds = fPathBounds.contains(e.getX(), e.getY());
            paintOneControl();
        }

        @Override
        public void mouseExited(MouseEvent e) {
            fIsInBounds = fPathBounds.contains(e.getX(), e.getY());
            paintOneControl();
        }

        public void mouseReleased(MouseEvent e) {
            if (tree == null) return;

            if (fIsPressed) {
                boolean wasInBounds = fIsInBounds;

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

        protected void paintAnimation(boolean expanding) {
            paintAnimationFrame(0);
            paintAnimationFrame(0.5f);
            paintAnimationFrame(1);
            fAnimationTransition = -1;
        }

        protected void paintAnimationFrame(float transition) {
            fAnimationTransition = transition;
            paintOneControl();
            try { Thread.sleep(20); } catch (InterruptedException e) { }
        }

        // Utility to paint just one widget while it's being tracked
        // Just doing "repaint" runs into problems if someone does "translate" on the graphics
        // (ie, Sun's JTreeTable example, which is used by Moneydance - see Radar 2697837)
        void paintOneControl() {
            if (tree == null) return;
            Graphics g = tree.getGraphics();
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
                TreePath parentPath = fTrackingPath.getParentPath();
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

    protected int getRowForPath(TreePath path) {
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
    protected Rectangle getPathArrowBounds(TreePath path) {
        Rectangle bounds = getPathBounds(tree, path); // Gives us the y values, but x is adjusted for the contents
        Insets i = tree.getInsets();

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
        private final boolean expand;
        private final boolean recursive;

        /**
         * True if the selection is reset, false means only the lead path changes.
         */
        public KeyboardExpandCollapseAction(boolean expand, boolean recursive) {
            this.expand = expand;
            this.recursive = recursive;
        }

        public void actionPerformed(ActionEvent e) {
            if (tree == null || 0 > getRowCount(tree)) return;

            TreePath[] selectionPaths = tree.getSelectionPaths();
            if (selectionPaths == null) return;

            for (int i = selectionPaths.length - 1; i >= 0; i--) {
                TreePath path = selectionPaths[i];

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
                    TreePath parentPath = path.getParentPath();
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

    private void expandNode(int row, boolean recursive) {
        TreePath path = getPathForRow(tree, row);
        if (path == null) return;

        tree.expandPath(path);
        if (!recursive) return;

        expandAllNodes(path, row + 1);
    }

    private void expandAllNodes(TreePath parent, int initialRow) {
        for (int i = initialRow; true; i++) {
            TreePath path = getPathForRow(tree, i);
            if (!parent.isDescendant(path)) return;

            tree.expandPath(path);
        }
    }

    private void collapseNode(int row, boolean recursive) {
        TreePath path = getPathForRow(tree, row);
        if (path == null) return;

        if (recursive) {
            collapseAllNodes(path, row + 1);
        }

        tree.collapsePath(path);
    }

    private void collapseAllNodes(TreePath parent, int initialRow) {
        int lastRow = -1;
        for (int i = initialRow; lastRow == -1; i++) {
            TreePath path = getPathForRow(tree, i);
            if (!parent.isDescendant(path)) {
                lastRow = i - 1;
            }
        }

        for (int i = lastRow; i >= initialRow; i--) {
            TreePath path = getPathForRow(tree, i);
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
