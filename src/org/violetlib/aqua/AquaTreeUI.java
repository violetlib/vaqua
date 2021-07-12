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
import java.util.Objects;
import java.util.TooManyListenersException;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.*;

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

    private static final int DEFAULT_INDENTATION = 16;
    private static final int SIDEBAR_INDENTATION = 13;

    private final FocusListener editingComponentFocusListener = new EditingComponentFocusListener();
    private final ComponentListener componentListener = new AquaTreeComponentListener();
    private final TreeModelListener treeModelListener = new AquaTreeModelListener();
    private Component editorFocusOwner;
    private @Nullable TreeCellEditor originalTreeCellEditor;

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
    protected @Nullable Boolean _isShallowSideBar;
    protected int indentationPerLevel = DEFAULT_INDENTATION;

    // state variables for painting, needed because we share painting implementation with the superclass
    protected boolean shouldPaintSelection;
    protected boolean isActive;     // for communication between paint() and paintRow()

    // state variables needed for cell renderer configuration
    private Font oldCellRendererFont;
    private Icon oldCellRendererIcon;
    private Icon oldCellRendererDisabledIcon;

    // support for sidebar presentation
    private SidebarVibrantEffects sidebarVibrantEffects;

    private @Nullable TreePath pathWithVisibleExpandControl;

    private static DropTargetListener defaultDropTargetListener = null;

    protected @NotNull ContainerContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    public AquaTreeUI() {
        this.colors = AquaColors.CONTAINER_COLORS;
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();

        LookAndFeel.installBorder(tree, "Tree.border");

        // Unfortunately, there is no reliable way to undo this change because we will not know if the application
        // later tries to set it to false. Fortunately, swapping LAFs is unusual and displaying a root node is dumb.
        tree.setRootVisible(false);
        // Too bad this does not work:
        // LookAndFeel.installProperty(tree, JTree.ROOT_VISIBLE_PROPERTY, false);
        tree.putClientProperty(AquaCellEditorPolicy.IS_CELL_CONTAINER_PROPERTY, true);

        // The following ensures that transferring focus away from a cell editor will save the changes
        tree.setInvokesStopCellEditing(true);

        originalTreeCellEditor = null;
        cellEditorChanged();

        isStriped = getStripedValue();
        isInset = getInsetValue();
        isSideBar = getSideBarValue();
        isCellFilled = getCellFilledValue();

        tree.repaint();
        if (treeState != null) {
            treeState.invalidateSizes();
        }
        colors = determineColors();
        updateInset();
        updateSideBarConfiguration();
        configureAppearanceContext(null);
    }

    protected void cellEditorChanged() {
        if (originalTreeCellEditor == null) {
            TreeCellEditor editor = tree.getCellEditor();
            if (editor == null || editor.getClass() == DefaultCellEditor.class) {
                // If not defined or not subclassed, substitute a custom cell editor for painting the icon.
                installSpecialCellEditorIfAppropriate();
            }
        }
    }

    protected void cellRendererChanged() {
        installSpecialCellEditorIfAppropriate();
    }

    protected void installSpecialCellEditorIfAppropriate() {
        // Install a special cell editor for painting the icon obtained from the cell renderer.
        // The icon is available only if the cell renderer is a DefaultTreeCellRenderer.
        TreeCellRenderer renderer = tree.getCellRenderer();
        if (renderer instanceof DefaultTreeCellRenderer) {
            DefaultTreeCellRenderer r = (DefaultTreeCellRenderer) renderer;
            if (originalTreeCellEditor == null) {
                originalTreeCellEditor = tree.getCellEditor();
            }
            tree.setCellEditor(new AquaTreeCellEditor(tree, r));
        }
    }

    @Override
    protected void uninstallDefaults() {
        TreeCellEditor editor = tree.getCellEditor();
        if (editor instanceof AquaTreeCellEditor) {
            tree.setCellEditor(originalTreeCellEditor);
        }
        originalTreeCellEditor = null;

        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        AppearanceManager.installListeners(tree);
        tree.addComponentListener(componentListener);
        TreeModel model = tree.getModel();
        if (model != null) {
            model.addTreeModelListener(treeModelListener);
        }
    }

    @Override
    protected void uninstallListeners() {
        TreeModel model = tree.getModel();
        if (model != null) {
            model.removeTreeModelListener(treeModelListener);
        }
        tree.removeComponentListener(componentListener);
        AppearanceManager.uninstallListeners(tree);
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

    protected void updateRowHeight() {
        // sidebar trees have two row heights, one for ordinary items and one for category headers
        int height = isSideBar() ? 0 : 19;
        LookAndFeel.installProperty(tree, "rowHeight", height);
    }

    // Called on a change to isSideBar or the ancestry of the tree
    protected void updateSideBarConfiguration() {
        if (isSideBar() && tree.isDisplayable()) {
            ensureSidebarVibrantEffects();
        } else {
            disposeSidebarVibrantEffects();
        }

        if (isSideBar()) {
            indentationPerLevel = SIDEBAR_INDENTATION;
        } else {
            indentationPerLevel = DEFAULT_INDENTATION;
        }
        int left = (5 * indentationPerLevel) / 12;
        setLeftChildIndent(left);
        setRightChildIndent(indentationPerLevel - left);

        // On macOS 11+, the sidebar style implies the inset view style.
        if (AquaUtils.isInsetViewSupported()) {
            updateRowHeight();
            tree.revalidate();
            tree.repaint();
            updateCellSizes();
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

    protected @NotNull JComponent getComponentForVisualEffectView() {
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

    private void updateCellFilled() {
        boolean cellFilledValue = getCellFilledValue();
        if (cellFilledValue != isCellFilled) {
            isCellFilled = cellFilledValue;
            if (tree != null) {
                tree.repaint();
                if (treeState != null) {
                    treeState.invalidateSizes();
                }
            }
        }
    }

    private boolean getCellFilledValue() {
        return tree != null
                && Boolean.TRUE.equals(AquaUtils.getBooleanProperty(tree, IS_CELL_FILLED_KEY, QUAQUA_IS_CELL_FILLED_KEY));
    }

    private void updateStyleProperties() {
        boolean isStripedChanged = false;
        boolean stripedValue = getStripedValue();
        if (stripedValue != isStriped) {
            isStriped = stripedValue;
            isStripedChanged = true;
        }
        boolean isSideBarChanged = false;
        boolean sideBarValue = getSideBarValue();
        if (sideBarValue != isSideBar) {
            isSideBar = sideBarValue;
            isSideBarChanged = true;
        }
        if (isStripedChanged || isSideBarChanged) {
            colors = determineColors();
            configureAppearanceContext(null);
            tree.repaint();
        }
        if (isSideBarChanged) {
            updateSideBarConfiguration();
        }
    }

    private boolean getStripedValue() {
        String value = getStyleProperty();
        return "striped".equals(value) && isBackgroundClear();
    }

    private boolean getSideBarValue() {
        String style = getStyleProperty();
        return style != null && (style.equals("sideBar") || style.equals("sourceList"));
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
            updateCellSizes();
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

    public boolean isSideBar() {
        return isSideBar;
    }

    public boolean isShallowSideBar() {
        if (isSideBar()) {
            if (_isShallowSideBar == null) {
                _isShallowSideBar = computeIsShallowSideBar();
            }
            return _isShallowSideBar;
        }
        return false;
    }

    protected boolean computeIsShallowSideBar() {
        if (tree != null) {
            TreeModel model = tree.getModel();
            Object root = model.getRoot();
            int categoryCount = model.getChildCount(root);
            for (int i = 0; i < categoryCount; i++) {
                Object category = model.getChild(root, i);
                int itemCount = model.getChildCount(category);
                for (int j = 0; j < itemCount; j++) {
                    Object item = model.getChild(category, j);
                    if (!model.isLeaf(item)) {
                        return false;
                    }
                }
            }
        }
        return true;
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
    public void checkForClickInExpandControl(TreePath path, int mouseX, int mouseY) {
        super.checkForClickInExpandControl(path, mouseX, mouseY);
    }

    public void mouseMoved(@Nullable MouseEvent e) {
        if (isSideBar()) {
            if (e != null) {
                TreePath path = getPathForYLocation(e.getY());
                if (path != null) {
                    if (isCategory(path)) {
                        updateCategoryExpandControlVisibility(path);
                        return;
                    }
                }
            }
            updateCategoryExpandControlVisibility(null);
        }
    }

    protected void updateCategoryExpandControlVisibility(@Nullable TreePath path) {
        if (!Objects.equals(path, pathWithVisibleExpandControl)) {
            pathWithVisibleExpandControl = path;
            tree.repaint();
        }
    }

    protected boolean useTrailingExpandControl(@NotNull TreePath path) {
        return isCategory(path);
    }

    protected boolean useTrailingExpandControl(int row) {
        if (isSideBar()) {
            TreePath path = getPathForRow(tree, row);
            return path != null && isCategory(path);
        }
        return false;
    }

    @Override
    public void startEditingAtPath(JTree tree, TreePath path) {
        if (AquaTreeMouseBehavior.isDebug) {
            Utils.logDebug("JTree editing cell " + path);
        }
        super.startEditingAtPath(tree, path);
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

    @Override
    public TreePath getClosestPathForLocation(@Nullable JTree treeLocal, int x, int y) {
        if (treeLocal == null || treeState == null) {
            return null;
        }
        Insets i = treeLocal.getInsets();
        return treeState.getPathClosestTo(x - i.left, y - i.top);
    }

    protected @Nullable TreePath getPathForYLocation(int y) {
        if (treeState != null) {
            Insets s = tree.getInsets();
            TreePath path = treeState.getPathClosestTo(s.left, y - s.top);
            if (path != null) {
                Rectangle bounds = getPathBounds(path, s, null);
                if (bounds != null && y >= bounds.y && y <= (bounds.y + bounds.height)) {
                    return path;
                }
            }
        }
        return null;
    }

    public boolean isCategory(int depth) {
        return isSideBar() && depth == 1;
    }

    public boolean isCategory(@NotNull TreePath path) {
        return isSideBar() && path.getPathCount() == 2;
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
                int width = tree.getWidth() - (s.left + s.right) - r.x;
                if (isCategory(depth)) {
                    width -= 5;
                }
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

                boolean isCategory = isCategory(depth);
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
     * This method returns the leading indentation for a cell.
     * Customize the indentation for the sidebar, to take into account that category rows have no icons.
     * Also take into account the use of a trailing expand control, which further reduces the indentation.
     */
    @Override
    protected int getRowX(int row, int depth) {
        int fudge = 0;
        if (isSideBar()) {
            depth--;
            if (OSXSystemProperties.OSVersion < 1016) {
                fudge = -3;
            } else if (isShallowSideBar()) {
                depth--;
            }
        }
        if (useTrailingExpandControl(row)) {
            depth--;
        }
        depth = Math.max(0, depth);
        return indentationPerLevel * (depth + depthOffset) + fudge;
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
        return isSideBar() ? null : tree.getBackground();
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
        assert appearanceContext != null;
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
                if (false) {
                    // Not needed, these colors are used when the component obtained
                    treeCellRenderer.setTextNonSelectionColor(colors.getForeground(appearanceContext.withSelected(false)));
                    treeCellRenderer.setTextSelectionColor(colors.getForeground(appearanceContext.withSelected(true)));
                }
            }
        }

        if (component instanceof JLabel) {
            JLabel label = (JLabel) component;

            // Ideally, we configure the same set of attributes each time. However, we cannot configure the icon except
            // in the case where we want to set it to null. Thus, we save and restore the icon in every case.

            oldCellRendererFont = label.getFont();
            oldCellRendererIcon = label.getIcon();
            oldCellRendererDisabledIcon = label.getDisabledIcon();

            Color fc = appearanceContext != null ? colors.getForeground(appearanceContext) : null;
            Font f = null;
            Icon icon = oldCellRendererIcon;

            if (isSideBar()) {
                if (isCategory) {
                    label.setIcon(null);
                    label.setDisabledIcon(null);
                    icon = null;
                }
                fc = getSideBarForeground(isCategory, isSelected);
                Font sf = getSideBarFont(isCategory, isSelected);
                if (sf != null) {
                    // Trick the renderer into accepting the font. It ignores instances of FontUIResource.
                    f = fixFont(sf);
                }
            }

            if (!isLayout) {
                if (icon != null) {
                    icon = convertIcon(isCategory, isSelected, icon);
                }
                if (icon != oldCellRendererIcon) {
                    label.setIcon(icon);
                }
                if (fc != null) {
                    Color existingForeground = label.getForeground();
                    if (existingForeground == null || existingForeground instanceof UIResource) {
                        label.setForeground(fc);
                    }
                }
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

    public void paintEditorIcon(@NotNull Graphics g, @NotNull TreePath path, @NotNull Icon icon, int x, int y) {
        // If the icon is a template icon, paint it using an appropriate color.
        // No icon is painted for a category header in a sidebar tree.
        boolean isCategory = isCategory(path);
        boolean isSelected = tree.isPathSelected(path);
        icon = convertIcon(isCategory, isSelected, icon);
        if (icon != null) {
            icon.paintIcon(tree, g, x, y);
        }
    }

    protected @Nullable Icon convertIcon(boolean isCategory, boolean isSelected, @NotNull Icon icon) {
        // If the icon is a template icon, convert it to an image icon using an appropriate color.
        // No icon should be painted for a category header in a sidebar tree.

        if (isCategory && isSideBar()) {
            return null;
        }

        if (AquaImageFactory.isTemplateIcon(icon)) {
            Object operator = getOperatorForTemplateIcon(isSelected);
            if (operator != null) {
                Image image = AquaImageFactory.getProcessedImage(icon, operator);
                if (image != null) {
                    return new ImageIcon(image);
                }
            }
        }
        return icon;
    }

    protected @Nullable Object getOperatorForTemplateIcon(boolean isSelected) {
        AquaAppearance appearance = appearanceContext != null ? appearanceContext.getAppearance() : null;
        if (isSideBar()) {
            if (OSXSystemProperties.OSVersion >= 1016 && appearance != null) {
                if (AquaFocusHandler.isActive(tree)) {
                    if (appearance.isDark()) {
                        return appearance.getColor("controlAccent");
                    } else {
                        return appearance.getColor("controlAccent_pressed");
                    }
                } else {
                    return appearance.getColor("controlAccent_disabled");
                }
            } else {
                Color iconColor = null;
                if (appearance != null) {
                    iconColor = appearance.getColor("sidebarIcon");
                }
                if (iconColor != null) {
                    return iconColor;
                }
                return AquaFocusHandler.isActive(tree) ? null : LIGHTEN_FOR_DISABLED;
            }
        } else {
            // For best results over a selection background, use the corresponding text color
            if (isSelected && appearanceContext != null && AquaFocusHandler.isActive(tree)) {
                AppearanceContext ac = appearanceContext.withSelected(true);
                return colors.getForeground(ac);
            }
            return appearance != null ? appearance.getColor("treeIcon") : null;
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

    @Override
    protected void paintVerticalPartOfLeg(Graphics g, Rectangle clipBounds, Insets insets, TreePath path) {
    }

    @Override
    protected void paintHorizontalPartOfLeg(Graphics g, Rectangle clipBounds, Insets insets, Rectangle bounds,
                                            TreePath path, int row, boolean isExpanded, boolean hasBeenExpanded,
                                            boolean isLeaf) {
    }

    private void treeModelChanged(@Nullable Object oldModel, @Nullable Object newModel) {
        if (oldModel instanceof TreeModel) {
            TreeModel model = (TreeModel) oldModel;
            model.removeTreeModelListener(treeModelListener);
        }
        if (newModel instanceof TreeModel) {
            TreeModel model = (TreeModel) newModel;
            model.addTreeModelListener(treeModelListener);
        }
        _isShallowSideBar = null;
    }

    class AquaTreeComponentListener extends ComponentAdapter {
        @Override
        public void componentResized(ComponentEvent e) {
            if (isCellFilled && treeState != null) {
                treeState.invalidateSizes();
            }
        }
    }

    class AquaTreeModelListener implements TreeModelListener {
        @Override
        public void treeNodesChanged(TreeModelEvent e) {
            treeModelChanged();
        }

        @Override
        public void treeNodesInserted(TreeModelEvent e) {
            treeModelChanged();
        }

        @Override
        public void treeNodesRemoved(TreeModelEvent e) {
            treeModelChanged();
        }

        @Override
        public void treeStructureChanged(TreeModelEvent e) {
            treeModelChanged();
        }

        private void treeModelChanged() {
            _isShallowSideBar = null;
        }
    }

    // This method made public so that AquaTreeMouseBehavior can access it.
    @Override
    public boolean isLocationInExpandControl(TreePath path, int mouseX, int mouseY) {
        if (path != null && !treeModel.isLeaf(path.getLastPathComponent())) {
            Point center = getExpandControlCenter(path);
            if (center != null) {
                return Math.abs(mouseX - center.x) < 8;
            }
        }
        return false;
    }

    /**
     * Paints the expand (toggle) part of a row. The receiver should NOT modify <code>clipBounds</code>, or
     * <code>insets</code>.
     */
    protected void paintExpandControl(@NotNull Graphics g,
                                      @NotNull Rectangle clipBounds,
                                      @NotNull Insets insets, Rectangle bounds,
                                      @NotNull TreePath path,
                                      int row,
                                      boolean isExpanded,
                                      boolean hasBeenExpanded,
                                      boolean isLeaf) {
        Object value = path.getLastPathComponent();

        // Draw icons if not a leaf and either hasn't been loaded, or the model child count is > 0.
        if (isLeaf || (hasBeenExpanded && treeModel.getChildCount(value) <= 0)) {
            return;
        }

        if (isCategory(path) && !path.equals(pathWithVisibleExpandControl)) {
            return;
        }

        Point center = getExpandControlCenter(path);
        if (center != null) {
            drawExpandControl(g, path, isExpanded, center);
        }
    }

    protected void drawExpandControl(@NotNull Graphics g,
                                     @NotNull TreePath path,
                                     boolean isExpanded,
                                     @NotNull Point center) {
        // Both icons are the same size
        Icon icon = isExpanded ? getExpandedIcon() : getCollapsedIcon();
        if (icon != null && !(icon instanceof UIResource)) {
            drawCentered(tree, g, icon, center.x, center.y);
        } else {
            State state = getState(path);
            if (!fIsInBounds && state == State.PRESSED) {
                state = State.ACTIVE;
            }
            boolean isLTR = AquaUtils.isLeftToRight(tree);
            Configuration tg = getDisclosureTriangleConfiguration(state, isExpanded, isLTR);
            LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo((LayoutConfiguration) tg);
            int width = (int) Math.ceil(layoutInfo.getFixedVisualWidth());
            int height = (int) Math.ceil(layoutInfo.getFixedVisualHeight());
            if (width == 0) {
                width = 20;
            }
            if (height == 0) {
                height = width;
            }
            int x = center.x - width / 2;
            int y = center.y - height / 2;
            AquaUtils.configure(painter, tree, width, height);
            painter.getPainter(tg).paint(g, x, y);
        }
    }

    protected @Nullable Point getExpandControlCenter(@NotNull TreePath path) {
        boolean isLTR = AquaUtils.isLeftToRight(tree);
        Insets s = tree.getInsets();
        Rectangle bounds = getPathBounds(path, s, null);
        if (bounds == null) {
            return null;
        }
        int y = bounds.y + (bounds.height / 2);

        int indentation = getRowX(tree.getRowForPath(path), path.getPathCount() - 1);
        int x;
        if (isLTR) {
            if (useTrailingExpandControl(path)) {
                x = tree.getWidth() - s.right + getLeftChildIndent() - 1;
            } else {
                x = indentation + s.left - getRightChildIndent() + 1;
            }
        } else {
            if (useTrailingExpandControl(path)) {
                x = s.left - getLeftChildIndent() + 1;
            } else {
                x = tree.getWidth() - (indentation + s.right) + getRightChildIndent() - 1;
            }
        }

        return new Point(x, y);
    }

    protected @Nullable Rectangle getExpandControlBounds(@NotNull TreePath path) {
        Point center = getExpandControlCenter(path);
        if (center != null) {
            Rectangle bounds = getPathBounds(path, tree.getInsets(), null);
            if (bounds != null) {
                return new Rectangle(center.x - 8, bounds.y, 16, bounds.height);
            }
        }
        return null;
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

    public @NotNull Insets getInsets() {
        boolean isInset = isInset();
        boolean isSideBar = isSideBar();
        int top = 0;
        int bottom = 0;
        int leading = isInset ? (isSideBar ? 18 : 10) : (isSideBar ? 9 : 1);
        int trailing = isInset ? 20 : 1;
        boolean isLTR = AquaUtils.isLeftToRight(tree);
        if (isLTR) {
            return new Insets(top, leading, bottom, trailing);
        } else {
            return new Insets(top, trailing, bottom, leading);
        }
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

    @Override
    protected void toggleExpandState(TreePath path) {
        // In a sidebar tree, collapsing a category header should not transfer the selection to the category
        // header, which would normally happen if a descendant of the category header is selected.
        // The workaround is to first remove any descendants from the selection.

        if (tree.isExpanded(path) && isCategory(path)) {
            TreePath[] paths = tree.getSelectionPaths();
            if (paths != null) {
                for (TreePath selectedPath : paths) {
                    if (path.isDescendant(selectedPath)) {
                        tree.removeSelectionPath(selectedPath);
                    }
                }
            }
        }

        super.toggleExpandState(path);
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

    protected void updateCellSizes() {
        int h = Math.max(tree.getRowHeight(), -1);
        LookAndFeel.installProperty(tree, "rowHeight", h + 1);
        LookAndFeel.installProperty(tree, "rowHeight", h);
        updateSize();
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
            boolean isCategory = isCategory(depth);
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

    public class MacPropertyChangeHandler
            extends PropertyChangeHandler {
        public void propertyChange(@NotNull PropertyChangeEvent e) {
            String pn = e.getPropertyName();
            if (pn == null || e.getSource() != tree) {
                return;
            }

            // This case duplicates BasicTreeUI because we have our own ignoreLAChange variable.
            if (pn.equals(JTree.LEAD_SELECTION_PATH_PROPERTY) && ignoreLAChange) {
                return;
            }

            // This case duplicates BasicTreeUI because we have our own ignoreLAChange variable.
            if (pn.equals(JTree.ANCHOR_SELECTION_PATH_PROPERTY) && ignoreLAChange) {
                return;
            }

            if (pn.equals("enabled")) {
                configureAppearanceContext(null);
            } else if (AquaFocusHandler.DISPLAY_AS_FOCUSED_KEY.equals(pn)) {
                configureAppearanceContext(null);
                return;
            } else if (isStyleProperty(pn)) {
                updateStyleProperties();
                return;
            } else if (isViewStyleProperty(pn)) {
                updateInset();
                return;
            } else if (isCellFilledProperty(pn)) {
                updateCellFilled();
                return;
            } else if (pn.equals("transferHandler")) {
                updateDropTargetListener();
                return;
            } else if (pn.equals(JTree.CELL_EDITOR_PROPERTY)) {
                cellEditorChanged();
            } else if (pn.equals(JTree.CELL_RENDERER_PROPERTY)) {
                cellRendererChanged();
            } else if (pn.equals(JTree.TREE_MODEL_PROPERTY)) {
                treeModelChanged(e.getOldValue(), e.getNewValue());
            } else if (pn.equals("ancestor")) {
                updateSideBarConfiguration();
            }

            super.propertyChange(e);
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
        protected Rectangle fPathBounds;

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
            this.fPathBounds = getExpandControlBounds(path);
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
            fIsInBounds = fPathBounds != null && fPathBounds.contains(e.getX(), e.getY());
            paintOneControl();
        }

        @Override
        public void mouseExited(MouseEvent e) {
            fIsInBounds = fPathBounds != null && fPathBounds.contains(e.getX(), e.getY());
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
//                TreePath parentPath = fTrackingPath.getParentPath();
//                if (parentPath != null) {
//                    paintVerticalPartOfLeg(g, fPathBounds, fInsets, parentPath);
//                    paintHorizontalPartOfLeg(g, fPathBounds, fInsets, fBounds, fTrackingPath, fTrackingRow, fIsExpanded, fHasBeenExpanded, fIsLeaf);
//                } else if (isRootVisible() && fTrackingRow == 0) {
//                    paintHorizontalPartOfLeg(g, fPathBounds, fInsets, fBounds, fTrackingPath, fTrackingRow, fIsExpanded, fHasBeenExpanded, fIsLeaf);
//                }
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

    private @Nullable Rectangle getPathBounds(TreePath path, Insets insets, Rectangle bounds) {
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
            if (selectionPaths == null) {
                return;
            }

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
                        if (!isCategory(parentPath)) {
                            tree.setSelectionPath(parentPath);
                        }
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
            if (!parent.isDescendant(path)) {
                return;
            }
            tree.expandPath(path);
        }
    }

    private void collapseNode(int row, boolean recursive) {
        TreePath path = getPathForRow(tree, row);
        if (path == null) {
            return;
        }
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
