/*
 * Copyright (c) 2014-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.plaf.TableUI;
import javax.swing.tree.TreePath;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.treetable.TreeTable;
import org.violetlib.treetable.TreeTableCellRenderer;
import org.violetlib.treetable.ui.BasicTreeTableUI;

import static org.violetlib.aqua.AquaFocusHandler.HAS_FOCUS_DELEGATE_KEY;

/**
 * Customize the tree table UI. The internal tree and table components should not be focusable themselves, but they
 * should use inactive colors based on the focus state of the tree table. The table (not the tree) is fully responsible
 * for painting the selection background. The selection background must be repainted when the focus state changes.
 */
public class AquaTreeTableUI extends BasicTreeTableUI implements AquaComponentUI {

    protected MyHandler handler;

    public AquaTreeTableUI() {
        handler = new MyHandler();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        AppearanceManager.installListeners(treeTable);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListeners(treeTable);
        super.uninstallListeners();
    }

    @Override
    protected Handler createHandler() {
        return handler;
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        JTable table = getTable();
        JTree tree = getTree();
        AquaFocusHandler.updateComponentTreeUIActivation(table, isActive);
        AquaFocusHandler.updateComponentTreeUIActivation(tree, isActive);
        table.repaint();
        tree.repaint();
    }

    protected AquaUIPainter.State getState() {
        return treeTable.isEnabled()
          ? (AquaFocusHandler.hasFocus(treeTable) ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE)
          : AquaUIPainter.State.DISABLED;
    }

    protected boolean computeStriped() {
        JTable t = getTable();
        AquaTableUI ui = AquaUtils.getUI(t, AquaTableUI.class);
        return ui != null && ui.isStriped();
    }

    @Override
    protected JTree createAndConfigureTree() {
        JTree tree = super.createAndConfigureTree();
        tree.setFocusable(false);
        // Let the focus state of the tree table determine when to use inactive colors in the tree.
        tree.putClientProperty(HAS_FOCUS_DELEGATE_KEY, treeTable);
        // Prevent the tree from painting selection backgrounds.
        tree.putClientProperty("JTree.paintSelectionBackground", false);
        return tree;
    }

    @Override
    protected JTable createAndConfigureTable() {
        JTable table = super.createAndConfigureTable();
        table.setFocusable(false);
        // Let the focus state of the tree table determine when to use inactive colors in the tree.
        table.putClientProperty(HAS_FOCUS_DELEGATE_KEY, treeTable);
        return table;
    }

    @Override
    protected void finishConfiguration(JTree tree, JTable table) {
        // The tree does not paint the selection background, the table does.
        // If the tree is asked to repaint the selection, it should delegate to the table.
        // This is not actually needed at present, but who knows about the future?
        tree.putClientProperty("JTree.selectionRepainter", table);
    }

    @Override
    protected TreeTableCellRenderer createFocusRenderer() {
        return null;
    }

    @Override
    protected boolean hasTreeHandle(TreeTable treeTable, TreePath path) {
        return !treeTable.isLeaf(path);
    }

    @Override
    protected List<String> getProperties() {
        List<String> props = new ArrayList<String>(super.getProperties());
        props.add(AquaFocusHandler.FRAME_ACTIVE_PROPERTY);
        return props;
    }

    protected PropertyChangeListener createPropertyChangeListener() {
        return new MyPropertyChangeListener();
    }

    protected class MyPropertyChangeListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent event) {
            handler.propertyChange(event);
        }
    }

    protected class MyHandler extends Handler {

        @Override
        protected void focusChanged() {
            repaintSelection();
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            super.propertyChange(evt);
            if (AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(evt.getPropertyName())) {
                // Need to propagate manually because the tree and table are not child components
                boolean isActive = Boolean.TRUE.equals(evt.getNewValue());
                AquaFocusHandler.setActiveStatus(getTree(), isActive);
                AquaFocusHandler.setActiveStatus(getTable(), isActive);
            }
        }
    }

    public void repaintSelection() {
        TableUI tableUI = getTable().getUI();
        if (tableUI instanceof SelectionRepaintable) {
            SelectionRepaintable sp = (SelectionRepaintable) tableUI;
            sp.repaintSelection();
        }
    }

    protected void handleDoubleClick(TreePath path) {
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceSupport.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {
        AquaUIPainter.State state = getState();
        AppearanceContext appearanceContext = new AppearanceContext(pc.appearance, state, false, false);
        boolean isStriped = computeStriped();
        ContainerContextualColors colors = isStriped ? AquaColors.STRIPED_CONTAINER_COLORS : AquaColors.CONTAINER_COLORS;
        colors.configureForContainer();
        AquaColors.installColors(treeTable, appearanceContext, colors);
        super.paint(g, c);
    }
}
