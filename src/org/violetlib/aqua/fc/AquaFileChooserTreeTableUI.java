/*
 * Copyright (c) 2014-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import org.violetlib.aqua.AquaTreeTableUI;
import org.violetlib.aqua.TreeTableModel;
import org.violetlib.treetable.TreeTable;

import javax.swing.*;
import javax.swing.tree.TreePath;
import java.awt.event.MouseEvent;
import java.io.File;

/**
 * Customize the tree table UI for use in the file chooser.
 */
public abstract class AquaFileChooserTreeTableUI extends AquaTreeTableUI {

    protected AquaFileChooserListMouseBehavior mouseBehavior;

    public AquaFileChooserTreeTableUI(JFileChooser fc, TreeTable tt) {
        mouseBehavior = new AquaFileChooserListMouseBehavior(fc, new TreeTableModel(tt));
        mouseBehavior.setFileSelectionHandler(new AquaFileChooserListMouseBehavior.FileSelectionHandler() {
            @Override
            public void fileSelected(File f) {
                SubtreeTreeModel model = (SubtreeTreeModel) tt.getTreeModel();
                FileSystemTreeModel fullModel = (FileSystemTreeModel) model.getTargetModel();
                TreePath path = fullModel.toPath(f, null);
                AquaFileChooserTreeTableUI.this.select(path);
            }
        });
    }

    @Override
    protected JTree createAndConfigureTree() {
        JTree tree = super.createAndConfigureTree();
        tree.putClientProperty("JTree.isCellFilled", true);
        return tree;
    }

    @Override
    protected JTable createAndConfigureTable() {
        JTable table = super.createAndConfigureTable();
        table.putClientProperty("JTable.style", "striped");
        return table;
    }

    @Override
    protected Handler createHandler() {
        return new MyHandler();
    }

    @Override
    protected void installListeners() {
        super.installListeners();

        // Avoid conflict with Cmd-Shift-A in the file chooser
        {
            JTable table = getTable();
            InputMap map = table.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).getParent();
            KeyStroke ks = KeyStroke.getKeyStroke("shift meta A");
            Object v = map.get(ks);
            if (v != null && v.equals("clearSelection")) {
                InputMap newMap = new InputMap();
                newMap.setParent(map);
                newMap.put(ks, "selectApplicationsFolder"); // dummy name for now
                SwingUtilities.replaceUIInputMap(table, JComponent.WHEN_FOCUSED, newMap);
            }
        }

        {
            JTree tree = getTree();
            InputMap map = tree.getInputMap(JComponent.WHEN_FOCUSED).getParent();
            KeyStroke ks = KeyStroke.getKeyStroke("shift meta A");
            Object v = map.get(ks);
            if (v != null && v.equals("clearSelection")) {
                InputMap newMap = new InputMap();
                newMap.setParent(map);
                newMap.put(ks, "selectApplicationsFolder"); // dummy name for now
                SwingUtilities.replaceUIInputMap(tree, JComponent.WHEN_FOCUSED, newMap);
            }
        }
    }

    protected class MyHandler extends AquaTreeTableUI.MyHandler {

        @Override
        protected void dispatchMouseEvent(MouseEvent e, JComponent c) {

            int id = e.getID();
            if (id == MouseEvent.MOUSE_CLICKED && e.getClickCount() == 2) {
                TreePath path = treeTable.getPathForLocation(e.getX(), e.getY());
                if (path != null) {
                    handleDoubleClick(path);
                    e.consume();
                    return;
                }
            }

            JTable table = getTable();
            if (c == table) {
                mouseBehavior.processMouseEvent(e);
                // Consuming the event will prevent mouse dragged events from being dispatched here.
                // e.consume();
                return;
            }

            super.dispatchMouseEvent(e, c);
        }
    }

    protected void handleDoubleClick(TreePath path) {
        Object node = path.getLastPathComponent();
        if (node instanceof FileInfo) {
            FileInfo info = (FileInfo) node;
            if (info.isAcceptable() || info.isTraversable()) {
                select(path);
            }
        }
    }

    protected abstract void select(TreePath path);
}
