/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.EventObject;

/**
 * Implements Aqua mouse behavior for a JTree. Based loosely on the mouse listener in BasicTreeUI.
 */
public class AquaTreeMouseBehavior extends MouseInputAdapter implements AquaDragRecognitionSupport.BeforeDrag {
    protected JTree tree;
    protected AquaTreeUI ui;

    private boolean mouseReleaseDeselects;
    private boolean mouseDragSelects;
    private boolean isMouseReleaseStartsEditing;
    private boolean isDragRecognitionOngoing;

    public AquaTreeMouseBehavior(JTree tree) {
        this.tree = tree;
        this.ui = (AquaTreeUI) tree.getUI();
    }

    public void dragStarting(MouseEvent me) {
    }

    public void mousePressed(MouseEvent e) {
        if (tree.isEnabled() && SwingUtilities.isLeftMouseButton(e) && !e.isConsumed()) {
            // if we can't stop any ongoing editing, do nothing
            if (ui.isEditing(tree) && tree.getInvokesStopCellEditing() && !ui.stopEditing(tree)) {
                return;
            }

            ui.completeEditing();

            // Note: Some applications depend on selection changes only occurring
            // on focused components. Maybe we must not do any changes to the
            // selection changes at all, when the component is not focused?
            if (tree.isRequestFocusEnabled()) {
                tree.requestFocusInWindow();
            }

            TreePath path = getMouseClickedClosestPathForLocation(tree, e.getX(), e.getY());

            // Check for clicks in expand control
            if (ui.isLocationInExpandControl(path, e.getX(), e.getY())) {
                ui.checkForClickInExpandControl(path, e.getX(), e.getY());
                return;
            }

            int index = tree.getRowForPath(path);

            mouseDragSelects = false;
            mouseReleaseDeselects = false;
            isMouseReleaseStartsEditing = true;
            isDragRecognitionOngoing = false;
            if (index != -1) {
                boolean isRowAtIndexSelected = tree.isRowSelected(index);
                if (isRowAtIndexSelected && e.isPopupTrigger()) {
                    // Do not change the selection, if the item is already
                    // selected, and the user triggers the popup menu.
                } else {
                    int anchorIndex = tree.getRowForPath(tree.getAnchorSelectionPath());

                    if ((e.getModifiersEx() & (MouseEvent.META_DOWN_MASK | MouseEvent.BUTTON2_DOWN_MASK | MouseEvent.BUTTON3_DOWN_MASK)) == MouseEvent.META_DOWN_MASK) {
                        if (isRowAtIndexSelected) {
                            tree.removeSelectionInterval(index, index);
                        } else {
                            tree.addSelectionInterval(index, index);
                            mouseDragSelects = true;
                            isMouseReleaseStartsEditing = false;
                        }
                    } else if ((e.getModifiersEx() & (MouseEvent.SHIFT_DOWN_MASK | MouseEvent.BUTTON2_DOWN_MASK | MouseEvent.BUTTON3_DOWN_MASK)) == MouseEvent.SHIFT_DOWN_MASK
                            && anchorIndex != -1) {
                        tree.setSelectionInterval(anchorIndex, index);
                        ui.setLeadSelectionPath(path);
                        mouseDragSelects = true;
                        isMouseReleaseStartsEditing = false;
                    } else if ((e.getModifiersEx() & (MouseEvent.SHIFT_DOWN_MASK | MouseEvent.META_DOWN_MASK)) == 0) {
                        if (isRowAtIndexSelected) {
                            if (tree.getDragEnabled()) {
                                isDragRecognitionOngoing = AquaDragRecognitionSupport.mousePressed(e);
                                mouseDragSelects = mouseReleaseDeselects = false;
                            } else {
                                mouseReleaseDeselects = tree.isFocusOwner();
                            }
                        } else {
                            tree.setSelectionInterval(index, index);
                            if (tree.getDragEnabled()
                                    && ui.getPathBounds(tree, path).contains(e.getPoint())) {
                                isDragRecognitionOngoing = AquaDragRecognitionSupport.mousePressed(e);
                                mouseDragSelects = mouseReleaseDeselects = false;
                                isMouseReleaseStartsEditing = false;
                            } else {
                                mouseDragSelects = true;
                                isMouseReleaseStartsEditing = false;
                            }
                        }
                        ui.setAnchorSelectionPath(path);
                        ui.setLeadSelectionPath(path);
                    }
                }
            }
        }
    }

    public void mouseDragged(MouseEvent e) {
        if (tree.isEnabled() && SwingUtilities.isLeftMouseButton(e) && !e.isConsumed()) {
            if (tree.getDragEnabled() && isDragRecognitionOngoing) {
                AquaDragRecognitionSupport.mouseDragged(e, this);
            }

            // Do nothing if we can't stop editing.
            if (ui.isEditing(tree) && tree.getInvokesStopCellEditing() && !ui.stopEditing(tree)) {
                return;
            }

            TreePath leadPath = ui.getClosestPathForLocation(tree, e.getX(), e.getY());

            // this is a dirty trick to reset the timer of the cell editor.
            if (tree.getCellEditor() != null) {
                tree.getCellEditor().isCellEditable(new EventObject(this));
            }

            mouseReleaseDeselects = false;
            isMouseReleaseStartsEditing = false;
            if (mouseDragSelects) {
                int index = tree.getRowForPath(leadPath);
                if (index != -1) {
                    Rectangle cellBounds = tree.getRowBounds(index);
                    tree.scrollRectToVisible(cellBounds);
                    TreePath anchorPath = tree.getAnchorSelectionPath();
                    int anchorIndex = tree.getRowForPath(anchorPath);
                    if (tree.getSelectionModel().getSelectionMode() == TreeSelectionModel.SINGLE_TREE_SELECTION) {
                        tree.setSelectionInterval(index, index);
                    } else {
                        if (anchorIndex < index) {
                            tree.setSelectionInterval(anchorIndex, index);
                        } else {
                            tree.setSelectionInterval(index, anchorIndex);
                        }
                        ui.setAnchorSelectionPath(anchorPath);
                        ui.setLeadSelectionPath(leadPath);
                    }
                }
            }
        }
    }

    /**
     * Invoked when the mouse button has been moved on a component
     * (with no buttons down).
     */
    public void mouseMoved(MouseEvent e) {
        isMouseReleaseStartsEditing = false;
        // this is a dirty trick to reset the timer of the cell editor.
        if (tree.getCellEditor() != null) {
            tree.getCellEditor().isCellEditable(new EventObject(this));
        }
    }

    public void mouseReleased(MouseEvent e) {
        if (tree.isEnabled()&& SwingUtilities.isLeftMouseButton(e) && !e.isConsumed()) {
            if (ui.isEditing(tree) && tree.getInvokesStopCellEditing() && !ui.stopEditing(tree)) {
                return;
            }
            TreePath path = getMouseClickedClosestPathForLocation(tree, e.getX(), e.getY());
            if (startEditingOnRelease(path, e, e)) {
                return;
            }

            mouseDragSelects = false;
            if (mouseReleaseDeselects) {
                int index = tree.getRowForPath(path);
                tree.setSelectionInterval(index, index);
            }
            //tree.getSelectionModel().setValueIsAdjusting(false);
        }
        if (tree.isRequestFocusEnabled()) {
            tree.requestFocus();
        }
    }

    public void mouseExited(MouseEvent e) {
        isMouseReleaseStartsEditing = false;
    }

    // cover method for startEditing that allows us to pass extra
    // information into that method via a class variable
    private boolean startEditingOnRelease(TreePath path, MouseEvent event, MouseEvent releaseEvent) {
        //this.releaseEvent = releaseEvent;
        try {
            if (isMouseReleaseStartsEditing) {
                return ui.startEditing(path, event);
            } else {
                return false;
            }
        } finally {
            //this.releaseEvent = null;
        }
    }

    private TreePath getMouseClickedClosestPathForLocation(JTree tree, int x, int y) {
        final TreePath path = ui.getClosestPathForLocation(tree, x, y);
        if (path == null) {
            return null;
        }

        final Rectangle pathBounds = ui.getPathBounds(tree, path);
        if (y > pathBounds.y + pathBounds.height) {
            return null;
        }

        return path;
    }
}
