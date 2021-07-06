/*
 * Copyright (c) 2014-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.EventObject;
import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Implements Aqua mouse behavior for a JTree. Based loosely on the mouse listener in BasicTreeUI.
 */
public class AquaTreeMouseBehavior extends MouseInputAdapter implements AquaDragRecognitionSupport.BeforeDrag {
    protected JTree tree;
    protected AquaTreeUI ui;

    public static boolean isDebug = false;

    private boolean mouseReleaseDeselects;
    private boolean mouseDragSelects;
    private boolean isMouseReleaseStartsEditing;
    private boolean isDragRecognitionOngoing;
    private boolean isDragging;

    public AquaTreeMouseBehavior(@NotNull JTree tree) {
        this.tree = tree;
        this.ui = (AquaTreeUI) tree.getUI();
    }

    public void dragStarting(@NotNull MouseEvent e) {
        if (isDebug) {
            Utils.logDebug("Drag gesture recognized");
        }

        // this is a dirty trick to reset the timer of the cell editor.
        if (tree.getCellEditor() != null) {
            tree.getCellEditor().isCellEditable(new EventObject(this));
        }

        mouseReleaseDeselects = false;
        isMouseReleaseStartsEditing = false;
        isDragging = true;
    }

    public void mousePressed(@NotNull MouseEvent e) {
        if (isDebug) {
            Utils.logDebug("JTree mouse pressed " + e.getClickCount());
        }

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
            if (isDebug) {
                Utils.logDebug("  Path: " + path);
            }

            // Check for clicks in expand control
            if (ui.isLocationInExpandControl(path, e.getX(), e.getY())) {
                ui.checkForClickInExpandControl(path, e.getX(), e.getY());
                return;
            }

            // Clicking on a sidebar category is like clicking on its expand control
            if (ui.isSideBar() && path != null && path.getPathCount() == 2) {
                ui.handleExpandControlClick(path);
                return;
            }

            int index = tree.getRowForPath(path);

            mouseDragSelects = false;
            mouseReleaseDeselects = false;
            isMouseReleaseStartsEditing = e.getClickCount() == 1;
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

            if (isDebug) {
                Utils.logDebug("  mouseDragSelects: " + mouseDragSelects);
                Utils.logDebug("  mouseReleaseStartsEditing: " + isMouseReleaseStartsEditing);
                Utils.logDebug("  mouseReleaseDeselects: " + mouseReleaseDeselects);
                Utils.logDebug("  dragRecognitionOngoing: " + isDragRecognitionOngoing);
            }
        }
    }

    public void mouseDragged(@NotNull MouseEvent e) {
        if (isDebug) {
            Utils.logDebug("JTree mouse dragged");
        }

        if (tree.isEnabled() && SwingUtilities.isLeftMouseButton(e) && !e.isConsumed()) {
            if (tree.getDragEnabled() && isDragRecognitionOngoing) {
                isDragRecognitionOngoing = AquaDragRecognitionSupport.mouseDragged(e, this);
                if (!isDragRecognitionOngoing && isDragRecognitionOngoing) {
                    Utils.logDebug("  Drag recognition stopped");
                }
            }

            // The following should have been done on mouse pressed.
            if (false) {
                // Do nothing if we can't stop editing.
                if (ui.isEditing(tree) && tree.getInvokesStopCellEditing() && !ui.stopEditing(tree)) {
                    return;
                }
            }
        }

        if (mouseDragSelects && isDragging) {
            TreePath leadPath = ui.getClosestPathForLocation(tree, e.getX(), e.getY());
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

    /**
     * Invoked when the mouse button has been moved on a component (with no buttons down).
     */
    public void mouseMoved(@NotNull MouseEvent e) {
        if (false) {
            // The following should be irrelevant:
            isMouseReleaseStartsEditing = false;
            // The following could allow a miniscule mouse movement to abort a click that would otherwise start editing.
            // this is a dirty trick to reset the timer of the cell editor.
            if (tree.getCellEditor() != null) {
                tree.getCellEditor().isCellEditable(new EventObject(this));
            }
        }
    }

    public void mouseReleased(@NotNull MouseEvent e) {
        if (isDebug) {
            Utils.logDebug("JTree mouse released " + e.getClickCount());
        }

        isDragging = false;

        if (tree.isEnabled() && SwingUtilities.isLeftMouseButton(e) && !e.isConsumed()) {
            if (ui.isEditing(tree) && tree.getInvokesStopCellEditing() && !ui.stopEditing(tree)) {
                return;
            }
            TreePath path = getMouseClickedClosestPathForLocation(tree, e.getX(), e.getY());
            if (path != null) {
                if (isDebug) {
                    Utils.logDebug("  path: " + path);
                    Utils.logDebug("  shouldStartEditing: " + isMouseReleaseStartsEditing);
                }

                boolean shouldEdit = isMouseReleaseStartsEditing;
                isMouseReleaseStartsEditing = false;

                if (shouldEdit && ui.startEditing(path, e)) {
                    if (isDebug) {
                        Utils.logDebug("  Editing started");
                    }
                    return;
                }
                if (shouldEdit && isDebug) {
                    Utils.logDebug("  UI declined to start editing");
                }

                mouseDragSelects = false;
                if (mouseReleaseDeselects) {
                    int index = tree.getRowForPath(path);
                    tree.setSelectionInterval(index, index);
                }
                //tree.getSelectionModel().setValueIsAdjusting(false);
            }
        }
        if (tree.isRequestFocusEnabled()) {
            tree.requestFocus();
        }
    }

    public void mouseExited(@NotNull MouseEvent e) {
        if (isDebug) {
            Utils.logDebug("  Mouse exited");
            if (isMouseReleaseStartsEditing) {
                Utils.logDebug("  Clearing mouseReleaseStartsEditing");
            }
        }

        isMouseReleaseStartsEditing = false;
    }

    private @Nullable TreePath getMouseClickedClosestPathForLocation(@NotNull JTree tree, int x, int y) {
        TreePath path = ui.getClosestPathForLocation(tree, x, y);
        if (path == null) {
            return null;
        }

        Rectangle pathBounds = ui.getPathBounds(tree, path);
        if (y > pathBounds.y + pathBounds.height) {
            return null;
        }

        return path;
    }
}
