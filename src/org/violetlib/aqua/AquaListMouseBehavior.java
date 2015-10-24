/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import java.awt.dnd.DragSource;
import java.awt.event.MouseEvent;

import static java.awt.event.InputEvent.*;
import static java.awt.event.InputEvent.META_DOWN_MASK;
import static java.awt.event.InputEvent.SHIFT_DOWN_MASK;

/**
 * Mouse behavior for list-like components. This class simulates NSTableView behavior.
 */
public class AquaListMouseBehavior extends MouseInputAdapter {

    protected GenericList list;

    // Selection changing operations (mutually exclusive):
    protected final int OP_SELECT = 1;      // select (unmodified)
    protected final int OP_INTERVAL = 2;    // extend or retract an interval (shift)
    protected final int OP_TOGGLE = 3;      // toggle the selection state (command)

    protected int targetIndex;              // the index of the list cell under the mouse pointer
    protected boolean dragWouldTransfer;    // true if a drag gesture would transfer data (not change the selection)

    protected int mouseReleaseOp;           // the operation to perform on mouse release (if any)
    protected int mouseDragOp;              // the operation to perform on mouse drag (if any)
    protected boolean isMultipleSelection;  // true if the list selection model supports multiple selection
    protected int mouseDragOpPreviousIndex; // used to recognize when a mouse drag moves to a new list cell

    protected MouseEvent armedEvent;        // used to recognize drag gestures
    protected int dragThreshold;            // used to recognize drag gestures
    protected boolean isDragging;

    public AquaListMouseBehavior(GenericList list) {
        this.list = list;
    }

    public void processMouseEvent(MouseEvent e) {
        int id = e.getID();
        if (id == MouseEvent.MOUSE_PRESSED) {
            mousePressed(e);
        } else if (id == MouseEvent.MOUSE_RELEASED) {
            mouseReleased(e);
        } else if (id == MouseEvent.MOUSE_DRAGGED) {
            mouseDragged(e);
        }
    }

    @Override
    public void mousePressed(MouseEvent e) {
        armedEvent = e;
        dragThreshold = DragSource.getDragThreshold();

        // Note: Some applications depend on selection changes only occurring on focused components. Maybe we must not
        // do any changes to the selection changes at all, when the component is not focused?

        list.requestFocus();

        mouseReleaseOp = 0;
        mouseDragOp = 0;
        targetIndex = -1;
        dragWouldTransfer = false;
        isDragging = false;

        if (!list.isEnabled() || e.isPopupTrigger()) {
            return;
        }

        isMultipleSelection = list.isMultipleSelection();

        interpretLocation(e);   // sets targetIndex and dragWouldTransfer

        int op = getSelectionOperation(e);

        if (targetIndex < 0) {
            mousePressedOutsideCell(op);
        }

        mouseDragOpPreviousIndex = targetIndex;

        /*
          Click selection operations may happen on mouse pressed or mouse released.
        */

        if (op != 0) {
            if (isMousePressedOperation(op)) {
                performSelectionOperation(op);
            } else {
                mouseReleaseOp = op;
            }

            if (!dragWouldTransfer) {
                mouseDragOp = op;
            }
        }

        list.setValueIsAdjusting(mouseDragOp != 0);
    }

    @Override
    public void mouseDragged(MouseEvent e) {

        // Abort if mouseDragged event is received without prior mousePressed event.
        if (armedEvent == null) {
            return;
        }

        /*
          NSTableView allows an unselected item to be dragged. Alas, Swing/AWT defers this decision to the transfer
          handler, which can (and often must) be redefined by the application. To make things look reasonable, we try to
          ensure that the dragged item is selected (as soon as possible).
        */

        if (list.isDragEnabled()) {
            TransferHandler th = list.getTransferHandler();
            int action = AquaDragRecognitionSupport.mapDragOperationFromModifiers(e, th);
            if (action != TransferHandler.NONE) {
                if (!list.isRowSelected(targetIndex)) {
                    setSelectionInterval(targetIndex, targetIndex);
                }
            }
        }

        if (!isDragging) {
            int dx = Math.abs(e.getX() - armedEvent.getX());
            int dy = Math.abs(e.getY() - armedEvent.getY());
            isDragging = Math.sqrt(dx * dx + dy * dy) > dragThreshold;
        }

        if (isDragging) {

            mouseReleaseOp = 0;

            if (mouseDragOp != 0) {

                targetIndex = getIndex(e);
                if (targetIndex != -1 && targetIndex != mouseDragOpPreviousIndex) {

                    list.scrollToViewRows(targetIndex, targetIndex);

                    if (!isMultipleSelection) {
                        performSelectionOperation(OP_SELECT);
                    } else {

                        /*
                          Drag selection operates on an interval between the anchor index and the drag index. The effect
                          is different if we are moving away from the anchor or towards the anchor.
                        */

                        int anchorIndex = getAnchor();
                        if (targetIndex > anchorIndex && targetIndex > mouseDragOpPreviousIndex) {
                            // moving away from the anchor
                            performDragSelectionOperation(mouseDragOp, mouseDragOpPreviousIndex+1, targetIndex, true);
                        } else if (targetIndex < anchorIndex && targetIndex < mouseDragOpPreviousIndex) {
                            // moving away from the anchor
                            performDragSelectionOperation(mouseDragOp, mouseDragOpPreviousIndex-1, targetIndex, true);
                        } else if (targetIndex >= anchorIndex && targetIndex < mouseDragOpPreviousIndex) {
                            // moving toward the anchor
                            performDragSelectionOperation(mouseDragOp, targetIndex +1, mouseDragOpPreviousIndex, false);
                        } else if (targetIndex <= anchorIndex && targetIndex > mouseDragOpPreviousIndex) {
                            // moving toward the anchor
                            performDragSelectionOperation(mouseDragOp, targetIndex -1, mouseDragOpPreviousIndex, false);
                        }
                    }

                    mouseDragOpPreviousIndex = targetIndex;
                }

            } else {
                if (list.isDragEnabled()) {
                    TransferHandler th = list.getTransferHandler();
                    int action = AquaDragRecognitionSupport.mapDragOperationFromModifiers(e, th);
                    if (action != TransferHandler.NONE) {
                        th.exportAsDrag(list.getComponent(), armedEvent, action);
                    }
                }
            }
        }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        if (mouseReleaseOp != 0) {
            performSelectionOperation(mouseReleaseOp);
            mouseReleaseOp = 0;
        }

        list.setValueIsAdjusting(false);
    }

    /**
     * Identify the operation to be performed, if any.
     */
    protected int getSelectionOperation(MouseEvent e) {
        int mods = e.getModifiersEx();

        if ((mods & (META_DOWN_MASK | BUTTON2_DOWN_MASK | BUTTON3_DOWN_MASK)) == META_DOWN_MASK) {
            return OP_TOGGLE;
        } else if ((mods & (SHIFT_DOWN_MASK | BUTTON2_DOWN_MASK | BUTTON3_DOWN_MASK)) == SHIFT_DOWN_MASK) {
            return OP_INTERVAL;
        } else if ((e.getModifiersEx() & (SHIFT_DOWN_MASK | META_DOWN_MASK)) == 0) {
            return OP_SELECT;
        } else {
            return 0;
        }
    }

    /**
     * Determine if an operation should be performed on mouse pressed.
     */
    protected boolean isMousePressedOperation(int op) {
        if (op == OP_SELECT) {
            return !dragWouldTransfer;
        } else if (op == OP_INTERVAL) {
            return true;
        } else if (op == OP_TOGGLE) {
            return !(list.isRowSelected(targetIndex) && dragWouldTransfer);
        } else {
            // must be a custom operation defined by a subclass
            return true;
        }
    }

    /**
     * Interpret the mouse location represented by a mouse pressed event. Set targetIndex to the index of the target
     * item (if any). Set dragWouldTransfer to true if a drag operation started here would perform a data transfer (as
     * opposed to a drag selection). The default behavior is that any drag gesture represents a data transfer if drag is
     * enabled on the list. However, there are situations where data transfers happen only if the drag gesture starts in
     * an active area of the list cell (such as Finder).
     */
    protected void interpretLocation(MouseEvent e) {
        targetIndex = getIndex(e);
        dragWouldTransfer = list.isDragEnabled();
    }

    /**
     * Identify the target index corresponding to a mouse event. Locations outside of any list cell are excluded.
     */
    protected int getIndex(MouseEvent e) {
        return list.identifyRowAtLocation(e.getPoint());
    }

    /**
     * The mouse has been pressed while outside any list cell. Perform appropriate actions.
     */
    protected void mousePressedOutsideCell(int op) {
        list.clearSelection();
        if (op == OP_TOGGLE) {
            op = OP_SELECT;
        }
        if (op == OP_SELECT) {
            int lastIndex = list.getRowCount()-1;
            list.setAnchorSelectionIndex(lastIndex);
        } else {
            list.setAnchorSelectionIndex(0);
        }
        dragWouldTransfer = false;
    }

    /**
     * Perform a click selection operation.
     */
    protected void performSelectionOperation(int op) {
        if (targetIndex < 0) {
            return;
        }

        if (op == OP_INTERVAL && !isMultipleSelection) {
            op = OP_SELECT;
        }

        if (op == OP_TOGGLE) {

            toggleSelection(targetIndex);

        } else if (op == OP_INTERVAL) {

            int anchorIndex = getAnchor();
            selectInterval(anchorIndex, targetIndex);

        } else if (op == OP_SELECT) {

            /*
              Make the target item the unique selection. Update the anchor index.
            */

            setSelectionInterval(targetIndex, targetIndex);
        }
    }

    /**
     * Perform a drag selection operation.
     */
    protected void performDragSelectionOperation(int op, int index1, int index2, boolean isExtending) {

        if (!isMultipleSelection) {
            setSelectionInterval(targetIndex, targetIndex);
            return;
        }

        /*
          It is surprising that extending a shift-click by dragging deselects items that are not contiguous with the
          interval. I wonder if this was intentional on Apple's part. Seems like a bug.
        */

        if (op == OP_TOGGLE) {
            int anchorIndex = getAnchor();
            toggleSelectionInterval(index1, index2);
            list.setAnchorSelectionIndex(anchorIndex);
        } else if (op == OP_INTERVAL || op == OP_SELECT) {
            int anchorIndex = getAnchor();
            setSelectionInterval(anchorIndex, targetIndex);
        }
    }

    /**
     * Toggle the selection state of the specified item. Update the anchor index.
     */
    protected void toggleSelection(int index) {

        if (list.isRowSelected(index)) {
            int anchorIndex = getAnchor();
            removeSelectionInterval(index, index);
            if (index == anchorIndex) {
                updateAnchor(anchorIndex);
            } else {
                list.setAnchorSelectionIndex(anchorIndex);
            }
        } else {
            addSelectionInterval(index, index);
            list.setAnchorSelectionIndex(index);
        }
    }

    /**
     * Extend and/or retract a contiguous interval of selection. Do not alter the anchor index. Do not alter selected
     * items that are not contiguous with the interval.
     */
    protected void selectInterval(int anchorIndex, int targetIndex) {
        list.setValueIsAdjusting(true);

        // Trim the selection on the opposite side of the anchor

        int direction = targetIndex - anchorIndex;
        trimSelectedInterval(anchorIndex, -direction);

        if (list.isRowSelected(targetIndex)) {

            // Retract the interval by removing contiguous selected items

            trimSelectedInterval(targetIndex, direction);

        } else {

            // Extends the current interval

            addSelectionInterval(anchorIndex, targetIndex);
        }

        list.setAnchorSelectionIndex(anchorIndex);
        list.setValueIsAdjusting(false);
    }

    /**
     * Get the anchor index. The anchor index is always valid unless the list is empty.
     */
    protected int getAnchor() {
        int anchorIndex = list.getAnchorSelectionIndex();
        return Math.max(0, anchorIndex);
    }

    /**
     * Update the anchor after deselecting the old anchor item.
     */
    protected void updateAnchor(int oldAnchor) {
        int count = list.getRowCount();
        for (int i = oldAnchor+1; i < count; i++) {
            if (list.isRowSelected(i)) {
                list.setAnchorSelectionIndex(i);
                return;
            }
        }

        for (int i = oldAnchor-1; i >= 0; i--) {
            if (list.isRowSelected(i)) {
                list.setAnchorSelectionIndex(i);
                return;
            }
        }

        list.setAnchorSelectionIndex(0);
    }

    protected void trimSelectedInterval(int index, int direction) {
        if (direction > 0) {
            int count = list.getRowCount();
            while (++index < count) {
                if (list.isRowSelected(index)) {
                    list.removeSelectionInterval(index, index);
                } else {
                    break;
                }
            }
        } else if (direction < 0) {
            while (--index >= 0) {
                if (list.isRowSelected(index)) {
                    list.removeSelectionInterval(index, index);
                } else {
                    break;
                }
            }
        }
    }

    protected void setSelectionInterval(int index1, int index2) {
        list.setSelectionInterval(index1, index2);
    }

    protected void addSelectionInterval(int index1, int index2) {
        list.addSelectionInterval(index1, index2);
    }

    protected void removeSelectionInterval(int index1, int index2) {
        list.removeSelectionInterval(index1, index2);
    }

    protected void toggleSelectionInterval(int index1, int index2) {
        int delta = index1 > index2 ? -1 : 1;
        int index = index1;
        for (;;) {
            if (!list.isRowSelected(index)) {
                list.addSelectionInterval(index, index);
            } else {
                list.removeSelectionInterval(index, index);
            }
            if (index == index2) {
                return;
            }
            index += delta;
        }
    }
}
