/*
 * Copyright (c) 2014-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.plaf.ListUI;
import java.awt.*;

/**
 * A wrapper for JList to support the generic list interface.
 */
public class JListModel implements GenericList {

    protected final JList list;

    public JListModel(JList list) {
        this.list = list;
    }

    @Override
    public JComponent getComponent() {
        return list;
    }

    @Override
    public void requestFocus() {
        if (list.isEnabled() && list.isRequestFocusEnabled()) {
            JavaSupport.requestFocusInWindowFromMouseEvent(list);
        }
    }

    @Override
    public boolean isEnabled() {
        return list.isEnabled();
    }

    @Override
    public int getRowCount() {
        return list.getModel().getSize();
    }

    @Override
    public Object getRow(int index) {
        return list.getModel().getElementAt(index);
    }

    @Override
    public boolean isMultipleSelection() {
        return list.getSelectionMode() != ListSelectionModel.SINGLE_SELECTION;
    }

    @Override
    public boolean isRowSelected(int index) {
        return list.isSelectedIndex(index);
    }

    @Override
    public boolean isSelectionEmpty() {
        return list.isSelectionEmpty();
    }

    @Override
    public void clearSelection() {
        list.clearSelection();
    }

    public void setSelectionInterval(int index1, int index2) {
        list.setSelectionInterval(index1, index2);
    }

    public void addSelectionInterval(int index1, int index2) {
        list.addSelectionInterval(index1, index2);
    }

    public void removeSelectionInterval(int index1, int index2) {
        list.removeSelectionInterval(index1, index2);
    }

    @Override
    public int getAnchorSelectionIndex() {
        return list.getAnchorSelectionIndex();
    }

    @Override
    public void setAnchorSelectionIndex(int index) {
        list.getSelectionModel().setAnchorSelectionIndex(index);
    }

    @Override
    public int getMinSelectionIndex() {
        return list.getMinSelectionIndex();
    }

    @Override
    public int getMaxSelectionIndex() {
        return list.getMaxSelectionIndex();
    }

    @Override
    public boolean isValueAdjusting() {
        return list.getValueIsAdjusting();
    }

    @Override
    public void setValueIsAdjusting(boolean b) {
        list.getSelectionModel().setValueIsAdjusting(b);
    }

    @Override
    public int identifyRowAtLocation(Point loc) {
        ListUI ui = list.getUI();
        int index = ui.locationToIndex(list, loc);

        if (index != -1) {
            Rectangle cellBounds = list.getCellBounds(index, index);
            if (loc.x > cellBounds.getX() + cellBounds.getWidth()
                || loc.y > cellBounds.getY() + cellBounds.getHeight()) {
                    index = -1;
            }
        }

        return index;
    }

    @Override
    public void scrollToViewRows(int index1, int index2) {
        ListUI ui = list.getUI();
        Rectangle cellBounds = ui.getCellBounds(list, index1, index2);
        list.scrollRectToVisible(cellBounds);
    }

    @Override
    public boolean isDragEnabled() {
        return list.getDragEnabled();
    }

    @Override
    public TransferHandler getTransferHandler() {
        return list.getTransferHandler();
    }
}
