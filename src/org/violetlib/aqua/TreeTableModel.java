/*
 * Copyright (c) 2014-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.violetlib.treetable.TreeTable;

/**
 * A wrapper for TreeTable to support the generic list interface.
 */
public class TreeTableModel implements GenericList {

    protected final TreeTable tt;
    protected final int designatedColumnIndex;

    public TreeTableModel(TreeTable tt) {
        this.tt = tt;
        this.designatedColumnIndex = tt.getHierarchicalColumn();
    }

    @Override
    public JComponent getComponent() {
        return tt;
    }

    @Override
    public void requestFocus() {
        if (tt.isEnabled() && tt.isRequestFocusEnabled()) {
            JavaSupport.requestFocusInWindowFromMouseEvent(tt);
        }
    }

    @Override
    public boolean isEnabled() {
        return tt.isEnabled();
    }

    @Override
    public int getRowCount() {
        return tt.getRowCount();
    }

    @Override
    public Object getRow(int index) {
        return tt.getValueAt(index, designatedColumnIndex);
    }

    @Override
    public boolean isMultipleSelection() {
        return tt.getSelectionModel().getSelectionMode() != ListSelectionModel.SINGLE_SELECTION;
    }

    @Override
    public boolean isRowSelected(int index) {
        return tt.isRowSelected(index);
    }

    @Override
    public boolean isSelectionEmpty() {
        return tt.getSelectionModel().isSelectionEmpty();
    }

    @Override
    public void clearSelection() {
        tt.clearSelection();
    }

    public void setSelectionInterval(int index1, int index2) {
        tt.setSelectionInterval(index1, index2);
    }

    public void addSelectionInterval(int index1, int index2) {
        tt.addSelectionInterval(index1, index2);
    }

    public void removeSelectionInterval(int index1, int index2) {
        tt.removeSelectionInterval(index1, index2);
    }

    @Override
    public int getAnchorSelectionIndex() {
        return tt.getRowSelectionModel().getAnchorSelectionIndex();
    }

    @Override
    public void setAnchorSelectionIndex(int index) {
        tt.getRowSelectionModel().setAnchorSelectionIndex(index);
    }

    @Override
    public int getMinSelectionIndex() {
        return tt.getRowSelectionModel().getMinSelectionIndex();
    }

    @Override
    public int getMaxSelectionIndex() {
        return tt.getRowSelectionModel().getMaxSelectionIndex();
    }

    @Override
    public boolean isValueAdjusting() {
        return tt.getRowSelectionModel().getValueIsAdjusting();
    }

    @Override
    public void setValueIsAdjusting(boolean b) {
        tt.getRowSelectionModel().setValueIsAdjusting(b);
    }

    @Override
    public int identifyRowAtLocation(Point loc) {
        return tt.rowAtPoint(loc);
    }

    @Override
    public void scrollToViewRows(int index1, int index2) {
        int columnCount = tt.getColumnCount();
        Rectangle r1 = tt.getCellRect(index1, 0, false);
        Rectangle r2 = tt.getCellRect(index2, columnCount-1, false);
        tt.scrollRectToVisible(r1.union(r2));
    }

    @Override
    public boolean isDragEnabled() {
        return tt.getDragEnabled();
    }

    @Override
    public TransferHandler getTransferHandler() {
        return tt.getTransferHandler();
    }
}
