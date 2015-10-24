/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import java.awt.*;

/**
 * A wrapper for JTable to support the generic list interface.
 */
public class JTableModel implements GenericList {

    protected final JTable table;
    protected final int designatedColumnIndex;

    public JTableModel(JTable table, int designatedColumnIndex) {
        this.table = table;
        this.designatedColumnIndex = designatedColumnIndex;
    }

    @Override
    public JComponent getComponent() {
        return table;
    }

    @Override
    public void requestFocus() {
        if (table.isEnabled() && table.isRequestFocusEnabled()) {
            table.requestFocusInWindow();
        }
    }

    @Override
    public boolean isEnabled() {
        return table.isEnabled();
    }

    @Override
    public int getRowCount() {
        return table.getRowCount();
    }

    @Override
    public Object getRow(int index) {
        return table.getModel().getValueAt(index, designatedColumnIndex);
    }

    @Override
    public boolean isMultipleSelection() {
        return table.getSelectionModel().getSelectionMode() != ListSelectionModel.SINGLE_SELECTION;
    }

    @Override
    public boolean isRowSelected(int index) {
        return table.getSelectionModel().isSelectedIndex(index);
    }

    @Override
    public boolean isSelectionEmpty() {
        return table.getSelectionModel().isSelectionEmpty();
    }

    @Override
    public void clearSelection() {
        table.clearSelection();
    }

    public void setSelectionInterval(int index1, int index2) {
        table.getSelectionModel().setSelectionInterval(index1, index2);
    }

    public void addSelectionInterval(int index1, int index2) {
        table.getSelectionModel().addSelectionInterval(index1, index2);
    }

    public void removeSelectionInterval(int index1, int index2) {
        table.getSelectionModel().removeSelectionInterval(index1, index2);
    }

    @Override
    public int getAnchorSelectionIndex() {
        return table.getSelectionModel().getAnchorSelectionIndex();
    }

    @Override
    public void setAnchorSelectionIndex(int index) {
        table.getSelectionModel().setAnchorSelectionIndex(index);
    }

    @Override
    public int getMinSelectionIndex() {
        return table.getSelectionModel().getMinSelectionIndex();
    }

    @Override
    public int getMaxSelectionIndex() {
        return table.getSelectionModel().getMaxSelectionIndex();
    }

    @Override
    public boolean isValueAdjusting() {
        return table.getSelectionModel().getValueIsAdjusting();
    }

    @Override
    public void setValueIsAdjusting(boolean b) {
        table.getSelectionModel().setValueIsAdjusting(b);
    }

    @Override
    public int identifyRowAtLocation(Point loc) {
        return table.rowAtPoint(loc);
    }

    @Override
    public void scrollToViewRows(int index1, int index2) {
        int columnCount = table.getColumnCount();
        Rectangle r1 = table.getCellRect(index1, 0, false);
        Rectangle r2 = table.getCellRect(index2, columnCount-1, false);
        table.scrollRectToVisible(r1.union(r2));
    }

    @Override
    public boolean isDragEnabled() {
        return table.getDragEnabled();
    }

    @Override
    public TransferHandler getTransferHandler() {
        return table.getTransferHandler();
    }
}
