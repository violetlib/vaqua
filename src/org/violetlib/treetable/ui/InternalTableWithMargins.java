/*
 * Copyright (c) 2020-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.treetable.ui;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.jetbrains.annotations.NotNull;

import static org.violetlib.aqua.AquaTableUI.INSET_VIEW_MARGIN_KEY;
import static org.violetlib.aqua.AquaTableUI.INSET_VIEW_VERTICAL_MARGIN_KEY;

// Support margins in a subclass of JTable

public class InternalTableWithMargins
        extends JTable
{
    private final TableLayoutManager layoutManager = new TableLayoutManager(this);
    private final MyPropertyChangeListener propertyChangeListener = new MyPropertyChangeListener();

    private int margin;
    private int verticalMargin;

    public InternalTableWithMargins(TableModel dm, TableColumnModel cm, ListSelectionModel sm)
    {
        super(dm, cm, sm);
        initialize();
    }

    private void initialize()
    {
        addPropertyChangeListener(propertyChangeListener);
        updateMargins();
    }

    private void updateMargins()
    {
        Object o = getClientProperty(INSET_VIEW_MARGIN_KEY);
        Object vo = getClientProperty(INSET_VIEW_VERTICAL_MARGIN_KEY);
        int margin = Math.max(0, o instanceof Integer ? (Integer) o : 0);
        int verticalMargin = Math.max(0, vo instanceof Integer ? (Integer) vo : 0);
        if (margin != this.margin || verticalMargin != this.verticalMargin) {
            installMargin(margin, verticalMargin);
        }
    }

    private void installMargin(int m, int vm)
    {
        margin = m;
        verticalMargin = vm;
        revalidate();
        repaint();
        JTableHeader header = getTableHeader();
        if (header != null) {
            header.revalidate();
            header.repaint();
        }
        marginChanged(margin, verticalMargin);
    }

    protected void marginChanged(int margin, int verticalMargin)
    {
    }

    @Override
    public void doLayout()
    {
        layoutManager.doLayout();
    }

    @Override
    public void sizeColumnsToFit(int resizingColumn)
    {
        layoutManager.sizeColumnsToFit(resizingColumn);
    }

    @Override
    public int columnAtPoint(@NotNull Point point)
    {
        if (margin > 0 || verticalMargin > 0) {
            point = new Point(point.x - margin, point.y - verticalMargin);
        }
        return super.columnAtPoint(point);
    }

    @Override
    public int rowAtPoint(@NotNull Point point)
    {
        if (margin > 0 || verticalMargin > 0) {
            point = new Point(point.x - margin, point.y - verticalMargin);
        }
        return super.rowAtPoint(point);
    }

    @Override
    public @NotNull Rectangle getCellRect(int row, int column, boolean includeSpacing)
    {
        Rectangle r = super.getCellRect(row, column, includeSpacing);
        if ((margin > 0 || verticalMargin > 0) && columnModel.getColumnCount() > 0) {
            r.x += margin;
            r.y += verticalMargin;
        }
        return r;
    }

    @Override
    protected JTableHeader createDefaultTableHeader()
    {
        return new TableHeaderWithMargins(columnModel);
    }

    private class MyPropertyChangeListener
            implements PropertyChangeListener
    {
        @Override
        public void propertyChange(PropertyChangeEvent evt)
        {
            String name = evt.getPropertyName();
            if (name == null || name.equals(INSET_VIEW_MARGIN_KEY) || name.equals(INSET_VIEW_VERTICAL_MARGIN_KEY)) {
                updateMargins();
            }
        }
    }

    /**
     The layout manager assigns column widths.
     */

    private static class TableLayoutManager
    {
        private InternalTableWithMargins table;

        public TableLayoutManager(InternalTableWithMargins table)
        {
            this.table = table;
        }

        public void doLayout()
        {
            TableColumn resizingColumn = getResizingColumn();
            if (resizingColumn == null) {
                setWidthsFromPreferredWidths(false);
            }
            else {
                int columnIndex = viewIndexForColumn(resizingColumn);
                int delta = getAvailableWidth() - table.getColumnModel().getTotalColumnWidth();
                accommodateDelta(columnIndex, delta);
                delta = getAvailableWidth() - table.getColumnModel().getTotalColumnWidth();
                if (delta != 0) {
                    resizingColumn.setWidth(resizingColumn.getWidth() + delta);
                }
                setWidthsFromPreferredWidths(true);
            }
        }

        public void sizeColumnsToFit(int resizingColumn)
        {
            if (resizingColumn == -1) {
                setWidthsFromPreferredWidths(false);
            } else {
                if (table.getAutoResizeMode() == AUTO_RESIZE_OFF) {
                    TableColumn aColumn = table.getColumnModel().getColumn(resizingColumn);
                    aColumn.setPreferredWidth(aColumn.getWidth());
                } else {
                    int delta = getAvailableWidth() - table.getColumnModel().getTotalColumnWidth();
                    accommodateDelta(resizingColumn, delta);
                    setWidthsFromPreferredWidths(true);
                }
            }
        }

        public int getAvailableWidth()
        {
            return table.getWidth() - 2 * table.margin;
        }

        public int getPreferredAvailableWidth()
        {
            return table.getPreferredSize().width - 2 * table.margin;
        }

        private TableColumn getResizingColumn()
        {
            JTableHeader tableHeader = table.getTableHeader();
            return (tableHeader == null) ? null
                    : tableHeader.getResizingColumn();
        }

        private void setWidthsFromPreferredWidths(final boolean inverse)
        {
            int totalWidth     = getAvailableWidth();
            int totalPreferred = getPreferredAvailableWidth();
            int target = !inverse ? totalWidth : totalPreferred;

            final TableColumnModel cm = table.getColumnModel();
            Resizable3 r = new Resizable3() {
                public int  getElementCount()      { return cm.getColumnCount(); }
                public int  getLowerBoundAt(int i) { return cm.getColumn(i).getMinWidth(); }
                public int  getUpperBoundAt(int i) { return cm.getColumn(i).getMaxWidth(); }
                public int  getMidPointAt(int i)  {
                    if (!inverse) {
                        return cm.getColumn(i).getPreferredWidth();
                    }
                    else {
                        return cm.getColumn(i).getWidth();
                    }
                }
                public void setSizeAt(int s, int i) {
                    if (!inverse) {
                        cm.getColumn(i).setWidth(s);
                    }
                    else {
                        cm.getColumn(i).setPreferredWidth(s);
                    }
                }
            };

            adjustSizes(target, r, inverse);
        }

        // Distribute delta over columns, as indicated by the autoresize mode.
        private void accommodateDelta(int resizingColumnIndex, int delta)
        {
            int columnCount = table.getColumnCount();
            int from = resizingColumnIndex;
            int to;

            // Use the mode to determine how to absorb the changes.
            switch(table.getAutoResizeMode()) {
                case AUTO_RESIZE_NEXT_COLUMN:
                    from = from + 1;
                    to = Math.min(from + 1, columnCount); break;
                case AUTO_RESIZE_SUBSEQUENT_COLUMNS:
                    from = from + 1;
                    to = columnCount; break;
                case AUTO_RESIZE_LAST_COLUMN:
                    from = columnCount - 1;
                    to = from + 1; break;
                case AUTO_RESIZE_ALL_COLUMNS:
                    from = 0;
                    to = columnCount; break;
                default:
                    return;
            }

            final int start = from;
            final int end = to;
            final TableColumnModel cm = table.getColumnModel();
            Resizable3 r = new Resizable3() {
                public int  getElementCount()       { return end-start; }
                public int  getLowerBoundAt(int i)  { return cm.getColumn(i+start).getMinWidth(); }
                public int  getUpperBoundAt(int i)  { return cm.getColumn(i+start).getMaxWidth(); }
                public int  getMidPointAt(int i)    { return cm.getColumn(i+start).getWidth(); }
                public void setSizeAt(int s, int i) {        cm.getColumn(i+start).setWidth(s); }
            };

            int totalWidth = 0;
            for(int i = from; i < to; i++) {
                TableColumn aColumn = table.getColumnModel().getColumn(i);
                int input = aColumn.getWidth();
                totalWidth = totalWidth + input;
            }

            adjustSizes(totalWidth + delta, r, false);
        }

        private interface Resizable2
        {
            int getElementCount();
            int getLowerBoundAt(int i);
            int getUpperBoundAt(int i);
            void setSizeAt(int newSize, int i);
        }

        private interface Resizable3 extends Resizable2
        {
            int getMidPointAt(int i);
        }

        private void adjustSizes(long target, final Resizable3 r, boolean inverse)
        {
            int N = r.getElementCount();
            long totalPreferred = 0;
            for(int i = 0; i < N; i++) {
                totalPreferred += r.getMidPointAt(i);
            }

            // Code change here. If preferred sizes match the target, should not increase or decrease.
            if (target == totalPreferred) {
                for (int i = 0; i < N; i++) {
                    int preferred = r.getMidPointAt(i);
                    r.setSizeAt(preferred, i);
                }
                return;
            }

            Resizable2 s;
            if ((target < totalPreferred) == !inverse) {
                s = new Resizable2() {
                    public int  getElementCount()      { return r.getElementCount(); }
                    public int  getLowerBoundAt(int i) { return r.getLowerBoundAt(i); }
                    public int  getUpperBoundAt(int i) { return r.getMidPointAt(i); }
                    public void setSizeAt(int newSize, int i) { r.setSizeAt(newSize, i); }

                };
            }
            else {
                s = new Resizable2() {
                    public int  getElementCount()      { return r.getElementCount(); }
                    public int  getLowerBoundAt(int i) { return r.getMidPointAt(i); }
                    public int  getUpperBoundAt(int i) { return r.getUpperBoundAt(i); }
                    public void setSizeAt(int newSize, int i) { r.setSizeAt(newSize, i); }

                };
            }
            adjustSizes(target, s, !inverse);
        }

        private void adjustSizes(long target, Resizable2 r, boolean limitToRange)
        {
            long totalLowerBound = 0;
            long totalUpperBound = 0;
            for(int i = 0; i < r.getElementCount(); i++) {
                totalLowerBound += r.getLowerBoundAt(i);
                totalUpperBound += r.getUpperBoundAt(i);
            }

            if (limitToRange) {
                target = Math.min(Math.max(totalLowerBound, target), totalUpperBound);
            }

            for(int i = 0; i < r.getElementCount(); i++) {
                int lowerBound = r.getLowerBoundAt(i);
                int upperBound = r.getUpperBoundAt(i);
                // Check for zero. This happens when the distribution of the delta
                // finishes early due to a series of "fixed" entries at the end.
                // In this case, lowerBound == upperBound, for all subsequent terms.
                int newSize;
                if (totalLowerBound == totalUpperBound) {
                    newSize = lowerBound;
                }
                else {
                    double f = (double)(target - totalLowerBound)/(totalUpperBound - totalLowerBound);
                    newSize = (int)Math.round(lowerBound+f*(upperBound - lowerBound));
                    // We'd need to round manually in an all integer version.
                    // size[i] = (int)(((totalUpperBound - target) * lowerBound +
                    //     (target - totalLowerBound) * upperBound)/(totalUpperBound-totalLowerBound));
                }
                r.setSizeAt(newSize, i);
                target -= newSize;
                totalLowerBound -= lowerBound;
                totalUpperBound -= upperBound;
            }
        }

        private int viewIndexForColumn(TableColumn aColumn)
        {
            TableColumnModel cm = table.getColumnModel();
            for (int column = 0; column < cm.getColumnCount(); column++) {
                if (cm.getColumn(column) == aColumn) {
                    return column;
                }
            }
            return -1;
        }
    }

    public static class TableHeaderWithMargins
            extends JTableHeader
    {
        public TableHeaderWithMargins(TableColumnModel cm)
        {
            super(cm);
        }

        @Override
        public int columnAtPoint(Point point)
        {
            int margin = getLeftMargin();
            if (margin > 0) {
                point = new Point(point.x - margin, point.y);
            }
            return super.columnAtPoint(point);
        }

        @Override
        public Rectangle getHeaderRect(int column)
        {
            Rectangle r = super.getHeaderRect(column);
            int lastColumn = columnModel.getColumnCount() - 1;
            if (lastColumn >= 0) {
                int margin = getLeftMargin();
                r.x += margin;
            }
            return r;
        }

        private int getLeftMargin()
        {
            if (table instanceof InternalTableWithMargins) {
                InternalTableWithMargins t = (InternalTableWithMargins) table;
                return t.margin;
            }
            return 0;
        }
    }
}
