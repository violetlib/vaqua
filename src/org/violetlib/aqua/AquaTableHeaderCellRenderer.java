/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

// Based on sun.swing.table.DefaultTableCellHeaderRenderer, see copyright notice below.

import org.violetlib.jnr.aqua.AquaUIPainter;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import java.awt.*;

/**
 * The table header cell renderer. It installs a border that paints the divider and the sort arrow.
 */
@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaTableHeaderCellRenderer extends DefaultTableCellRenderer implements UIResource {

    public AquaTableHeaderCellRenderer() {
        setHorizontalAlignment(LEADING);
    }

    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected, boolean hasFocus, int row, int column) {
        boolean isPaintingForPrint = false;

        AquaUIPainter.ColumnSortArrowDirection sortDirection = AquaUIPainter.ColumnSortArrowDirection.NONE;

        if (table != null) {
            JTableHeader header = table.getTableHeader();
            if (header != null) {
                setForeground(header.getForeground());
                setBackground(header.getBackground());
                setFont(header.getFont());
                isPaintingForPrint = header.isPaintingForPrint();
            }

            if (!isPaintingForPrint && table.getRowSorter() != null) {
                SortOrder sortOrder = getColumnSortOrder(table, column);
                if (sortOrder != null) {
                    switch(sortOrder) {
                    case ASCENDING:
                        sortDirection = AquaUIPainter.ColumnSortArrowDirection.UP;
                        break;
                    case DESCENDING:
                        sortDirection = AquaUIPainter.ColumnSortArrowDirection.DOWN;
                        break;
                    }
                }
            }
        }

        setText((value == null) ? "" : value.toString());

        AquaTableHeaderBorder cellBorder = AquaTableHeaderBorder.getListHeaderBorder();
        cellBorder.setOwner(table);
        cellBorder.setSortArrowDirection(sortDirection);
        setBorder(cellBorder);
        return this;
    }

    public static SortOrder getColumnSortOrder(JTable table, int column) {
        SortOrder rv = null;
        if (table == null || table.getRowSorter() == null) {
            return rv;
        }
        java.util.List<? extends RowSorter.SortKey> sortKeys =
            table.getRowSorter().getSortKeys();
        if (sortKeys.size() > 0 && sortKeys.get(0).getColumn() ==
            table.convertColumnIndexToModel(column)) {
            rv = sortKeys.get(0).getSortOrder();
        }
        return rv;
    }
}

/*
 * Copyright (c) 2005, 2014, Oracle and/or its affiliates. All rights reserved.
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
