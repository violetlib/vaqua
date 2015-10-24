/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
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

package org.violetlib.aqua;

import java.awt.*;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicTableHeaderUI;
import javax.swing.table.*;

public class AquaTableHeaderUI extends BasicTableHeaderUI {
    protected int borderHeight;
    protected Color borderColor;
    private TableCellRenderer prevRenderer = null;

    public static ComponentUI createUI(final JComponent c) {
        return new AquaTableHeaderUI();
    }

    @Override
    public void installDefaults() {
        super.installDefaults();

        borderHeight = UIManager.getInt("TableHeader.borderHeight");
        borderColor = UIManager.getColor("TableHeader.borderColor");

        prevRenderer = header.getDefaultRenderer();
        if (prevRenderer instanceof UIResource) {
            header.setDefaultRenderer(new AquaTableHeaderCellRenderer());
        }
    }

    @Override
    public void uninstallDefaults() {
        if (header.getDefaultRenderer() instanceof AquaTableHeaderCellRenderer) {
            header.setDefaultRenderer(prevRenderer);
        }

        super.uninstallDefaults();
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        super.paint(g, c);
        g.setColor(borderColor);
        g.fillRect(0, c.getHeight()-borderHeight, c.getWidth(), borderHeight);
    }

    private int getHeaderHeightAqua() {
        int height = 0;
        boolean accomodatedDefault = false;

        final TableColumnModel columnModel = header.getColumnModel();
        for (int column = 0; column < columnModel.getColumnCount(); column++) {
            final TableColumn aColumn = columnModel.getColumn(column);
            // Configuring the header renderer to calculate its preferred size is expensive.
            // Optimise this by assuming the default renderer always has the same height.
            if (aColumn.getHeaderRenderer() != null || !accomodatedDefault) {
                final Component comp = getHeaderRendererAqua(column);
                final int rendererHeight = comp.getPreferredSize().height;
                height = Math.max(height, rendererHeight);
                // If the header value is empty (== "") in the
                // first column (and this column is set up
                // to use the default renderer) we will
                // return zero from this routine and the header
                // will disappear altogether. Avoiding the calculation
                // of the preferred size is such a performance win for
                // most applications that we will continue to
                // use this cheaper calculation, handling these
                // issues as `edge cases'.

                // Mac OS X Change - since we have a border on our renderers
                // it is possible the height of an empty header could be > 0,
                // so we chose the relatively safe number of 4 to handle this case.
                // Now if we get a size of 4 or less we assume it is empty and measure
                // a different header.
                if (rendererHeight > 4) {
                    accomodatedDefault = true;
                }
            }
        }
        return height + borderHeight;
    }

    private Component getHeaderRendererAqua(final int columnIndex) {
        final TableColumn aColumn = header.getColumnModel().getColumn(columnIndex);
        TableCellRenderer renderer = aColumn.getHeaderRenderer();
        if (renderer == null) {
            renderer = header.getDefaultRenderer();
        }
        return renderer.getTableCellRendererComponent(header.getTable(), aColumn.getHeaderValue(), false, false, -1, columnIndex);
    }

    private Dimension createHeaderSizeAqua(long width) {
        // None of the callers include the intercell spacing, do it here.
        if (width > Integer.MAX_VALUE) {
            width = Integer.MAX_VALUE;
        }
        return new Dimension((int)width, getHeaderHeightAqua());
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        long width = 0;
        final Enumeration<TableColumn> enumeration = header.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            final TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getMinWidth();
        }
        return createHeaderSizeAqua(width);
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        long width = 0;
        final Enumeration<TableColumn> enumeration = header.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            final TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getPreferredWidth();
        }
        return createHeaderSizeAqua(width);
    }
}
