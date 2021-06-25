/*
 * Copyright (c) 2018-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTableHeaderUI;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

public class AquaTableHeaderUI extends BasicTableHeaderUI implements AquaComponentUI {

    public static ComponentUI createUI(JComponent c) {
        return new AquaTableHeaderUI();
    }

    protected int borderHeight;
    private TableCellRenderer prevRenderer = null;
    private PropertyChangeListener propertyChangeListener;

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;
    protected @Nullable Color separatorColor;

    // The selected column index superclass concept is effectively impossible to access.

    public AquaTableHeaderUI() {
        this.colors = AquaColors.TABLE_HEADER_COLORS;
    }

    public @Nullable Color getSeparatorColor() {
        return separatorColor;
    }

    public int getBorderHeight() {
        return borderHeight;
    }

    @Override
    public void installDefaults() {
        super.installDefaults();

        borderHeight = UIManager.getInt("TableHeader.borderHeight");

        prevRenderer = header.getDefaultRenderer();
        if (prevRenderer instanceof UIResource) {
            header.setDefaultRenderer(new AquaTableHeaderCellRenderer());
        }
        configureAppearanceContext(null);
    }

    @Override
    public void uninstallDefaults() {
        if (header.getDefaultRenderer() instanceof AquaTableHeaderCellRenderer) {
            header.setDefaultRenderer(prevRenderer);
        }

        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        propertyChangeListener = new AquaPropertyChangeListener();
        header.addPropertyChangeListener(propertyChangeListener);
        AppearanceManager.installListeners(header);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListeners(header);
        header.removePropertyChangeListener(propertyChangeListener);
        propertyChangeListener = null;
        super.uninstallListeners();
    }

    private class AquaPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent ev) {
            String pn = ev.getPropertyName();
            if (pn != null) {
                if (pn.equals("background")) {
                    JTable table = header.getTable();
                    AquaTableUI ui = AquaUtils.getUI(table, AquaTableUI.class);
                    if (ui != null) {
                        ui.repaintScrollPaneCorner();
                    }
                }
            }
        }
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        configureAppearanceContext(null);
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(header);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        AquaColors.installColors(header, appearanceContext, colors);
        EffectName effect = state == AquaUIPainter.State.ACTIVE ? EffectName.EFFECT_NONE : EffectName.EFFECT_DISABLED;
        separatorColor = appearance.getColorForEffect("tableHeaderSeparator", effect);
        header.repaint();
    }

    protected @NotNull AquaUIPainter.State getState() {
        return AquaFocusHandler.isActive(header) ? AquaUIPainter.State.ACTIVE : AquaUIPainter.State.INACTIVE;
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        Color background = c.getBackground();

        if (c.isOpaque()) {
            g.setColor(background);
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }

        AquaTableHeaderPainter p = new AquaTableHeaderPainter(background);
        p.paint(g, c);

        if (separatorColor != null) {
            g.setColor(separatorColor);
            g.fillRect(0, c.getHeight() - borderHeight, c.getWidth(), borderHeight);
        }
    }

    private class AquaTableHeaderPainter {
        private final @NotNull Color background;

        public AquaTableHeaderPainter(@NotNull Color background) {
            this.background = background;
        }

        public void paint(Graphics g, JComponent c) {

            if (header.getColumnModel().getColumnCount() <= 0) {
                return;
            }
            boolean ltr = header.getComponentOrientation().isLeftToRight();

            Rectangle clip = g.getClipBounds();
            Point left = clip.getLocation();
            Point right = new Point( clip.x + clip.width - 1, clip.y );
            TableColumnModel cm = header.getColumnModel();
            int cMin = header.columnAtPoint( ltr ? left : right );
            int cMax = header.columnAtPoint( ltr ? right : left );
            // This should never happen.
            if (cMin == -1) {
                cMin =  0;
            }
            // If the table does not have enough columns to fill the view we'll get -1.
            // Replace this with the index of the last column.
            if (cMax == -1) {
                cMax = cm.getColumnCount()-1;
            }

            TableColumn draggedColumn = header.getDraggedColumn();
            int columnWidth;
            Rectangle cellRect = header.getHeaderRect(ltr ? cMin : cMax);
            TableColumn aColumn;
            if (ltr) {
                for(int column = cMin; column <= cMax ; column++) {
                    aColumn = cm.getColumn(column);
                    columnWidth = aColumn.getWidth();
                    cellRect.width = columnWidth;
                    if (aColumn != draggedColumn) {
                        paintCell(g, cellRect, column);
                    }
                    cellRect.x += columnWidth;
                }
            } else {
                for(int column = cMax; column >= cMin; column--) {
                    aColumn = cm.getColumn(column);
                    columnWidth = aColumn.getWidth();
                    cellRect.width = columnWidth;
                    if (aColumn != draggedColumn) {
                        paintCell(g, cellRect, column);
                    }
                    cellRect.x += columnWidth;
                }
            }

            // Paint the dragged column if we are dragging.
            if (draggedColumn != null) {
                int draggedColumnIndex = viewIndexForColumn(draggedColumn);
                Rectangle draggedCellRect = header.getHeaderRect(draggedColumnIndex);

                // Draw a gray well in place of the moving column.
                g.setColor(header.getParent().getBackground());
                g.fillRect(draggedCellRect.x, draggedCellRect.y,
                        draggedCellRect.width, draggedCellRect.height);

                draggedCellRect.x += header.getDraggedDistance();

                // Fill the background.
                g.setColor(background);
                g.fillRect(draggedCellRect.x, draggedCellRect.y,
                        draggedCellRect.width, draggedCellRect.height);

                paintCell(g, draggedCellRect, draggedColumnIndex);
            }

            // Remove all components in the rendererPane.
            rendererPane.removeAll();
        }

        private Component getHeaderRenderer(int columnIndex) {
            TableColumn aColumn = header.getColumnModel().getColumn(columnIndex);
            TableCellRenderer renderer = aColumn.getHeaderRenderer();
            if (renderer == null) {
                renderer = header.getDefaultRenderer();
            }

            boolean hasFocus = !header.isPaintingForPrint()
                    // && (columnIndex == getSelectedColumnIndex())
                    && header.hasFocus();

            Component rendererComponent = renderer.getTableCellRendererComponent(header.getTable(),
                    aColumn.getHeaderValue(),
                    false, hasFocus,
                    -1, columnIndex);

            return rendererComponent;
        }

        private void paintCell(Graphics g, Rectangle cellRect, int columnIndex) {
            Component rendererComponent = getHeaderRenderer(columnIndex);

            Color bc = getOverrideCellBackground(columnIndex);
            if (bc != null) {
                rendererComponent.setBackground(bc);
            }

            Color bg = rendererComponent.getBackground();
            if (bg instanceof ColorUIResource) {
                Color rowBackground = AquaColors.CLEAR;
                rendererComponent.setBackground(rowBackground);
            }

            Color fg = rendererComponent.getForeground();
            if (fg instanceof ColorUIResource) {
                boolean isSelected = false; // TBD
                assert appearanceContext != null;
                AppearanceContext c = appearanceContext.withSelected(isSelected);
                Color foreground = colors.getForeground(c);
                rendererComponent.setForeground(foreground);
            }

            rendererPane.paintComponent(g, rendererComponent, header, cellRect.x, cellRect.y,
                    cellRect.width, cellRect.height, true);

            // Setting the foreground or background color of a DefaultTableCellRenderer makes that color the color
            // to use when the cell is not selected. So, if we installed a color, we should also remove it.
            Color fc = rendererComponent.getForeground();
            if (fc instanceof UIResource) {
                rendererComponent.setForeground(null);
            }
            bc = rendererComponent.getBackground();
            if (bc instanceof UIResource) {
                rendererComponent.setBackground(null);
            }
        }

        protected Color getOverrideCellBackground(int column) {
            return null;
        }

        private int viewIndexForColumn(TableColumn aColumn) {
            TableColumnModel cm = header.getColumnModel();
            for (int column = 0; column < cm.getColumnCount(); column++) {
                if (cm.getColumn(column) == aColumn) {
                    return column;
                }
            }
            return -1;
        }
    }

    private int getHeaderHeightAqua() {
        int height = 0;
        boolean accomodatedDefault = false;

        TableColumnModel columnModel = header.getColumnModel();
        for (int column = 0; column < columnModel.getColumnCount(); column++) {
            TableColumn aColumn = columnModel.getColumn(column);
            // Configuring the header renderer to calculate its preferred size is expensive.
            // Optimise this by assuming the default renderer always has the same height.
            if (aColumn.getHeaderRenderer() != null || !accomodatedDefault) {
                Component comp = getHeaderRendererAqua(column);
                int rendererHeight = comp.getPreferredSize().height;
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

    private Component getHeaderRendererAqua(int columnIndex) {
        TableColumn aColumn = header.getColumnModel().getColumn(columnIndex);
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
    public Dimension getMinimumSize(JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = header.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getMinWidth();
        }
        return createHeaderSizeAqua(width);
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = header.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getPreferredWidth();
        }
        return createHeaderSizeAqua(width);
    }
}
