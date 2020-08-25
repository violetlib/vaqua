/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2012, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTableUI;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/**
 * A table UI based on AquaTableUI for Yosemite. It implements the striped style. It paints the selection background
 * behind the entire selected row, to avoid gaps between cells. It disables the grid by default. It displays using an
 * inactive style when not the focus owner. It works around a problem in JTable that interprets Meta (Command) as an
 * ordinary key instead of a modifier.
 *
 * For best results using the striped style, cell renderer components should not be opaque, and the table should use
 * auto resizing and setFillsViewportHeight(true).
 */
public class AquaTableUI extends BasicTableUI implements SelectionRepaintable {
    public static ComponentUI createUI(final JComponent c) {
        return new AquaTableUI();
    }

    public static final String TABLE_STYLE_KEY = "JTable.style";
    public static final String QUAQUA_TABLE_STYLE_KEY = "Quaqua.Table.style";

    protected final PropertyChangeListener propertyChangeListener;
    protected final ListSelectionListener selectionListener;

    protected TableCellRenderer originalBooleanRenderer;
    protected AquaTablePainter painter;

    public AquaTableUI() {
        propertyChangeListener = new TablePropertyChangeListener();
        selectionListener = new SelectionListener();
    }

    public static boolean isStriped(JTable table) {
        String value = getStyleProperty(table);
        return value != null && value.equals("striped");
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, TABLE_STYLE_KEY, QUAQUA_TABLE_STYLE_KEY);
    }

    protected static String getStyleProperty(JTable table) {
        return AquaUtils.getProperty(table, TABLE_STYLE_KEY, QUAQUA_TABLE_STYLE_KEY);
    }

    /**
     * Creates the focus listener to repaint the selection when the focus changes.
     */
    protected FocusListener createFocusListener() {
        return new AquaTableUI.FocusHandler();
    }

    /**
     * Creates the mouse listener for the JTable.
     */
    protected MouseInputListener createMouseInputListener() {
        return new AquaTableUI.MouseInputHandler();
    }

    public class FocusHandler implements FocusListener {
        public void focusGained(final FocusEvent e) {
            repaintSelection();
        }

        public void focusLost(final FocusEvent e) {
            repaintSelection();
        }
    }

    protected class TablePropertyChangeListener implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent ev) {
            String pn = ev.getPropertyName();
            if (pn != null) {
                if (pn.equals(AquaFocusHandler.FRAME_ACTIVE_PROPERTY) || pn.equals("enabled")) {
                    repaintSelection();
                    return;
                }
                if (pn.equals("selectionModel")) {
                    ListSelectionModel old = (ListSelectionModel) ev.getOldValue();
                    updateSelectionListener(old);
                }
                if (isStyleProperty(pn)) {
                    table.repaint();
                }
            }
        }
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        painter = new AquaTablePainter(table, rendererPane);
    }

    @Override
    public void uninstallUI(JComponent c) {
        painter = null;
        super.uninstallUI(c);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        table.putClientProperty("terminateEditOnFocusLost", true);
        table.putClientProperty(AquaCellEditorPolicy.IS_CELL_CONTAINER_PROPERTY, true);
        table.setShowHorizontalLines(false);
        table.setShowVerticalLines(false);
        LookAndFeel.installProperty(table, "rowHeight", 19);
        originalBooleanRenderer = installRendererIfPossible(Boolean.class, new AquaBooleanRenderer());
    }

    @Override
    protected void uninstallDefaults() {
        TableCellRenderer booleanRenderer = table.getDefaultRenderer(Boolean.class);
        if (booleanRenderer instanceof AquaBooleanRenderer) {
            table.setDefaultRenderer(Boolean.class, originalBooleanRenderer);
        }
        super.uninstallDefaults();
    }

    protected void installListeners() {
        super.installListeners();
        table.addPropertyChangeListener(propertyChangeListener);
        updateSelectionListener(null);
    }

    protected void uninstallListeners() {
        table.getSelectionModel().removeListSelectionListener(selectionListener);
        table.removePropertyChangeListener(propertyChangeListener);
        super.uninstallListeners();
    }

    // TODO: Using default handler for now, need to handle cmd-key

    // Replace the mouse event with one that returns the cmd-key state when asked
    // for the control-key state, which super assumes is what everyone does to discontiguously extend selections
    public class MouseInputHandler extends BasicTableUI.MouseInputHandler {
        /*public void mousePressed(final MouseEvent e) {
            super.mousePressed(new SelectionMouseEvent(e));
        }
        public void mouseDragged(final MouseEvent e) {
            super.mouseDragged(new SelectionMouseEvent(e));
        }*/
    }

    @Override
    protected KeyListener createKeyListener() {
        KeyListener base = super.createKeyListener();
        return new AquaTableKeyHandler(base);
    }

    protected class AquaTableKeyHandler implements KeyListener {
        protected KeyListener base;

        public AquaTableKeyHandler(KeyListener base) {
            this.base = base;
        }

        @Override
        public void keyPressed(KeyEvent e) {
            // Eat away META down keys..
            // We need to do this, because the JTable.processKeyBinding(…)
            // method does not treat VK_META as a modifier key, and starts
            // editing a cell whenever this key is pressed.

            // XXX - This is bogus but seems to work. Consider disabling
            // automatic editing in JTable by setting the client property
            // "JTable.autoStartsEdit" to Boolean.FALSE and doing all the
            // processing here.

            if (e.getKeyCode() == KeyEvent.VK_META) {
                e.consume();
            } else if (base != null) {
                base.keyPressed(e);
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {
            if (base != null) {
                base.keyReleased(e);
            }
        }

        @Override
        public void keyTyped(KeyEvent e) {
            if (base != null) {
                base.keyTyped(e);
            }
        }
    }

    protected void updateSelectionListener(ListSelectionModel old) {
        if (old != null) {
            old.removeListSelectionListener(selectionListener);
        }
        table.getSelectionModel().addListSelectionListener(selectionListener);
    }

    /**
     * Because JTable takes charge of repainting on a row selection change instead of deferring to the TableUI, we must
     * duplicate its code to ensure that the full width of the table is repainted, not just the cells.
     */
    protected class SelectionListener implements ListSelectionListener {
        @Override
        public void valueChanged(ListSelectionEvent e) {
            if (table.getRowCount() <= 0 || table.getColumnCount() <= 0) {
                return;
            }
            int firstIndex = limit(e.getFirstIndex(), 0, table.getRowCount() - 1);
            int lastIndex = limit(e.getLastIndex(), 0, table.getRowCount()-1);
            Rectangle firstRowRect = table.getCellRect(firstIndex, 0, true);
            Rectangle lastRowRect = table.getCellRect(lastIndex, 0, true);
            Rectangle dirtyRegion = new Rectangle(firstRowRect.x, firstRowRect.y, table.getWidth(), lastRowRect.y + lastRowRect.height);
            table.repaint(dirtyRegion);
        }

        protected int limit(int i, int a, int b) {
            return Math.min(b, Math.max(i, a));
        }
    }

    /**
     * This method is called after a possible change to the state that affects the display of selected cells.
     */
    @Override
    public void repaintSelection() {
        // All of the selected cells must be repainted when the focus/active/enabled state changes, because the selected
        // cell background depends upon these states.

        AquaFocusHandler.swapSelectionColors("Table", table, tableHasFocus());
        table.repaint();
    }

    protected boolean tableHasFocus() {
        if (table.isEditing()) {
            return AquaFocusHandler.isActive(table);
        } else {
            return AquaFocusHandler.hasFocus(table);
        }
    }

    public void paint(Graphics g, JComponent c) {
        if (painter != null) {
            painter.paint(g, c);
        }
    }

    protected class AquaTablePainter extends BasicTableUIPainter {

        protected final Color[] stripes;

        protected boolean tableHasFocus;
        protected boolean isStriped;
        protected Color selectedBackground;
        protected Color selectedForeground;

        public AquaTablePainter(JTable table, CellRendererPane rendererPane) {
            super(table, rendererPane);

            stripes = new Color[]{UIManager.getColor("Table.evenRowBackground"), UIManager.getColor("Table.oddRowBackground")};
        }

        public void paint(Graphics g, JComponent c) {

            tableHasFocus = tableHasFocus();
            selectedBackground = table.getSelectionBackground();
            selectedForeground = table.getSelectionForeground();

            isStriped = AquaTableUI.isStriped(table);
            boolean isSelection = table.getSelectedRowCount() > 0 && table.getRowSelectionAllowed()
                    || table.getSelectedColumnCount() > 0 && table.getColumnSelectionAllowed();

            // Most of the following code is copied from BasicTableUI, with minor changes.

            Rectangle clip = g.getClipBounds();

            Rectangle bounds = table.getBounds();
            // account for the fact that the graphics has already been translated
            // into the table's bounds
            bounds.x = bounds.y = 0;

            // The following is changed because we want to paint stripes even if there are no rows or columns.
            if (!bounds.intersects(clip)) {
                return;
            }

            boolean ltr = table.getComponentOrientation().isLeftToRight();

            Point upperLeft = clip.getLocation();
            Point lowerRight = new Point(clip.x + clip.width - 1,
                    clip.y + clip.height - 1);

            int rMin = table.rowAtPoint(upperLeft);
            int rMax = table.rowAtPoint(lowerRight);
            // This should never happen (as long as our bounds intersect the clip,
            // which is why we bail above if that is the case).
            if (rMin == -1) {
                rMin = 0;
            }
            // If the table does not have enough rows to fill the view we'll get -1.
            // (We could also get -1 if our bounds don't intersect the clip,
            // which is why we bail above if that is the case).
            // Replace this with the index of the last row.
            if (rMax == -1) {
                rMax = table.getRowCount() - 1;
            }

            int cMin = table.columnAtPoint(ltr ? upperLeft : lowerRight);
            int cMax = table.columnAtPoint(ltr ? lowerRight : upperLeft);
            // This should never happen.
            if (cMin == -1) {
                cMin = 0;
            }
            // If the table does not have enough columns to fill the view we'll get -1.
            // Replace this with the index of the last column.
            if (cMax == -1) {
                cMax = table.getColumnCount() - 1;
            }

            if (isStriped || isSelection) {
                paintBackground(g, rMin, rMax, cMin, cMax);
            }

            paintGrid(g, rMin, rMax, cMin, cMax);
            paintCells(g, rMin, rMax, cMin, cMax);
        }

        protected void paintBackground(Graphics g, int rMin, int rMax, int cMin, int cMax) {
            Rectangle clip = g.getClipBounds();

            boolean isRowSelection = table.getSelectedRowCount() > 0 && table.getRowSelectionAllowed() && !table.getColumnSelectionAllowed();
            boolean isColumnSelection = table.getSelectedColumnCount() > 0 && table.getColumnSelectionAllowed() && !table.getRowSelectionAllowed();

            int nextRowY = 0;

            for (int row = rMin; row <= rMax; row++) {
                Rectangle cellRect = table.getCellRect(row, cMin, true);
                boolean isSelected = isRowSelection && table.isRowSelected(row);
                Color bg = isSelected ? selectedBackground : (isStriped ? stripes[row % 2] : null);
                if (bg == null) {
                    bg = table.getBackground();
                }
                g.setColor(bg);
                g.fillRect(clip.x, cellRect.y, clip.width, cellRect.height);
                nextRowY = cellRect.y + cellRect.height;
            }

            if (isStriped) {
                int clipTop = clip.y + clip.height;
                if (nextRowY < clipTop) {
                    int rowHeight = table.getRowHeight();
                    if (rowHeight > 0) {
                        int row = rMax + 1;
                        while (nextRowY < clipTop) {
                            Color bg = stripes[row % 2];
                            g.setColor(bg);
                            g.fillRect(clip.x, nextRowY, clip.width, rowHeight);
                            row++;
                            nextRowY += rowHeight;
                        }
                    }
                }
            }

            // TBD: should selected column be painted here or is it OK for just the cells to paint the selection background?
        }

        @Override
        protected void paintGrid(Graphics g, int rMin, int rMax, int cMin, int cMax) {
            g.setColor(table.getGridColor());

            Rectangle minCell = table.getCellRect(rMin, cMin, true);
            Rectangle maxCell = table.getCellRect(rMax, cMax, true);
            Rectangle damagedArea = minCell.union( maxCell );

            if (table.getShowHorizontalLines()) {
                // horizontal grid lines should be as wide as the striped background
                if (isStriped) {
                    Rectangle clip = g.getClipBounds();
                    int y = damagedArea.y;
                    for (int row = rMin; row <= rMax; row++) {
                        y += table.getRowHeight(row);
                        g.fillRect(clip.x, y - 1, clip.width, 1);
                    }
                } else {
                    int tableWidth = damagedArea.x + damagedArea.width;
                    int y = damagedArea.y;
                    for (int row = rMin; row <= rMax; row++) {
                        y += table.getRowHeight(row);
                        g.fillRect(damagedArea.x, y-1, tableWidth, 1);
                    }
                }
            }

            if (table.getShowVerticalLines()) {
                TableColumnModel cm = table.getColumnModel();
                int tableHeight = damagedArea.y + damagedArea.height;
                int x;
                if (table.getComponentOrientation().isLeftToRight()) {
                    x = damagedArea.x;
                    for (int column = cMin; column <= cMax; column++) {
                        int w = cm.getColumn(column).getWidth();
                        x += w;
                        g.fillRect(x - 1, 0, 1, tableHeight);
                    }
                } else {
                    x = damagedArea.x;
                    for (int column = cMax; column >= cMin; column--) {
                        int w = cm.getColumn(column).getWidth();
                        x += w;
                        g.fillRect(x - 1, 0, 1, tableHeight);
                    }
                }
            }
        }

        @Override
        protected void paintCell(Graphics g, Rectangle cellRect, int row, int column) {
            if (table.isEditing() && table.getEditingRow()==row && table.getEditingColumn()==column) {
                Component component = table.getEditorComponent();
                component.setBounds(cellRect);
                component.validate();
            }
            else {
                TableCellRenderer renderer = table.getCellRenderer(row, column);
                Component component = table.prepareRenderer(renderer, row, column);
                Color bc = getOverrideCellBackground(row, column);
                if (bc != null) {
                    component.setBackground(bc);
                }
                rendererPane.paintComponent(g, component, table, cellRect.x, cellRect.y,
                        cellRect.width, cellRect.height, true);
                // Setting the background color of a DefaultTableCellRenderer makes that color the color to use instead
                // of the table background when the cell is not selected. So, if we installed a color, we should also
                // remove it.
                bc = component.getBackground();
                if (bc instanceof UIResource) {
                    component.setBackground(null);
                }
            }
        }

        /**
         * Here we reverse engineer DefaultTableCellRenderer to determine when it would install the list background
         * color in the renderer component. If the table is striped, then we intend to override that background color.
         */
        protected Color getOverrideCellBackground(int row, int column) {
            if (!isStriped) {
                return null;
            }

            if (table.isCellSelected(row, column)) {
                return null;
            }

            JTable.DropLocation dropLocation = table.getDropLocation();
            if (dropLocation != null
                    && !dropLocation.isInsertRow()
                    && !dropLocation.isInsertColumn()
                    && dropLocation.getRow() == row
                    && dropLocation.getColumn() == column) {

                return null;
            }

            return stripes[row % 2];
        }

        protected void paintDraggedArea(Graphics g, int rMin, int rMax, TableColumn draggedColumn, int distance) {
            int draggedColumnIndex = viewIndexForColumn(draggedColumn);

            Rectangle minCell = table.getCellRect(rMin, draggedColumnIndex, true);
            Rectangle maxCell = table.getCellRect(rMax, draggedColumnIndex, true);

            Rectangle vacatedColumnRect = minCell.union(maxCell);

            // Paint a gray well in place of the moving column.
            g.setColor(table.getParent().getBackground());
            g.fillRect(vacatedColumnRect.x, vacatedColumnRect.y,
                    vacatedColumnRect.width, vacatedColumnRect.height);

            // Move to the where the cell has been dragged.
            vacatedColumnRect.x += distance;

            // Fill the background.
            paintDraggedAreaBackground(g, vacatedColumnRect, rMin, rMax);

            // Paint the vertical grid lines if necessary.
            if (table.getShowVerticalLines()) {
                g.setColor(table.getGridColor());
                int x1 = vacatedColumnRect.x;
                int y1 = vacatedColumnRect.y;
                int x2 = x1 + vacatedColumnRect.width - 1;
                int y2 = y1 + vacatedColumnRect.height - 1;
                // Left
                g.drawLine(x1-1, y1, x1-1, y2);
                // Right
                g.drawLine(x2, y1, x2, y2);
            }

            for(int row = rMin; row <= rMax; row++) {
                // Render the cell value
                Rectangle r = table.getCellRect(row, draggedColumnIndex, false);
                r.x += distance;
                paintCell(g, r, row, draggedColumnIndex);

                // Paint the (lower) horizontal grid line if necessary.
                if (table.getShowHorizontalLines()) {
                    g.setColor(table.getGridColor());
                    Rectangle rcr = table.getCellRect(row, draggedColumnIndex, true);
                    rcr.x += distance;
                    int x1 = rcr.x;
                    int y1 = rcr.y;
                    int x2 = x1 + rcr.width - 1;
                    int y2 = y1 + rcr.height - 1;
                    g.drawLine(x1, y2, x2, y2);
                }
            }
        }

        protected void paintDraggedAreaBackground(Graphics g, Rectangle columnRect, int rMin, int rMax) {
            if (isStriped) {
                Graphics gg = g.create();
                try {
                    gg.clipRect(columnRect.x, columnRect.y, columnRect.width, columnRect.height);
                    paintBackground(gg, rMin, rMax, 0, table.getColumnCount()-1);
                } finally {
                    gg.dispose();
                }
            } else {
                g.setColor(table.getBackground());
                g.fillRect(columnRect.x, columnRect.y,
                        columnRect.width, columnRect.height);
            }
        }
    }

    protected TableCellRenderer installRendererIfPossible(Class<?> objectClass, TableCellRenderer renderer) {
        TableCellRenderer currentRenderer = table.getDefaultRenderer(objectClass);
        if (currentRenderer instanceof UIResource) {
            table.setDefaultRenderer(objectClass, renderer);
        }
        return currentRenderer;
    }

    protected class AquaBooleanRenderer extends JCheckBox implements TableCellRenderer, UIResource {

        public AquaBooleanRenderer() {
            setHorizontalAlignment(JLabel.CENTER);
            setBorderPainted(false);
            setOpaque(false);
        }

        public Component getTableCellRendererComponent(JTable table, Object value,
                                                       boolean isSelected, boolean hasFocus, int row, int column) {
            if (isSelected) {
                setForeground(table.getSelectionForeground());
            } else {
                setForeground(table.getForeground());
            }
            // TBD: Setting the background to a transparent color is a work around for the code in
            // AquaButtonLabeledUI that ignores opaque for cell renderers. Not sure why it does that.
            // Alternatively, we could install the appropriate background just as DefaultTableCellRenderer does.
            setBackground(new Color(0, 0, 0, 0));   //
            setSelected((value != null && (Boolean) value));
            return this;
        }
    }
}
