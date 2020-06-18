/*
 * Copyright (c) 2014-2018 Alan Snyder.
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
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTableUI;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * A table UI based on AquaTableUI for Yosemite. It implements the striped style. It paints the selection background
 * behind the entire selected row, to avoid gaps between cells. It disables the grid by default. It displays using an
 * inactive style when not the focus owner. It works around a problem in JTable that interprets Meta (Command) as an
 * ordinary key instead of a modifier.
 *
 * For best results using the striped style, cell renderer components should not be opaque, and the table should use
 * auto resizing and setFillsViewportHeight(true).
 */
public class AquaTableUI extends BasicTableUI
        implements SelectionRepaintable, AquaComponentUI {
    public static ComponentUI createUI(JComponent c) {
        return new AquaTableUI();
    }

    public static final String TABLE_STYLE_KEY = "JTable.style";
    public static final String QUAQUA_TABLE_STYLE_KEY = "Quaqua.Table.style";

    protected final PropertyChangeListener propertyChangeListener;
    protected final ListSelectionListener selectionListener;
    private @Nullable CellEditorFocusManager cellEditorFocusManager;
    protected TableCellRenderer originalBooleanRenderer;
    protected AquaTablePainter painter;

    private boolean isStriped = false;
    protected @NotNull ContainerContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;
    protected @Nullable Color actualTableBackground;

    public AquaTableUI() {
        propertyChangeListener = new TablePropertyChangeListener();
        selectionListener = new SelectionListener();
        cellEditorFocusManager = new CellEditorFocusManager();
        this.colors = AquaColors.CONTAINER_COLORS;
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
        public void focusGained(FocusEvent e) {
            focusChanged();
        }

        public void focusLost(FocusEvent e) {
            focusChanged();
        }

        private void focusChanged() {
            configureAppearanceContext(null);
        }
    }

    // This class removes the cell editor when it permanently loses focus.

    protected class CellEditorFocusManager implements FocusListener {
        private @Nullable Component managedCellEditorComponent;

        public CellEditorFocusManager() {
        }

        public void cellEditorChanged(@Nullable TableCellEditor oldEditor, @Nullable TableCellEditor currentEditor) {
            detach();
            if (currentEditor != null) {
                managedCellEditorComponent = table.getEditorComponent();
                managedCellEditorComponent.addFocusListener(this);
            }
        }

        public void detach() {
            if (managedCellEditorComponent != null) {
                managedCellEditorComponent.removeFocusListener(this);
                managedCellEditorComponent = null;
            }
        }

        @Override
        public void focusGained(FocusEvent e) {
        }

        @Override
        public void focusLost(FocusEvent e) {
            if (!e.isTemporary()) {
                TableCellEditor editor = table.getCellEditor();
                if (editor != null) {
                    if (!editor.stopCellEditing()) {
                        editor.cancelCellEditing();
                    }
                }
            }
        }
    }

    protected class TablePropertyChangeListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent ev) {
            String pn = ev.getPropertyName();
            if (pn != null) {
                if (pn.equals("enabled")) {
                    configureAppearanceContext(null);
                    return;
                }
                if (pn.equals("selectionModel")) {
                    ListSelectionModel old = (ListSelectionModel) ev.getOldValue();
                    updateSelectionListener(old);
                }
                if (isStyleProperty(pn)) {
                    updateStriped();
                    table.repaint();
                }
                if (pn.equals("background")) {
                    repaintScrollPaneCorner();
                }
                if (pn.equals("tableCellEditor")) {
                    if (cellEditorFocusManager != null) {
                        TableCellEditor oldEditor = (TableCellEditor) ev.getOldValue();
                        TableCellEditor editor = (TableCellEditor) ev.getNewValue();
                        cellEditorFocusManager.cellEditorChanged(oldEditor, editor);
                    }
                }
            }
        }
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        painter = new AquaTablePainter(table, rendererPane);
        table.putClientProperty("terminateEditOnFocusLost", true);
        table.putClientProperty("JTable.autoStartsEdit", false);  // do not simulate an open cell editor
        table.putClientProperty(AquaCellEditorPolicy.IS_CELL_CONTAINER_PROPERTY, true);
        table.setShowHorizontalLines(false);
        table.setShowVerticalLines(false);
        LookAndFeel.installProperty(table, "rowHeight", 19);
        originalBooleanRenderer = installRendererIfPossible(Boolean.class, new AquaBooleanRenderer());
        isStriped = getStripedValue();
        configureAppearanceContext(null);
    }

    @Override
    protected void uninstallDefaults() {
        TableCellRenderer booleanRenderer = table.getDefaultRenderer(Boolean.class);
        if (booleanRenderer instanceof AquaBooleanRenderer) {
            table.setDefaultRenderer(Boolean.class, originalBooleanRenderer);
        }
        painter = null;
        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        table.addPropertyChangeListener(propertyChangeListener);
        updateSelectionListener(null);
        AppearanceManager.installListener(table);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListener(table);
        table.getSelectionModel().removeListSelectionListener(selectionListener);
        table.removePropertyChangeListener(propertyChangeListener);
        cellEditorFocusManager.detach();
        super.uninstallListeners();
    }

    // TODO: Using default handler for now, need to handle cmd-key

    // Replace the mouse event with one that returns the cmd-key state when asked
    // for the control-key state, which super assumes is what everyone does to discontiguously extend selections
    public class MouseInputHandler extends BasicTableUI.MouseInputHandler {
        /*public void mousePressed(MouseEvent e) {
            super.mousePressed(new SelectionMouseEvent(e));
        }
        public void mouseDragged(MouseEvent e) {
            super.mouseDragged(new SelectionMouseEvent(e));
        }*/
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
            appearance = AppearanceManager.ensureAppearance(table);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        colors = isStriped ? AquaColors.STRIPED_CONTAINER_COLORS : AquaColors.CONTAINER_COLORS;
        colors.configureForContainer();
        actualTableBackground = colors.getBackground(appearanceContext);
        AquaColors.installColors(table, appearanceContext, colors);
        table.repaint();
    }

    protected AquaUIPainter.State getState() {
        return table.isEnabled()
                ? (shouldDisplayAsFocused() ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE)
                : AquaUIPainter.State.DISABLED;
    }

    protected boolean shouldDisplayAsFocused() {
        return AquaFocusHandler.hasFocus(table) || table.isEditing() && AquaFocusHandler.isActive(table);
    }

    private void updateStriped() {
        boolean value = getStripedValue();
        if (value != isStriped) {
            isStriped = value;
            configureAppearanceContext(null);
        }
    }

    private boolean getStripedValue() {
        String value = getStyleProperty(table);
        return "striped".equals(value) && isBackgroundClear();
    }

    private boolean isBackgroundClear() {
        Color c = table.getBackground();
        return c == null || c.getAlpha() == 0 || c instanceof ColorUIResource;
    }

    public boolean isStriped() {
        return isStriped;
    }

    protected boolean tableHasFocus() {
        if (table.isEditing()) {
            return AquaFocusHandler.isActive(table);
        } else {
            return AquaFocusHandler.hasFocus(table);
        }
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, TABLE_STYLE_KEY, QUAQUA_TABLE_STYLE_KEY);
    }

    protected static String getStyleProperty(JTable table) {
        return AquaUtils.getProperty(table, TABLE_STYLE_KEY, QUAQUA_TABLE_STYLE_KEY);
    }

    /**
     * This method is called after a possible change to the state that affects the display of selected cells.
     */
    @Override
    public void repaintSelection() {
        // All of the selected cells must be repainted when the focus/active/enabled state changes, because the selected
        // cell background depends upon these states.

        configureAppearanceContext(null);
    }

    @Override
    public void update(Graphics g, JComponent c) {

        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        Color background = getBackgroundColor();
        if (background != null) {
            g.setColor(background);
            g.fillRect(0, 0, c.getWidth(),c.getHeight());
        }

        paint(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    private @Nullable Color getBackgroundColor() {
        if (table.isOpaque()) {
            if (isStriped && actualTableBackground != null) {
                // The dark mode stripes presume a dark background.
                return actualTableBackground;
            } else {
                return table.getBackground();
            }
        }
        return null;
    }

    public void repaintScrollPaneCorner() {
        Container p1 = table.getParent();
        if (p1 instanceof JViewport) {
            Container p2 = p1.getParent();
            if (p2 instanceof JScrollPane) {
                JScrollPane sp = (JScrollPane) p2;
                Component corner = sp.getCorner(JScrollPane.UPPER_TRAILING_CORNER);
                if (corner != null) {
                    corner.repaint();
                }
            }
        }
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        if (painter != null && appearanceContext != null) {
            painter.paint(g, c);
        }
    }

    protected class AquaTablePainter extends BasicTableUIPainter {

        protected boolean tableHasFocus;

        public AquaTablePainter(JTable table, CellRendererPane rendererPane) {
            super(table, rendererPane);
        }

        public void paint(Graphics g, JComponent c) {

            assert appearanceContext != null;

            tableHasFocus = tableHasFocus();

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
            boolean extendVerticalGrid = false;

            // This should never happen (as long as our bounds intersect the clip,
            // which is why we bail above if that is the case).
            if (rMin == -1) {
                rMin = 0;
            }
            // If the table does not have enough rows to fill the view we'll get -1.
            // (We could also get -1 if our bounds don't intersect the clip,
            // which is why we bail above if that is the case).
            if (rMax == -1) {
                rMax = table.getRowCount() - 1;
                extendVerticalGrid = true;
            }

            int cMin = table.columnAtPoint(ltr ? upperLeft : lowerRight);
            int cMax = table.columnAtPoint(ltr ? lowerRight : upperLeft);
            boolean extendHorizontalGrid = false;
            // This should never happen.
            if (cMin == -1) {
                cMin = 0;
            }
            // If the table does not have enough columns to fill the view we'll get -1.
            if (cMax == -1) {
                cMax = table.getColumnCount() - 1;
                extendHorizontalGrid = true;
            }

            colors.configureForContainer();

            if (isStriped || isSelection) {
                paintBackground(g, rMin, rMax, cMin, cMax);
            }

            paintGrid(g, rMin, rMax, cMin, cMax, extendVerticalGrid, extendHorizontalGrid);
            paintCells(g, rMin, rMax, cMin, cMax);
            colors.configureForContainer();
        }

        protected void paintBackground(Graphics g, int rMin, int rMax, int cMin, int cMax) {
            Rectangle clip = g.getClipBounds();

            boolean isRowSelection = table.getSelectedRowCount() > 0 && table.getRowSelectionAllowed() && !table.getColumnSelectionAllowed();
            boolean isColumnSelection = table.getSelectedColumnCount() > 0 && table.getColumnSelectionAllowed() && !table.getRowSelectionAllowed();

            assert appearanceContext != null;

            // Note: the table is configured with colors for the entire table, not with colors for individual rows

            int nextRowY = 0;

            for (int row = rMin; row <= rMax; row++) {
                Rectangle cellRect = table.getCellRect(row, cMin, true);
                boolean isSelected = isRowSelection && table.isRowSelected(row);
                if (isStriped) {
                    colors.configureForRow(row, isSelected);
                } else {
                    colors.configureForRow(isSelected);
                }
                Color rowBackground = colors.getBackground(appearanceContext);
                if (!isStriped && !isSelected) {
                    Color c = table.getBackground();
                    if (AquaColors.isPriority(c)) {
                        rowBackground = c;
                    }
                }
                g.setColor(rowBackground);
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
                            colors.configureForRow(row, false);
                            Color bg = colors.getBackground(appearanceContext);
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

        protected void paintGrid(Graphics g, int rMin, int rMax, int cMin, int cMax, boolean xVertical, boolean xHorizontal) {

            assert appearanceContext != null;
            Color gridColor = colors.getGrid(appearanceContext);
            g.setColor(gridColor);

            Rectangle minCell = table.getCellRect(rMin, cMin, true);
            Rectangle maxCell = table.getCellRect(rMax, cMax, true);
            Rectangle damagedArea = minCell.union( maxCell );

            if (table.getShowHorizontalLines()) {
                if (xHorizontal) {
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
                if (xVertical) {
                    Rectangle clip = g.getClipBounds();
                    tableHeight = clip.y + clip.height;
                }
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
                paintEditorCell(g, cellRect, row, column);
            } else {
                paintRenderedCell(g, cellRect, row, column);
            }
        }

        protected void paintEditorCell(@NotNull Graphics g,
                                       @NotNull Rectangle cellRect,
                                       int row,
                                       int column) {
            Component component = table.getEditorComponent();
            component.setBounds(cellRect);
            component.validate();
        }

        protected void paintRenderedCell(@NotNull Graphics g,
                                         @NotNull Rectangle cellRect,
                                         int row,
                                         int column) {

            TableCellRenderer renderer = table.getCellRenderer(row, column);
            Component rendererComponent = table.prepareRenderer(renderer, row, column);
            rendererPane.paintComponent(g, rendererComponent, table, cellRect.x, cellRect.y,
                    cellRect.width, cellRect.height, true);
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

            assert appearanceContext != null;
            Color gridColor = colors.getGrid(appearanceContext);

            // Paint the vertical grid lines if necessary.
            if (table.getShowVerticalLines()) {
                g.setColor(gridColor);
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
                    g.setColor(gridColor);
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
                Color background = getBackgroundColor();
                if (background != null) {
                    g.setColor(background);
                    g.fillRect(columnRect.x, columnRect.y, columnRect.width, columnRect.height);
                }
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
            setBackground(AquaColors.CLEAR);
            setSelected((value != null && (Boolean) value));
            return this;
        }
    }
}
