/*
 * Changes Copyright (c) 2015 Alan Snyder.
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
import java.awt.event.*;
import java.beans.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicListUI;

import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.GradientWidget;
import org.violetlib.jnr.aqua.GradientConfiguration;

/**
 * A Mac L&F implementation of JList
 */
public class AquaListUI extends BasicListUI {
    public static ComponentUI createUI(final JComponent c) {
        return new AquaListUI();
    }

    public static final String LIST_STYLE_KEY = "JList.style";
    public static final String QUAQUA_LIST_STYLE_KEY = "Quaqua.List.style";

    private boolean isStriped = false;

    protected Color[] stripes;

    public AquaListUI() {
        stripes = new Color[]{UIManager.getColor("List.evenRowBackground"), UIManager.getColor("List.oddRowBackground")};
    }

    /**
     * Creates the focus listener to repaint the focus ring
     */
    protected FocusListener createFocusListener() {
        return new AquaListUI.FocusHandler();
    }

    /**
     * Creates a delegate that implements MouseInputListener.
     */
    protected MouseInputListener createMouseInputListener() {
        return new AquaListMouseBehavior(new JListModel(list));
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        list.putClientProperty(AquaCellEditorPolicy.IS_CELL_CONTAINER_PROPERTY, true);
        updateStriped();
    }

    protected void installKeyboardActions() {
        super.installKeyboardActions();
        list.getActionMap().put("aquaHome", new AquaHomeEndAction(true));
        list.getActionMap().put("aquaEnd", new AquaHomeEndAction(false));
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    static class AquaHomeEndAction extends AbstractAction {
        private boolean fHomeAction = false;

        protected AquaHomeEndAction(final boolean isHomeAction) {
            fHomeAction = isHomeAction;
        }

        /**
         * For a Home action, scrolls to the top. Otherwise, scroll to the end.
         */
        public void actionPerformed(final ActionEvent e) {
            final JList<?> list = (JList<?>)e.getSource();

            if (fHomeAction) {
                list.ensureIndexIsVisible(0);
            } else {
                final int size = list.getModel().getSize();
                list.ensureIndexIsVisible(size - 1);
            }
        }
    }

    /**
     * This focus listener repaints all of the selected cells, not just the lead cell, since the selected cell
     * background depends upon the list focus state.
     */
    protected class FocusHandler implements FocusListener {

        public void focusGained(FocusEvent event) {
            repaintSelection();
        }

        public void focusLost(FocusEvent event) {
            repaintSelection();
        }
    }

    protected PropertyChangeListener createPropertyChangeListener() {
        return new AquaPropertyChangeHandler();
    }

    class AquaPropertyChangeHandler extends PropertyChangeHandler {
        public void propertyChange(final PropertyChangeEvent e) {
            final String prop = e.getPropertyName();
            if (AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(prop) || "enabled".equals(prop)) {
                repaintSelection();
            } else {
                if (isStyleProperty(prop)) {
                    updateStriped();
                } else if ("layoutOrientation".equals(prop)) {
                    updateStriped();
                }
                super.propertyChange(e);
            }
        }
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, LIST_STYLE_KEY, QUAQUA_LIST_STYLE_KEY);
    }

    protected String getStyleProperty() {
        return AquaUtils.getProperty(list, LIST_STYLE_KEY, QUAQUA_LIST_STYLE_KEY);
    }

    protected void repaintSelection() {
        // All of the selected cells must be repainted when the focus/active/enabled state changes, because the selected
        // cell background depends upon these states.

        AquaFocusHandler.swapSelectionColors("List", list, AquaFocusHandler.hasFocus(list));

        java.util.List cells = list.getSelectedValuesList();
        if (cells.size() > 1) {
            list.repaint();
            return;
        }

        int leadIndex = list.getLeadSelectionIndex();
        if (leadIndex != -1) {
            Rectangle r = getCellBounds(list, leadIndex, leadIndex);
            if (r != null) {
                list.repaint(r.x, r.y, r.width, r.height);
            }
        }
    }

//    // TODO: Using default handler for now, need to handle cmd-key
//
//    // Replace the mouse event with one that returns the cmd-key state when asked
//    // for the control-key state, which super assumes is what everyone does to discontiguously extend selections
//    class MouseInputHandler extends BasicListUI.MouseInputHandler {
//        /*public void mousePressed(final MouseEvent e) {
//            super.mousePressed(new SelectionMouseEvent(e));
//        }
//        public void mouseDragged(final MouseEvent e) {
//            super.mouseDragged(new SelectionMouseEvent(e));
//        }*/
//    }

    @Override
    public void paint(Graphics g, JComponent c) {
        paintStripes(g, c);
        super.paint(g, c);
    }

    @Override
    protected void paintCell(
            Graphics g,
            int row,
            Rectangle rowBounds,
            ListCellRenderer cellRenderer,
            ListModel dataModel,
            ListSelectionModel selModel,
            int leadIndex) {
        Object value = dataModel.getElementAt(row);
        boolean isEnabled = list.isEnabled();
        boolean isFocused = isEnabled && AquaFocusHandler.hasFocus(list);
        boolean cellHasFocus = isFocused && (row == leadIndex);
        boolean isSelected = selModel.isSelectedIndex(row);

        int cx = rowBounds.x;
        int cy = rowBounds.y;
        int cw = rowBounds.width;
        int ch = rowBounds.height;
        if (list.isOpaque() || isSelected) {
            // Paint the background, in case the cell renderer doesn't
            Color c = isSelected ? list.getSelectionBackground() : isStriped ? getAlternateColor(row % 2) : list.getBackground();
            g.setColor(c);
            g.fillRect(cx, cy, cw, ch);
        }

        Component rendererComponent =
                cellRenderer.getListCellRendererComponent(list, value, row, isSelected, cellHasFocus);

        Color bc = getOverrideCellBackground(row, isSelected);
        if (bc != null) {
            rendererComponent.setBackground(bc);
        }

        rendererPane.paintComponent(g, rendererComponent, list, cx, cy, cw, ch, true);
    }

    /**
     * Here we reverse engineer DefaultListCellRenderer to determine when it would install the list background
     * color in the renderer component. If the list is striped, then we intend to override that background color.
     */
    protected Color getOverrideCellBackground(int index, boolean isSelected) {
        if (!isStriped) {
            return null;
        }

        if (isSelected) {
            return null;
        }

        JList.DropLocation dropLocation = list.getDropLocation();
        if (dropLocation != null
                && !dropLocation.isInsert()
                && dropLocation.getIndex() == index) {

            return null;
        }

        return getAlternateColor(index % 2);
    }

    /**
     * Paint stripes where there are no list cells, if appropriate.
     */
    public void paintStripes(Graphics g, JComponent c) {
        if (isStriped && list.getModel() != null) {
            // Now check if we need to paint some stripes
            Dimension vs = c.getSize();
            Dimension ts = list.getSize();

            Point p = list.getLocation();
            int rh = list.getFixedCellHeight();
            int n = list.getModel().getSize();
            if (rh <= 0) {
                rh = (n == 0) ? 12 : getCellBounds(list, 0, 0).height;
            }
            int row = Math.abs(p.y / rh);
            int th = n * rh - row * rh;

            // Fill the background of the list with stripe color 1
            g.setColor(getAlternateColor(1));
            g.fillRect(0, 0, ts.width, ts.height);

            // Fill rectangles with stripe color 0
            g.setColor(getAlternateColor(0));

            // Paint empty rows at the right to fill the viewport
            if (ts.width < vs.width) {
                int y = p.y + row * rh;
                while (y < th) {
                    if (row % 2 == 0) {
                        g.fillRect(0, y, vs.width, rh);
                    }
                    y += rh;
                    row++;
                }
            }

            // Paint empty rows at the bottom to fill the viewport
            if (th < vs.height) {
                row = n;
                int y = th;
                while (y < vs.height) {
                    if (row % 2 == 0) {
                        g.fillRect(0, y, vs.width, rh);
                    }
                    y += rh;
                    row++;
                }
            }
        }
    }

    private Color getAlternateColor(int modulo) {
        return stripes[modulo];
    }

    private void updateStriped() {
        String value = getStyleProperty();
        isStriped = value != null && value.equals("striped") && list.getLayoutOrientation() == JList.VERTICAL;
    }

    // this is used for blinking combobox popup selections when they are selected
    protected void repaintCell(final Object value, final int selectedIndex, final boolean selected) {
        final Rectangle rowBounds = getCellBounds(list, selectedIndex, selectedIndex);
        if (rowBounds == null) return;

        final ListCellRenderer<Object> renderer = list.getCellRenderer();
        if (renderer == null) return;

        final Component rendererComponent = renderer.getListCellRendererComponent(list, value, selectedIndex, selected, true);
        if (rendererComponent == null) return;

        final AquaComboBoxRenderer aquaRenderer = renderer instanceof AquaComboBoxRenderer ? (AquaComboBoxRenderer)renderer : null;
        if (aquaRenderer != null) aquaRenderer.setDrawCheckedItem(false);
        rendererPane.paintComponent(list.getGraphics().create(), rendererComponent, list, rowBounds.x, rowBounds.y, rowBounds.width, rowBounds.height, true);
        if (aquaRenderer != null) aquaRenderer.setDrawCheckedItem(true);
    }

    /*
      Gradient painters not used on Yosemite
    */
    public static Border getSourceListBackgroundPainter() {
        return new GradientPainter(GradientWidget.GRADIENT_SIDE_BAR);
    }

    public static Border getSourceListSelectionBackgroundPainter() {
        return new GradientPainter(GradientWidget.GRADIENT_SIDE_BAR_SELECTION);
    }

    public static Border getSourceListFocusedSelectionBackgroundPainter() {
        // TBD: was GRADIENT_SIDE_BAR_FOCUSED_SELECTION, what is this?
        return new GradientPainter(GradientWidget.GRADIENT_SIDE_BAR_SELECTION);
    }

    public static Border getListEvenBackgroundPainter() {
        return new GradientPainter(GradientWidget.GRADIENT_LIST_BACKGROUND_EVEN);
    }

    public static Border getListOddBackgroundPainter() {
        return new GradientPainter(GradientWidget.GRADIENT_LIST_BACKGROUND_ODD);
    }

    static class GradientPainter extends AquaBorder {
        protected final GradientWidget gw;

        public GradientPainter(GradientWidget gw) {
            this.gw = gw;
        }

        @Override
        public Insets getBorderInsets(Component c) {
            return new Insets(0, 0, 0, 0);
        }

        public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int w, final int h) {
            final JComponent jc = c instanceof JComponent ? (JComponent)c : null;
            State state = jc != null && !AquaFocusHandler.isActive(jc) ? State.ACTIVE : State.INACTIVE;
            painter.configure(w, h);
            GradientConfiguration bg = new GradientConfiguration(gw, state);
            painter.getPainter(bg).paint(g, x, y);
        }
    }
}
