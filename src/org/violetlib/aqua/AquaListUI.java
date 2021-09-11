/*
 * Changes Copyright (c) 2015-2021 Alan Snyder.
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
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicListUI;
import javax.swing.text.JTextComponent;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * A Mac L&F implementation of JList
 */
public class AquaListUI extends BasicListUI implements AquaComponentUI, AquaViewStyleContainerUI {
    public static ComponentUI createUI(JComponent c) {
        return new AquaListUI();
    }

    public static final String LIST_STYLE_KEY = "JList.style";
    public static final String QUAQUA_LIST_STYLE_KEY = "Quaqua.List.style";
    public static final String LIST_VIEW_STYLE_KEY = "JList.viewStyle";

    private boolean isSideBar;
    private boolean isStriped;
    private boolean isInset;
    private boolean isMenu;

    // Vibrant effects for sidebar lists and optionally for menus
    private ListVibrantEffects vibrantEffects;

    protected @NotNull ContainerContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;
    private static final Border insetBorder = new BorderUIResource.EmptyBorderUIResource(5, 0, 5, 0);

    public AquaListUI() {
        colors = AquaColors.CONTAINER_COLORS;
    }

    public void setMenu(boolean b) {
        isMenu = b;
        updateVibrantEffects();
    }

    public void setColors(@NotNull ContainerContextualColors colors) {
        if (colors != this.colors) {
            this.colors = colors;
            configureAppearanceContext(null);
        }
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

        isStriped = getStripedValue();
        isInset = getInsetValue();
        isSideBar = getSideBarValue();
        colors = determineColors();
        updateBorderForInset();
        updateOpaque();
        updateSideBarConfiguration();
        updateVibrantEffects();
        configureAppearanceContext(null);
    }

    @Override
    protected void uninstallDefaults() {
        disposeVibrantEffects();
        removeInsetBorder();
        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        AppearanceManager.installListeners(list);
        AquaUtils.installInsetViewListener(list);
    }

    @Override
    protected void uninstallListeners() {
        AquaUtils.uninstallInsetViewListener(list);
        AppearanceManager.uninstallListeners(list);
        super.uninstallListeners();
    }

    protected void installKeyboardActions() {
        super.installKeyboardActions();
        list.getActionMap().put("aquaHome", new AquaHomeEndAction(true));
        list.getActionMap().put("aquaEnd", new AquaHomeEndAction(false));
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    static class AquaHomeEndAction extends AbstractAction {
        private boolean fHomeAction;

        protected AquaHomeEndAction(boolean isHomeAction) {
            fHomeAction = isHomeAction;
        }

        /**
         * For a Home action, scrolls to the top. Otherwise, scroll to the end.
         */
        public void actionPerformed(ActionEvent e) {
            JList<?> list = (JList<?>)e.getSource();

            if (fHomeAction) {
                list.ensureIndexIsVisible(0);
            } else {
                int size = list.getModel().getSize();
                list.ensureIndexIsVisible(size - 1);
            }
        }
    }

    /**
     * This focus listener repaints all of the selected cells, not just the lead cell, since the selected cell
     * background depends upon the list focus state.
     */
    protected class FocusHandler implements FocusListener {

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

    protected PropertyChangeListener createPropertyChangeListener() {
        return new AquaPropertyChangeHandler();
    }

    class AquaPropertyChangeHandler extends PropertyChangeHandler {
        public void propertyChange(PropertyChangeEvent e) {
            String prop = e.getPropertyName();
            if ("enabled".equals(prop)) {
                configureAppearanceContext(null);
            } else if (AquaFocusHandler.DISPLAY_AS_FOCUSED_KEY.equals(prop)) {
                configureAppearanceContext(null);
            } else {
                if (isStyleProperty(prop)) {
                    updateStyleProperties();
                } else if ("layoutOrientation".equals(prop)) {
                    updateStriped();
                } else if (isViewStyleProperty(prop)) {
                    updateInset();
                } else if ("ancestor".equals(prop)) {
                    updateSideBarConfiguration();
                }
                super.propertyChange(e);
            }
        }
    }

    //    // TODO: Using default handler for now, need to handle cmd-key
    //
    //    // Replace the mouse event with one that returns the cmd-key state when asked
    //    // for the control-key state, which super assumes is what everyone does to discontiguously extend selections
    //    class MouseInputHandler extends BasicListUI.MouseInputHandler {
    //        /*public void mousePressed(MouseEvent e) {
    //            super.mousePressed(new SelectionMouseEvent(e));
    //        }
    //        public void mouseDragged(MouseEvent e) {
    //            super.mouseDragged(new SelectionMouseEvent(e));
    //        }*/
    //    }

    public @NotNull ContainerContextualColors getColors() {
        return colors;
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
            appearance = AppearanceManager.ensureAppearance(list);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        colors.configureForContainer();
        AquaColors.installColors(list, appearanceContext, colors);
        updateOpaque();
        list.repaint();
    }

    protected @NotNull ContainerContextualColors determineColors() {
        if (isSideBar()) {
            return AquaColors.SIDEBAR_CONTAINER_COLORS;
        }
        if (isStriped) {
            return AquaColors.STRIPED_CONTAINER_COLORS;
        }
        return AquaColors.CONTAINER_COLORS;
    }

    protected AquaUIPainter.State getState() {
        return list.isEnabled()
          ? (shouldDisplayAsFocused() ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE)
          : AquaUIPainter.State.DISABLED;
    }

    protected boolean shouldDisplayAsFocused() {
        return AquaFocusHandler.hasFocus(list);
   }

    // Called on a change to isSideBar or the ancestry of the tree
    protected void updateSideBarConfiguration() {
        if (isSideBar()) {
            list.setLayoutOrientation(JList.VERTICAL);
        }
        updateVibrantEffects();
        // On macOS 11+, the sidebar style implies the inset view style.
        if (AquaUtils.isInsetViewSupported()) {
            updateInsetConfiguration();
        }
    }

    protected void updateVibrantEffects() {
        if (list.isDisplayable()) {
            if (isSideBar()) {
                ensureVibrantEffects(AquaVibrantSupport.SIDEBAR_STYLE);
                return;
            }
            if (isMenu && AquaLookAndFeel.USE_VIBRANT_MENU) {
                ensureVibrantEffects(AquaVibrantSupport.MENU_STYLE);
                return;
            }
        }
        disposeVibrantEffects();
    }

    protected void ensureVibrantEffects(int style) {
        JComponent top = getComponentForVisualEffectView();
        if (vibrantEffects != null && (vibrantEffects.getComponent() != top || vibrantEffects.style != style)) {
            disposeVibrantEffects();
        }
        if (vibrantEffects == null) {
            vibrantEffects = new ListVibrantEffects(top, style);
        }
    }

    protected void disposeVibrantEffects() {
        if (vibrantEffects != null) {
            vibrantEffects.dispose();
            vibrantEffects = null;
        }
    }

    protected @NotNull JComponent getComponentForVisualEffectView() {
        Container parent = list.getParent();
        if (parent instanceof JViewport) {
            return (JViewport) parent;
        }
        return list;
    }

    /**
     * Support for vibrant effects. An NSVisualEffectView is created for the list background and for each selected row
     * background region.
     */
    protected class ListVibrantEffects extends VisualEffectView {
        protected @Nullable ListSelectionBoundsTracker bt;
        protected final int style;

        public ListVibrantEffects(@NotNull JComponent top, int style) {
            super(top, style, true);

            this.style = style;
            bt = new ListSelectionBoundsTracker(list, this::updateSelectionBackgrounds) {
                @Override
                protected int convertRowYCoordinateToSelectionDescription(int y) {
                    if (top != list) {
                        Point p = SwingUtilities.convertPoint(list, 0, y, top);
                        return p.y;
                    } else {
                        return y;
                    }
                }
            };
        }

        public void update() {
            if (bt != null) {
                bt.update();
            }
        }

        public void dispose() {
            super.dispose();
            if (bt != null) {
                bt.dispose();
                bt = null;
            }
        }

        @Override
        protected void windowChanged(Window newWindow) {
            super.windowChanged(newWindow);
            if (bt != null) {
                bt.reset();
            }
        }
    }

    private void updateStyleProperties() {
        boolean isStripedChanged = false;
        boolean stripedValue = getStripedValue();
        if (stripedValue != isStriped) {
            isStriped = stripedValue;
            isStripedChanged = true;
        }
        boolean isSideBarChanged = false;
        boolean sideBarValue = getSideBarValue();
        if (sideBarValue != isSideBar) {
            isSideBar = sideBarValue;
            isSideBarChanged = true;
        }
        if (isStripedChanged || isSideBarChanged) {
            colors = determineColors();
            configureAppearanceContext(null);
        }
        if (isSideBarChanged) {
            updateSideBarConfiguration();
        }
    }

    private void updateStriped() {
        boolean value = getStripedValue();
        if (value != isStriped) {
            isStriped = value;
            colors = isStriped ? AquaColors.STRIPED_CONTAINER_COLORS : AquaColors.CONTAINER_COLORS;
            updateOpaque();
            configureAppearanceContext(null);
        }
    }

    private boolean getStripedValue() {
        String value = getStyleProperty();
        return "striped".equals(value)
          && list.getLayoutOrientation() == JList.VERTICAL
          && isBackgroundClear();
    }

    @Override
    public void scrollPaneAncestorChanged(@Nullable JScrollPane sp) {
    }

    private void updateInset() {
        boolean value = getInsetValue();
        if (value != isInset) {
            isInset = value;
            updateInsetConfiguration();
        }
    }

    private void updateInsetConfiguration() {
        updateBorderForInset();
        configureAppearanceContext(null);
        // If the default cell renderer is being used, its insets will be different.
        updateLayoutStateNeeded |= cellRendererChanged;
        list.revalidate();
        list.repaint();
    }

    private void updateBorderForInset() {
        updateBorder(isInset() && !isMenu ? insetBorder : null);
    }

    private void removeInsetBorder() {
        Border b = list.getBorder();
        if (b == insetBorder) {
            list.setBorder(null);
        }
    }

    private boolean getInsetValue() {
        if (AquaUtils.isInsetViewSupported()) {
            String value = getViewStyleProperty();
            return "inset".equals(value);
        }
        return false;
    }

    private boolean getSideBarValue() {
        String style = getStyleProperty();
        return style != null && (style.equals("sideBar") || style.equals("sourceList"));
    }

    private void updateBorder(@Nullable Border b) {
        Border existing = list.getBorder();
        if (existing != b && existing == null || existing instanceof UIResource) {
            list.setBorder(b);
        }
    }

    private void updateOpaque() {
        // JList forces opaque to be true, so LookAndFeel.installProperty cannot be used
        Color background = list.getBackground();
        list.setOpaque(!isStriped && (isVibrant() || background == null || background.getAlpha() == 255));
    }

    private boolean isBackgroundClear() {
        Color c = list.getBackground();
        return c == null || c.getAlpha() == 0 || c instanceof ColorUIResource;
    }

    public boolean isStriped() {
        return isStriped;
    }

    public boolean isSideBar() {
        return isSideBar;
    }

    public boolean isVibrant() {
        return isSideBar || (AquaLookAndFeel.USE_VIBRANT_MENU && isMenu);
    }

    @Override
    public boolean isInset() {
        return isInset || (isVibrant() && OSXSystemProperties.useInsetViewStyle());
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, LIST_STYLE_KEY, QUAQUA_LIST_STYLE_KEY);
    }

    protected @Nullable String getStyleProperty() {
        return AquaUtils.getProperty(list, LIST_STYLE_KEY, QUAQUA_LIST_STYLE_KEY);
    }

    protected boolean isViewStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, LIST_VIEW_STYLE_KEY);
    }

    protected @Nullable String getViewStyleProperty() {
        return AquaUtils.getProperty(list, LIST_VIEW_STYLE_KEY);
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        if (list.isOpaque()) {
            Color background = getBackgroundColor();
            int width = list.getWidth();
            int height = list.getHeight();
            AquaUtils.fillRect(g, background, 0, 0, width, height);
        }
        paint(g, c);
    }

    private @Nullable Color getBackgroundColor() {
        return isVibrant() ? null : list.getBackground();
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        if (appearanceContext != null) {
            if (vibrantEffects != null) {
                vibrantEffects.update();
            } else {
                paintStripes(g);
            }
            super.paint(g, c);
        }
    }

    /**
     * Paint stripes, if appropriate.
     */
    public void paintStripes(Graphics g) {

        if (isStriped && list.getModel() != null) {
            Graphics2D gg = (Graphics2D) g;

            assert appearanceContext != null;

            Dimension vs = list.getSize();
            Insets s = list.getInsets();
            int rh = list.getFixedCellHeight();
            int n = list.getModel().getSize();
            if (rh <= 0) {
                rh = (n == 0) ? 12 : getCellBounds(list, 0, 0).height;
            }
            int visibleRowCount = (int) Math.ceil(Math.abs(vs.getHeight() / rh));
            int row = 0;
            int y = s.top;
            ListSelectionModel selectionModel = list.getSelectionModel();

            while (row < visibleRowCount) {
                boolean isSelected = selectionModel.isSelectedIndex(row);
                colors.configureForRow(row, isSelected && !isInset());
                Color background = colors.getBackground(appearanceContext);
                g.setColor(background);
                if (isInset()) {
                    AquaUtils.paintInsetStripedRow(gg, 0, y, vs.width, rh);
                } else {
                    g.fillRect(0, y, vs.width, rh);
                }
                row++;
                y += rh;
            }
        }
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

        int cx = rowBounds.x;
        int cy = rowBounds.y;
        int cw = rowBounds.width;
        int ch = rowBounds.height;

        assert appearanceContext != null;
        boolean isSelected = selModel.isSelectedIndex(row);
        boolean isFocused = shouldDisplayAsFocused();
        boolean cellHasFocus = isFocused && (row == leadIndex);
        boolean isWrapped = list.getLayoutOrientation() != JList.VERTICAL;

        colors.configureForRow(row, isSelected);
        if (isSelected && !isVibrant()) {
            Color background = colors.getBackground(appearanceContext);
            g.setColor(background);
            if (isInset()) {
                Graphics2D gg = (Graphics2D) g;
                if (isMenu) {
                    AquaUtils.paintInsetMenuItemSelection(gg, cx, cy, cw, ch);
                } else if (isWrapped) {
                    AquaUtils.paintInsetCellSelection(gg, cx, cy, cw, ch);
                } else {
                    boolean isSelectedAbove = row > 0 && selModel.isSelectedIndex(row-1);
                    boolean isSelectedBelow = row < list.getModel().getSize()-1 && selModel.isSelectedIndex(row+1);
                    AquaUtils.paintInsetCellSelection(gg, isSelectedAbove, isSelectedBelow, cx, cy, cw, ch);
                }
            } else if (!isStriped) {
                g.fillRect(cx, cy, cw, ch);
            }
        }

        Object value = dataModel.getElementAt(row);
        Component rendererComponent =
          cellRenderer.getListCellRendererComponent(list, value, row, isSelected, cellHasFocus);

        if (rendererComponent instanceof JTextComponent) {
            ((JTextComponent) rendererComponent).putClientProperty(AquaColors.COMPONENT_COLORS_KEY, AquaColors.CELL_TEXT_COLORS);
        }

        if (isInset() && rendererComponent.isOpaque() && rendererComponent instanceof JComponent) {
            JComponent jc = (JComponent) rendererComponent;
            AquaUtils.setOpaqueCarefully(jc, false);
        }

        rendererPane.paintComponent(g, rendererComponent, list, cx, cy, cw, ch, true);

        if (rendererComponent instanceof JTextComponent) {
            ((JTextComponent) rendererComponent).putClientProperty(AquaColors.COMPONENT_COLORS_KEY, null);
        }
    }

    // this is used for blinking combobox popup selections when they are selected
//    protected void repaintCell(Object value, int selectedIndex, boolean selected) {
//        Rectangle rowBounds = getCellBounds(list, selectedIndex, selectedIndex);
//        if (rowBounds == null) {
//            return;
//        }
//
//        ListCellRenderer<Object> renderer = list.getCellRenderer();
//        if (renderer == null) {
//            return;
//        }
//
//        Component rendererComponent = renderer.getListCellRendererComponent(list, value, selectedIndex, selected, true);
//        if (rendererComponent == null) {
//            return;
//        }
//
//        AquaComboBoxRenderer aquaRenderer = renderer instanceof AquaComboBoxRenderer ? (AquaComboBoxRenderer)renderer : null;
//        if (aquaRenderer != null) {
//            aquaRenderer.setDrawCheckedItem(false);
//        }
//        rendererPane.paintComponent(list.getGraphics().create(), rendererComponent, list, rowBounds.x, rowBounds.y, rowBounds.width, rowBounds.height, true);
//        if (aquaRenderer != null) {
//            aquaRenderer.setDrawCheckedItem(true);
//        }
//    }
}
