/*
 * Changes Copyright (c) 2015-2026 Alan Snyder.
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
import java.awt.dnd.*;
import java.awt.event.*;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.TooManyListenersException;
import javax.swing.*;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicListUI;
import javax.swing.text.JTextComponent;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static org.violetlib.aqua.OSXSystemProperties.OSVersion;
import static org.violetlib.jnr.aqua.AquaUIPainter.State.*;

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
    private boolean isVibrantMenu;
    private boolean isRoundedScrollable;

    private boolean hasSelection;
    private boolean isDropActive;
    private boolean isSelectionMuted;
    private boolean hasDropOnTarget;

    // Vibrant effects for sidebar lists (prior to macOS 26) and optionally for menus
    private @Nullable ListVibrantEffects vibrantEffects;
    private @Nullable LocalSidebarContainerSupport sidebarContainerSupport;
    protected @NotNull ContainerContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;
    private static final Insets insetViewInsets = new Insets(0, 0, 0, 0);  // sides only
    private final HierarchyListener hierarchyListener = new AquaListHierarchyListener();
    private DropTargetListener dropTargetListener;
    private DropTarget knownDropTarget;

    public AquaListUI() {
        colors = AquaColors.CONTAINER_COLORS;
    }

    public void configureAsMenu(boolean isVibrant) {
        isMenu = true;
        isVibrantMenu = isVibrant;
        updateVibrantEffects();
    }

    public void setColors(@NotNull ContainerContextualColors colors) {
        if (colors != this.colors) {
            this.colors = colors;
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
        sidebarContainerSupport = new LocalSidebarContainerSupport(list, this);
        isRoundedScrollable = AquaUtils.isRoundedScrollable(list);
        isStriped = getStripedValue();
        isInset = getInsetValue();
        isSideBar = getSideBarValue();
        colors = determineColors();
        sidebarContainerSupport.update();
        updateVibrantEffects();
    }

    @Override
    protected void uninstallDefaults() {
        disposeVibrantEffects();
        if (sidebarContainerSupport != null) {
            sidebarContainerSupport.detach();
            sidebarContainerSupport = null;
        }
        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        AppearanceManager.install(list);
        AquaUtils.installInsetViewListener(list);
        list.addHierarchyListener(hierarchyListener);
    }

    @Override
    protected void uninstallListeners() {
        if (knownDropTarget != null) {
            knownDropTarget.removeDropTargetListener(dropTargetListener);
            knownDropTarget = null;
        }
        list.removeHierarchyListener(hierarchyListener);
        AquaUtils.uninstallInsetViewListener(list);
        AppearanceManager.uninstall(list);
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
     * This focus listener repaints all selected cells, not just the lead cell, since the selected cell
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
            list.repaint();
        }
    }

    class AquaListHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            if ((e.getChangeFlags() & HierarchyEvent.DISPLAYABILITY_CHANGED) != 0) {
                updateSideBarConfiguration();
            }
        }
    }

    protected class MyDropTargetListener implements DropTargetListener
    {
        @Override
        public void dragEnter(DropTargetDragEvent dtde) {
            isDropActive = true;
            repaintDropTarget();
        }

        @Override
        public void dragOver(DropTargetDragEvent dtde) {
        }

        @Override
        public void dropActionChanged(DropTargetDragEvent dtde) {
        }

        @Override
        public void dragExit(DropTargetEvent dte) {
            isDropActive = false;
            repaintDropTarget();
        }

        @Override
        public void drop(DropTargetDropEvent dtde) {
            isDropActive = false;
            repaintDropTarget();
        }
    }

    protected void repaintDropTarget() {
        SwingUtilities.invokeLater(() -> list.repaint());
    }

    protected PropertyChangeListener createPropertyChangeListener() {
        return new AquaPropertyChangeHandler();
    }

    class AquaPropertyChangeHandler extends PropertyChangeHandler {
        public void propertyChange(PropertyChangeEvent e) {
            String prop = e.getPropertyName();
            if ("enabled".equals(prop)) {
                list.repaint();
            } else if (AquaFocusHandler.DISPLAY_AS_FOCUSED_KEY.equals(prop)) {
                list.repaint();
            } else {
                if (isStyleProperty(prop)) {
                    updateStyleProperties();
                } else if ("layoutOrientation".equals(prop)) {
                    updateStriped();
                } else if (isViewStyleProperty(prop)) {
                    updateInset();
                } else if ("ancestor".equals(prop)) {
                    updateSideBarConfiguration();
                } else if ("transferHandler".equals(prop)) {
                    configureDropTargetListener();
                } else if ("dropLocation".equals(prop)) {
                    if (hasSelection) {
                        list.repaint();
                    }
                    return;
                }
                super.propertyChange(e);
            }
        }
    }

    private void configureDropTargetListener() {
        if (knownDropTarget != null) {
            knownDropTarget.removeDropTargetListener(dropTargetListener);
            knownDropTarget = null;
        }

        if (dropTargetListener == null) {
            dropTargetListener = new MyDropTargetListener();
        }

        knownDropTarget = list.getDropTarget();
        if (knownDropTarget != null) {
            try {
                knownDropTarget.addDropTargetListener(dropTargetListener);
            } catch (TooManyListenersException ignore) {
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

    private void updateOpaque() {
        // In some versions of the JDK, JList forces opaque to be true, so LookAndFeel.installProperty cannot be used
        Color background = list.getBackground();
        if (background == null) {
            list.setBackground(background = AquaColors.CLEAR);
        }
        list.setOpaque(background.getAlpha() == 255);
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
        // Not sure which OS release made this change:
        if (!AquaFocusHandler.isActive(list) && OSVersion >= 1500) {
            return INACTIVE;
        }
        return list.isEnabled() ? (shouldDisplayAsFocused() ? ACTIVE_DEFAULT : ACTIVE) : DISABLED;
    }

    public boolean shouldSuppressBackground()
    {
        return isStriped() || isVibrant();
    }

    public boolean shouldSuppressSelectionBackground()
    {
        return isInset() || isVibrant();
    }

    protected boolean shouldDisplayAsFocused() {
        return AquaFocusHandler.hasFocus(list);
    }

    // Called on a change to the style or a possibly relevant change to ancestry of the list
    protected void updateSideBarConfiguration()
    {
        if (sidebarContainerSupport != null) {
            sidebarContainerSupport.update();
        }
    }

    @Override
    public void configureSidebarStyle()
    {
        if (isSideBar()) {
            list.setLayoutOrientation(JList.VERTICAL);
            updateVibrantEffects();
            // On macOS 11+, the sidebar style implies the inset view style.
            if (AquaUtils.isInsetViewSupported()) {
                updateInsetConfiguration();
            }
        } else {
            updateVibrantEffects();
            if (AquaUtils.isInsetViewSupported()) {
                updateInsetConfiguration();
            }
        }
    }

    protected void updateVibrantEffects() {
        if (list.isDisplayable()) {
            if (isSideBar() && AquaPainting.isSidebarVibrant(list)) {
                ensureVibrantEffects(AquaVibrantSupport.SIDEBAR_STYLE);
                return;
            }
            if (isVibrantMenu && AquaLookAndFeel.ENABLE_VIBRANT_MENU) {
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
        JComponent c = AquaUtils.getScrollPaneContainer(list);
        if (c != null) {
            return c;
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
        }
    }

    private boolean getStripedValue() {
        String value = getStyleProperty();
        return "striped".equals(value)
          && list.getLayoutOrientation() == JList.VERTICAL
          && isBackgroundOKForStriped();
    }

    @Override
    public void scrollPaneAncestorChanged(@Nullable JScrollPane sp)
    {
        updateSideBarConfiguration();
    }

    @Override
    public void scrollPaneRoundedBorderStatusChanged(boolean isRoundedBorder) {
        isRoundedScrollable = isRoundedBorder;
        updateInset();
    }

    private void updateInset() {
        boolean value = getInsetValue();
        if (value != isInset) {
            isInset = value;
            updateInsetConfiguration();
        }
    }

    private void updateInsetConfiguration() {
        // If the default cell renderer is being used, its insets will be different.
        updateLayoutStateNeeded |= cellRendererChanged;
        list.revalidate();
        list.repaint();
    }

    private boolean getInsetValue() {
        if (AquaUtils.isInsetViewSupported()) {
            String value = getViewStyleProperty();
            if ("inset".equals(value)) {
                return true;
            }
            if (isRoundedScrollable) {
                return true;
            }
        }
        return false;
    }

    private boolean getSideBarValue() {
        String style = getStyleProperty();
        return style != null && (style.equals("sideBar") || style.equals("sourceList"));
    }

    public @NotNull Insets getInsets() {
        if (isMenu) {
            return new Insets(0, 0, 0, 0);
        }
        boolean isInset = isInset();
        boolean isSideBar = isSideBar();
        boolean isGlass = isSideBar && AquaPainting.useLiquidGlassSidebar();
        int v = isInset ? isRoundedScrollable ? 10 : 5 : 0;
        int side = isGlass || isInset ? 5 : 0;
        return new Insets(v, side, v, side);
    }

    private boolean isBackgroundOKForStriped() {
        Color c = list.getBackground();
        return c == null || c.getAlpha() == 0 || c instanceof ColorUIResource;
    }

    public boolean isStriped() {
        return isStriped;
    }

    @Override
    public boolean isSideBar() {
        return isSideBar;
    }

    public boolean isVibrant() {
        return vibrantEffects != null;
    }

    @Override
    public boolean isInset() {
        return isInset || (OSXSystemProperties.useInsetViewStyle() && (isVibrant() || isSideBar()));
    }

    @Override
    public @NotNull Insets getInsetViewInsets() {
        return getInsets();
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
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceManager.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {

        updateVibrantEffects();

        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(pc.appearance, state, false, false);
        colors.configureForContainer();
        AquaColors.installColors(list, appearanceContext, colors);
        updateOpaque();

        hasSelection = !list.getSelectionModel().isSelectionEmpty();
        isSelectionMuted = false;
        hasDropOnTarget = false;

        JList.DropLocation loc = null;

        if (isDropActive) {
            loc = list.getDropLocation();
            if (loc != null) {
                // Drop target highlighting uses the accent color.
                // To avoid confusion, selections should not also use the accent color.
                hasDropOnTarget = !loc.isInsert();
                isSelectionMuted = hasSelection;
            }
        }

        if (sidebarContainerSupport != null) {
            g = sidebarContainerSupport.setupContainerGraphics(g, appearanceContext);
        } else {
            g = (Graphics2D) g.create();
        }

        if (vibrantEffects != null) {
            eraseBackground(g);
            vibrantEffects.update();
        } else {
            paintBackground(g);
            paintStripes(g, appearanceContext);
        }

        SafeGraphics sg = new SafeGraphics(g);
        super.paint(sg, c);

        if (loc != null && loc.isInsert()) {
            Color color = appearanceContext.getAppearance().getColor("controlAccent");
            if (color == null) {
                color = appearanceContext.getAppearance().isDark() ? Color.WHITE : Color.BLACK;
            }
            paintDropLine(g, loc, color);
        }
        g.dispose();
    }

    private void paintBackground(@NotNull Graphics g) {
        Color background = getBackgroundColor();
        if (background.getAlpha() > 0) {
            int width = list.getWidth();
            int height = list.getHeight();
            AquaUtils.fillRect(g, background, 0, 0, width, height);
        }
    }

    private void eraseBackground(@NotNull Graphics g) {
        int width = list.getWidth();
        int height = list.getHeight();
        AquaUtils.erase(g, 0, 0, width, height);
    }

    private @NotNull Color getBackgroundColor() {
        Color c = list.getBackground();
        return c != null ? c : AquaColors.CLEAR;
    }

    /**
     * A delegated graphics context that prevents BasicListUI from painting outside the bounds of the original clip
     * region.
     */

    private static class SafeGraphics
      extends DelegatedGraphicsBase
    {
        private final @NotNull Shape originalClip;

        public SafeGraphics(@NotNull Graphics g) {
            super(g);
            originalClip = g.getClip();
        }

        @Override
        public @NotNull Graphics create() {
            return new SafeGraphics(delegate.create());
        }

        @Override
        public @NotNull Graphics create(int x, int y, int width, int height) {
            return new SafeGraphics(delegate.create(x, y, width, height));
        }

        @Override
        public void setClip(int x, int y, int width, int height) {
            delegate.setClip(originalClip);
            delegate.clipRect(x, y, width, height);
        }

        @Override
        public void setClip(@NotNull Shape clip) {
            delegate.setClip(originalClip);
            delegate.clip(clip);
        }
    }

    /**
     * Paint stripes, if appropriate.
     */
    public void paintStripes(@NotNull Graphics2D g, @NotNull AppearanceContext appearanceContext) {

        if (isStriped && list.getModel() != null) {
            Dimension vs = list.getSize();
            Insets s = list.getInsets();
            int rh = list.getFixedCellHeight();
            int n = list.getModel().getSize();
            if (rh <= 0) {
                rh = (n == 0) ? 17 : getCellBounds(list, 0, 0).height;
            }
            int maximumVisibleRowCount = (int) Math.ceil(vs.getHeight() / rh);
            int row = 0;
            int y = s.top;
            ListSelectionModel selectionModel = list.getSelectionModel();

            while (row < maximumVisibleRowCount) {
                boolean isSelected = row < n && selectionModel.isSelectedIndex(row);
                colors.configureForRow(row, isSelected && !isInset());
                Color background = colors.getBackground(appearanceContext);
                g.setColor(background);
                if (isInset()) {
                    if (row % 2 == 1) {
                        AquaUtils.paintInsetStripedRow(g, 0, y, vs.width, rh);
                    }
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

        boolean isSelected = selModel.isSelectedIndex(row);
        boolean isFocused = shouldDisplayAsFocused();
        boolean cellHasFocus = isFocused && (row == leadIndex);
        boolean isWrapped = list.getLayoutOrientation() != JList.VERTICAL;
        boolean isDropTarget = false;

        assert appearanceContext != null;
        AppearanceContext ac = appearanceContext;
        // When an accent color drop target is displayed, selected rows should not also use an accent color background.
        if (isSelected && isSelectionMuted) {
            ac = ac.withState(ACTIVE);
        }

        if (isDropActive) {
            cellHasFocus = false;
            if (hasDropOnTarget) {
                JList.DropLocation loc = list.getDropLocation();
                if (loc != null && loc.getIndex() == row) {
                    isDropTarget = true;
                    ac = ac.withState(ACTIVE_DEFAULT);
                }
            }
        }

        colors.configureForRow(row, isSelected || isDropTarget);
        if ((isSelected || isDropTarget) && vibrantEffects == null) {
            if (isDropTarget) {
                ac = ac.withSelected(true);
            }
            Color background = colors.getBackground(ac);
            g.setColor(background);
            if (!AquaColors.isPriority(list.getSelectionBackground())) {
                Color c = isInset() ? AquaColors.CLEAR : background;
                list.setSelectionBackground(c);
            }

            if (isInset()) {
                Graphics2D gg = (Graphics2D) g;
                if (isMenu) {
                    AquaUtils.paintInsetMenuItemSelection(gg, cx, cy, cw, ch);
                } else if (isWrapped) {
                    AquaUtils.paintInsetCellSelection(gg, 0, cy, list.getWidth(), ch);
                } else {
                    boolean isSelectedAbove
                      = row > 0 && selModel.isSelectedIndex(row-1) && !hasDropOnTarget;
                    boolean isSelectedBelow
                      = row < list.getModel().getSize()-1 && selModel.isSelectedIndex(row+1) && !hasDropOnTarget;
                    AquaUtils.paintInsetCellSelection(gg, isSelectedAbove, isSelectedBelow, 0, cy, list.getWidth(), ch);
                }
            } else if (!isStriped) {
                g.fillRect(cx, cy, cw, ch);
            }
        }

        if (isDropTarget && !isInset) {
            isSelected = true;
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

    // The following code, adapted from BasicListUI, supports drop insert highlighting.

    private static final int DROP_LINE_THICKNESS = 2;
    private int layoutOrientation;
    private boolean isLeftToRight;

    private void paintDropLine(Graphics g, @NotNull JList.DropLocation loc, @NotNull Color dropLineColor)
    {
        layoutOrientation = list.getLayoutOrientation();
        isLeftToRight = list.getComponentOrientation().isLeftToRight();

        g.setColor(dropLineColor);
        Rectangle rect = getDropLineRect(loc);
        g.fillRect(rect.x, rect.y, rect.width, rect.height);
    }

    private Rectangle getDropLineRect(JList.DropLocation loc)
    {
        int size = list.getModel().getSize();

        if (size == 0) {
            Insets insets = list.getInsets();
            if (layoutOrientation == JList.HORIZONTAL_WRAP) {
                if (isLeftToRight) {
                    return new Rectangle(insets.left, insets.top, DROP_LINE_THICKNESS, 20);
                } else {
                    return new Rectangle(list.getWidth() - DROP_LINE_THICKNESS - insets.right,
                      insets.top, DROP_LINE_THICKNESS, 20);
                }
            } else {
                return new Rectangle(insets.left, insets.top,
                  list.getWidth() - insets.left - insets.right,
                  DROP_LINE_THICKNESS);
            }
        }

        Rectangle rect;
        int index = loc.getIndex();
        boolean decr = false;

        if (layoutOrientation == JList.HORIZONTAL_WRAP) {
            if (index == size) {
                decr = true;
            } else if (index != 0) {
                Rectangle prev = getCellBounds(index - 1);
                Rectangle me = getCellBounds(index);
                if (prev.y != me.y) {
                    Point p = loc.getDropPoint();
                    if (isLeftToRight) {
                        decr = Point2D.distance(prev.x + prev.width, prev.y + (int)(prev.height / 2.0), p.x, p.y)
                          < Point2D.distance(me.x, me.y + (int)(me.height / 2.0), p.x, p.y);
                    } else {
                        decr = Point2D.distance(prev.x,
                          prev.y + (int)(prev.height / 2.0),
                          p.x, p.y)
                          < Point2D.distance(me.x + me.width,
                          me.y + (int)(prev.height / 2.0),
                          p.x, p.y);
                    }
                }
            }

            if (decr) {
                index--;
                rect = getCellBounds(index);
                if (isLeftToRight) {
                    rect.x += rect.width;
                } else {
                    rect.x -= DROP_LINE_THICKNESS;
                }
            } else {
                rect = getCellBounds(index);
                if (!isLeftToRight) {
                    rect.x += rect.width - DROP_LINE_THICKNESS;
                }
            }

            if (rect.x >= list.getWidth()) {
                rect.x = list.getWidth() - DROP_LINE_THICKNESS;
            } else if (rect.x < 0) {
                rect.x = 0;
            }

            rect.width = DROP_LINE_THICKNESS;
        } else if (layoutOrientation == JList.VERTICAL_WRAP) {
            if (index == size) {
                index--;
                rect = getCellBounds(index);
                rect.y += rect.height;
            } else if (index != 0) {
                Rectangle prev = getCellBounds(index - 1);
                Rectangle me = getCellBounds(index);
                if (prev.x != me.x) {
                    Point p = loc.getDropPoint();
                    if (Point2D.distance(prev.x + (int)(prev.width / 2.0), prev.y + prev.height, p.x, p.y)
                      < Point2D.distance(me.x + (int)(me.width / 2.0), me.y, p.x, p.y)) {
                        index--;
                        rect = getCellBounds(index);
                        rect.y += rect.height;
                    } else {
                        rect = getCellBounds(index);
                    }
                } else {
                    rect = getCellBounds(index);
                }
            } else {
                rect = getCellBounds(index);
            }

            if (rect.y >= list.getHeight()) {
                rect.y = list.getHeight() - DROP_LINE_THICKNESS;
            }

            rect.height = DROP_LINE_THICKNESS;
        } else {
            if (index == size) {
                index--;
                rect = getCellBounds(index);
                rect.y += rect.height;
            } else {
                rect = getCellBounds(index);
            }

            if (rect.y >= list.getHeight()) {
                rect.y = list.getHeight() - DROP_LINE_THICKNESS;
            }

            rect.height = DROP_LINE_THICKNESS;
        }

        return rect;
    }

    private Rectangle getCellBounds(int index)
    {
        return getCellBounds(list, index, index);
    }
}
