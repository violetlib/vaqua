/*
 * Changes Copyright (c) 2015-2025 Alan Snyder.
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
import java.awt.geom.AffineTransform;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.View;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.*;
import org.violetlib.jnr.aqua.SegmentedButtonConfiguration;
import org.violetlib.jnr.aqua.SegmentedButtonLayoutConfiguration;

import static org.violetlib.aqua.OSXSystemProperties.OSVersion;
import static org.violetlib.jnr.aqua.AquaUIPainter.Position.*;
import static org.violetlib.jnr.aqua.SegmentedButtonConfiguration.DividerState;

public class AquaTabbedPaneUI extends AquaTabbedPaneCopyFromBasicUI
  implements AquaUtilControlSize.Sizeable, FocusRingOutlineProvider, AquaComponentUI,
  SystemPropertyChangeManager.SystemPropertyChangeListener {

    public static ComponentUI createUI(JComponent c) {
        return new AquaTabbedPaneUI();
    }

    public static final @NotNull String CONTENT_BACKGROUND_KEY = "JTabbedPane.contentBackground";

    private static final double kNinetyDegrees = (Math.PI / 2.0); // used for rotation

    public static final SegmentedButtonWidget buttonWidget
      = OSVersion >= 1016 ? SegmentedButtonWidget.BUTTON_SEGMENTED_SLIDER : SegmentedButtonWidget.BUTTON_TAB;

    protected final Insets currentContentDrawingInsets = new Insets(0, 0, 0, 0);
    protected final Insets currentContentBorderInsets = new Insets(0, 0, 0, 0);
    protected final Insets contentDrawingInsets = new Insets(0, 0, 0, 0);

    // identify the (visible) left or right scroll arrow tab (viewed horizontally with labels right side up)
    protected static final int LEFT_SCROLL_TAB = -2;
    protected static final int RIGHT_SCROLL_TAB = -1;

    // a return value indicating no tab
    protected static final int NO_TAB = -3;

    protected int pressedTab = NO_TAB;
    protected boolean scrollPositionChangeNeeded;

    protected Boolean isDefaultFocusReceiver = null;
    protected boolean hasAvoidedFirstFocus = false;

    protected final AquaTabbedPaneTabState visibleTabState = new AquaTabbedPaneTabState(this);
    protected final AquaUIPainter painter = AquaPainting.create();

    protected Size sizeVariant = Size.REGULAR;
    protected int fixedTabHeight = 0;
    protected int maxIconSize = 0;
    protected Insets onlyTabInsets;
    protected Insets leftTabInsets;
    protected Insets rightTabInsets;
    protected Insets middleTabInsets;
    protected boolean isReversed;
    protected boolean isDark;

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    protected AquaTabbedPaneModel paneModel;

    public AquaTabbedPaneUI() {
        colors = AquaColors.TAB_COLORS;
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();

        if (tabPane.getFont() instanceof UIResource) {
            Boolean b = (Boolean)UIManager.get("TabbedPane.useSmallLayout");
            if (b != null && b == Boolean.TRUE) {
                tabPane.setFont(UIManager.getFont("TabbedPane.smallFont"));
                sizeVariant = Size.SMALL;
            }
        }

        updateOrientation();
        updateLayoutParameters();
        contentDrawingInsets.set(0, 10, 10, 10);
        LookAndFeel.installProperty(tabPane, "opaque", false);
        configureAppearanceContext(null, tabPane);
        configureFocusable(tabPane);
    }

    @Override
    protected void uninstallDefaults() {
        contentDrawingInsets.set(0, 0, 0, 0);
    }

    @Override
    protected void installListeners() {
        super.installListeners();

        // We're not just a mouseListener, we're a mouseMotionListener
        if (mouseListener != null) {
            tabPane.addMouseMotionListener((MouseMotionListener) mouseListener);
        }

        AquaUtilControlSize.addSizePropertyListener(tabPane);
        OSXSystemProperties.register(tabPane);
        AppearanceManager.installListeners(tabPane);
    }

    @Override
    protected void uninstallListeners() {

        if (mouseListener != null) {
            tabPane.removeMouseMotionListener((MouseMotionListener) mouseListener);
        }

        AppearanceManager.uninstallListeners(tabPane);
        AquaUtilControlSize.removeSizePropertyListener(tabPane);
        OSXSystemProperties.unregister(tabPane);

        super.uninstallListeners();
    }

    protected MouseListener createMouseListener() {
        return new MouseHandler();
    }

    protected FocusListener createFocusListener() {
        return new FocusHandler();
    }

    protected PropertyChangeListener createPropertyChangeListener() {
        return new TabbedPanePropertyChangeHandler();
    }

    protected LayoutManager createLayoutManager() {
        return new AquaTruncatingTabbedPaneLayout();
    }

    protected boolean shouldRepaintSelectedTabOnMouseDown() {
        return false;
    }

    @Override
    public void systemPropertyChanged(JComponent c, Object type) {
        if (type.equals(OSXSystemProperties.USER_PREFERENCE_CHANGE_TYPE)) {
            configureFocusable(c);
        }
    }

    private void configureFocusable(JComponent c) {
        boolean isFocusable = OSXSystemProperties.isFullKeyboardAccessEnabled();
        c.setFocusable(isFocusable);
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance, (JTabbedPane)c);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        configureAppearanceContext(null, (JTabbedPane)c);
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance, @NotNull JTabbedPane s) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(s);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        isDark = appearance.isDark();
        AquaColors.installColors(s, appearanceContext, colors);
        s.repaint();
    }

    @Override
    public void applySizeFor(JComponent c, Size size, boolean isDefaultSize) {
        if (size != sizeVariant) {
            sizeVariant = size;
            AquaUtilControlSize.configureFontFromSize(c, size);
            updateLayoutParameters();
            c.revalidate();
            c.repaint();
        }
    }

    protected void updateLayoutParameters() {
        SegmentedButtonLayoutConfiguration g = new SegmentedButtonLayoutConfiguration(buttonWidget, sizeVariant, FIRST);
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
        fixedTabHeight = (int) layoutInfo.getFixedVisualHeight();

        // The renderer does not know about right to left orientation, because the rendering is symmetric.
        // Therefore, the First position is always on the left side.

        onlyTabInsets = getTabInsets(ONLY);
        leftTabInsets = getTabInsets(FIRST);
        rightTabInsets = getTabInsets(LAST);
        middleTabInsets = getTabInsets(MIDDLE);

        // Icon size is less than text height because having icons touch the border is uglier...
        int delta = onlyTabInsets.top + onlyTabInsets.bottom + 1;
        if (delta % 2 == 1) {
            delta++;
        }
        maxIconSize = fixedTabHeight - delta;

        invalidatePaneModel();  // layout uses insets based on tab position
    }

    protected Insets getTabInsets(Position pos) {
        SegmentedButtonLayoutConfiguration g = new SegmentedButtonLayoutConfiguration(buttonWidget, sizeVariant, pos);
        Insetter s = painter.getLayoutInfo().getSegmentedButtonLabelInsets(g);
        Insets n = s.asInsets();
        if (n == null) {
            n = new Insets(3, 0, 3, 0);
        }
        AquaButtonExtendedTypes.WidgetInfo info = AquaButtonExtendedTypes.getTabWidgetInfo(buttonWidget, sizeVariant, pos);
        int margin = info.getDefaultSideMargin();
        return new Insets(n.top, n.left + margin, n.bottom, n.right + margin);
    }

    protected void assureRectsCreated(int tabCount) {
        visibleTabState.init(tabCount);
        super.assureRectsCreated(tabCount);
    }

    @Override
    protected void layoutCompleted() {
        invalidatePaneModel();
    }

    @Override
    protected void layoutInvalidated() {
        visibleTabState.invalidateLayout();
        invalidatePaneModel();
    }

    @Override
    protected void tabsChanged() {
        visibleTabState.invalidateTabs();
        invalidatePaneModel();
    }

    private @NotNull AquaTabbedPaneModel getPaneModel() {
        if (paneModel == null) {
            createPaneModel();
        }
        return paneModel;
    }

    private void invalidatePaneModel() {
        paneModel = null;
    }

    private void createPaneModel() {

        // debug
        System.err.println("Creating tabbed pane model for " + tabPane.getTabCount() + " tabs");

        int tabCount = tabPane.getTabCount();
        int placement = tabPane.getTabPlacement();
        boolean isVertical = placement == SwingConstants.LEFT || placement == SwingConstants.RIGHT;
        int visibleTabCount = visibleTabState.getTotal();
        int firstVisibleTab = visibleTabCount > 0 ? visibleTabState.getIndex(0) : 0;

        if (visibleTabCount == 0) {
            // Most likely, a new layout is being created (and the old visible tab state was invalidated).
            // For layout purposes, it is reasonable to assume that all tabs are visible.
            visibleTabCount = tabCount;
        }

        int selectedIndex = tabPane.getSelectedIndex();
        paneModel = new AquaTabbedPaneModel(isVertical, isReversed, placement, tabCount, visibleTabCount, firstVisibleTab, selectedIndex);
    }

    // Paint Methods
    // Cache for performance
    private Rectangle fContentRect = new Rectangle();
    private Rectangle fIconRect = new Rectangle();
    private Rectangle fTextRect = new Rectangle();

    // UI Rendering

    @Override
    public void update(@NotNull Graphics g, @NotNull JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        if (c.isOpaque()) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    public void paint(@NotNull Graphics g, @NotNull JComponent c) {

        ensureCurrentLayout();

        int tabPlacement = tabPane.getTabPlacement();
        int selectedIndex = tabPane.getSelectedIndex();
        paintContentBorder(g, tabPlacement, selectedIndex);

        Rectangle clipRect = g.getClipBounds();

        if (DEBUG_CUTOUT && !isDark) {
            g = g.create();
            ((Graphics2D) g).setComposite(AlphaComposite.SrcOver.derive(0.2f));
        }

        if (visibleTabState.needsScrollTabs()) {
            paintScrollingTabs(g, clipRect, tabPlacement, selectedIndex);
        } else {
            paintAllTabs(g, clipRect, tabPlacement, selectedIndex);
        }

        if (DEBUG_CUTOUT) {
            g.dispose();
        }
    }

    protected void paintAllTabs(Graphics g, Rectangle clipRect, int tabPlacement, int selectedIndex) {
        boolean drawSelectedLast = false;
        for (int i = 0; i < rects.length; i++) {
            if (i == selectedIndex) {
                drawSelectedLast = true;
            } else {
                if (rects[i].intersects(clipRect)) {
                    paintTabNormal(g, tabPlacement, i, false);
                }
            }
        }

        // paint the selected tab last
        if (drawSelectedLast && rects[selectedIndex].intersects(clipRect)) {
            paintTabNormal(g, tabPlacement, selectedIndex, true);
        }
    }

    protected void paintScrollingTabs(Graphics g, Rectangle clipRect, int tabPlacement, int selectedIndex) {
        // for each visible tab, except the selected one
        for (int i = 0; i < visibleTabState.getTotal(); i++) {
            int realIndex = visibleTabState.getIndex(i);
            if (realIndex != selectedIndex) {
                if (rects[realIndex].intersects(clipRect)) {
                    paintTabNormal(g, tabPlacement, realIndex, false);
                }
            }
        }

        Rectangle leftScrollTabRect = visibleTabState.getLeftScrollTabRect();
        if (visibleTabState.needsLeftScrollTab() && leftScrollTabRect.intersects(clipRect)) {
            paintTabNormalFromRect(g, tabPlacement, leftScrollTabRect, LEFT_SCROLL_TAB, false, fIconRect, fTextRect);
        }

        Rectangle rightScrollTabRect = visibleTabState.getRightScrollTabRect();
        if (visibleTabState.needsRightScrollTab() && rightScrollTabRect.intersects(clipRect)) {
            paintTabNormalFromRect(g, tabPlacement, rightScrollTabRect, RIGHT_SCROLL_TAB, false, fIconRect, fTextRect);
        }

        if (selectedIndex >= 0) { // && rects[selectedIndex].intersects(clipRect)) {
            paintTabNormal(g, tabPlacement, selectedIndex, true);
        }
    }

    @Override
    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        // A tabbed pane is focusable in Full Keyboard Access mode. The selected tab displays a focus ring.
        // The individual tabs are not focusable.

        int selectedIndex = tabPane.getSelectedIndex();
        if (selectedIndex >= 0 && selectedIndex < rects.length) {
            Rectangle bounds = rects[selectedIndex];
            return createFocusRingOutline(selectedIndex, bounds);
        }

        return null;
    }

    protected @NotNull Shape createFocusRingOutline(int tabIndex, Rectangle bounds) {
        int tabPlacement = tabPane.getTabPlacement();
        boolean isVertical = tabPlacement == JTabbedPane.LEFT || tabPlacement == JTabbedPane.RIGHT;

        int x = bounds.x;
        int y = bounds.y;
        int width = isVertical ? bounds.height : bounds.width;
        int height = isVertical ? bounds.width : bounds.height;

        SegmentedButtonLayoutConfiguration lg = getTabLayoutConfiguration(tabIndex);
        AppearanceManager.ensureAppearance(tabPane);
        AquaUtils.configure(painter, tabPane, width, height);
        Shape outline = painter.getOutline(lg);
        AffineTransform tr = new AffineTransform();

        if (isVertical) {
            tr.translate(x + height, y);
            tr.rotate(kNinetyDegrees);
        } else {
            tr.translate(x, y);
        }

        return tr.createTransformedShape(outline);
    }

    private static boolean isScrollArrowTab(int index) {
        return index == LEFT_SCROLL_TAB || index == RIGHT_SCROLL_TAB;
    }

    protected static void transposeRect(@NotNull Rectangle r) {
        int temp = r.width;
        r.width = r.height;
        r.height = temp;
    }

    protected int getTabLabelShiftX(int tabPlacement, int tabIndex, boolean isSelected) {
        Rectangle tabRect = (tabIndex >= 0 ? rects[tabIndex] : visibleTabState.getRightScrollTabRect());
        int nudge = 0;
//        switch (tabPlacement) {
//            case LEFT:
//            case RIGHT:
//                nudge = tabRect.height % 2;
//                break;
//            case BOTTOM:
//            case TOP:
//            default:
//                nudge = tabRect.width % 2;
//        }
        return nudge;
    }

    protected int getTabLabelShiftY(int tabPlacement, int tabIndex, boolean isSelected) {
//        switch (tabPlacement) {
//            case RIGHT:
//            case LEFT:
//            case BOTTOM:
//                return -1;
//            case TOP:
//            default:
//        }
        return 0;
    }

    protected @Nullable Icon getIconForScrollTab(int tab, boolean enabled) {
        int direction = tab == LEFT_SCROLL_TAB ? WEST : EAST;
        Image image = AquaImageFactory.getArrowImageForDirection(direction);
        if (image == null) {
            return null;
        }

        if (!enabled) {
            image = AquaImageFactory.getProcessedImage(image, AquaImageFactory.LIGHTEN_FOR_DISABLED);
        } else if (isDark) {
            image = AquaImageFactory.getProcessedImage(image, AquaImageFactory.LIGHTEN_100);
        }

        if (sizeVariant == Size.MINI) {
            int w = (int) (image.getWidth(null) * 0.8);
            int h = (int) (image.getHeight(null) * 0.8);
            image = image.getScaledInstance(w, h, Image.SCALE_SMOOTH);
        }

        return new ImageIcon(image);
    }

    protected void paintTabNormal(@NotNull Graphics g, int tabPlacement, int tabIndex, boolean isSelected) {
        paintTabNormalFromRect(g, tabPlacement, rects[tabIndex], tabIndex, isSelected, fIconRect, fTextRect);
    }

    /**
     * Paint one tab, which may be a scroll arrow tab.
     * @param tab The semantic tab index or the ID of a scroll arrow tab.
     */
    protected void paintTabNormalFromRect(@NotNull Graphics g, int tabPlacement, Rectangle tabRect,
                                          int tab, boolean isSelected,
                                          @NotNull Rectangle iconRect, @NotNull Rectangle textRect) {

        AquaTabsView tv = getPaneModel().getView(false, true);
        int visualIndex = tv.find(tab);
        if (visualIndex < 0) {
            // not visible
            return;
        }

        Direction direction = getDirection();
        boolean isVertical = direction == Direction.LEFT || direction == Direction.RIGHT;

        if (isVertical) {
            // Rotate the graphics so that we can paint normally and it will display vertically.

            Graphics2D gg = AquaUtils.toGraphics2D(g);
            if (gg == null) {
                return;
            }
            gg = (Graphics2D) gg.create();

            if (tabPlacement == LEFT) {
                gg.translate(0, tabRect.height);
            } else {
                gg.translate(tabRect.width, 0);
            }

            double rotateAmount = (tabPlacement == LEFT ? -kNinetyDegrees : kNinetyDegrees);
            gg.transform(AffineTransform.getRotateInstance(rotateAmount, tabRect.x, tabRect.y));

            transposeRect(fContentRect);
            transposeRect(tabRect);
            g = gg;
        }

        SegmentedButtonConfiguration bg = getTabConfiguration(tv, visualIndex, isSelected);
        paintTabBackground(g, tabRect, bg);
        Insets labelInsets = getTabInsets(tabPlacement, tab);
        fContentRect.setBounds(tabRect.x + labelInsets.left, tabRect.y + labelInsets.top,
          tabRect.width - labelInsets.left - labelInsets.right, tabRect.height - labelInsets.top - labelInsets.bottom);
        paintTabContents(g, tabPlacement, tab, tabRect, iconRect, textRect, bg);

        if (isVertical) {
            g.dispose();
            transposeRect(tabRect);
        }
    }

    protected void paintTabBackground(@NotNull Graphics g,
                                      @NotNull Rectangle tabRect,
                                      @NotNull SegmentedButtonConfiguration bg) {
        AquaUtils.configure(painter, tabPane, tabRect.width, tabRect.height);
        Painter p = painter.getPainter(bg);
        p.paint(g, tabRect.x, tabRect.y);
    }

    protected @NotNull SegmentedButtonConfiguration
    getTabConfiguration(@NotNull AquaTabsView tv, int visualIndex, boolean isSelected) {

        // This code is similar to AquaSegmentedButtonBorder.getConfiguration()

        int version = AquaPainting.getVersion();
        boolean isSlider = version >= 1600;

        int tab = tv.identifyTabAtIndex(visualIndex);

        State state = getTabState(tab);
        boolean isSelectedBefore = tv.isSelected(visualIndex - 1);
        boolean isSelectedAfter = tv.isSelected(visualIndex + 1);
        Position segmentPosition = getSegmentPosition(tv, visualIndex);
        boolean isPaintedBefore = false;
        boolean isPaintedAfter = segmentPosition == Position.FIRST || segmentPosition == Position.MIDDLE;
        // The divider next to the selected segment must be suppressed in certain cases.
        if (isPaintedAfter && isSelectedAfter && isSlider) {
            isPaintedAfter = false;
        }
        boolean isFocused = tabPane.hasFocus() && isSelected;
        DividerState leftState = AquaSegmentedButtonBorder.getDividerState(isPaintedBefore, isSelectedBefore);
        DividerState rightState = AquaSegmentedButtonBorder.getDividerState(isPaintedAfter, isSelectedAfter);
        return new SegmentedButtonConfiguration(buttonWidget, sizeVariant, state, isSelected,
          isFocused, Direction.UP, segmentPosition, leftState, rightState, SwitchTracking.SELECT_ONE);
    }

    protected void paintTabContents(@NotNull Graphics g,
                                    int tabPlacement,
                                    int tab,
                                    Rectangle tabRect,
                                    Rectangle iconRect,
                                    Rectangle textRect,
                                    @NotNull SegmentedButtonConfiguration bg) {
        String title;
        Icon icon;

        if (isScrollArrowTab(tab)) {
            title = null;
            icon = getIconForScrollTab(tab, tabPane.isEnabled());
        } else {
            Component component = getTabComponentAt(tab);
            if (component != null) {
                return;
            }
            title = tabPane.getTitleAt(tab);
            icon = getIconForTab(tab);
        }

        Shape temp = g.getClip();
        g.clipRect(fContentRect.x, fContentRect.y, fContentRect.width, fContentRect.height);

        Font font = tabPane.getFont();
        FontMetrics metrics = g.getFontMetrics(font);

        // our scrolling tabs
        layoutLabel(tabPlacement, metrics, tab < 0 ? 0 : tab, title, icon, fContentRect, iconRect, textRect, false); // Never give it "isSelected" - ApprMgr handles this

        // from super.paintText - its normal text painting is totally wrong for the Mac
        if (!(g instanceof Graphics2D)) {
            g.setClip(temp);
            return;
        }
        Graphics2D g2d = (Graphics2D) g;

        // not for the scrolling tabs
        if (tab >= 0) {
            paintTitle(g2d, font, metrics, textRect, tab, title, bg);
        }

        if (icon != null) {
            paintIcon(g, tabPlacement, tab, icon, iconRect, bg);
        }

        g.setClip(temp);
    }

    protected void paintTitle(@NotNull Graphics2D g2d,
                              Font font,
                              FontMetrics metrics,
                              Rectangle textRect,
                              int tabIndex,
                              String title,
                              @NotNull SegmentedButtonConfiguration bg) {
        View v = getTextViewForTab(tabIndex);
        if (v != null) {
            v.paint(g2d, textRect);
            return;
        }

        if (title == null) {
            return;
        }

        Color color = tabPane.getForegroundAt(tabIndex);
        if (color instanceof UIResource) {
            g2d.setColor(getTabTextColor(bg));
        } else {
            g2d.setColor(color);
        }

        g2d.setFont(font);
        JavaSupport.drawString(tabPane, g2d, title, textRect.x, textRect.y + metrics.getAscent());
    }

    protected @NotNull Color getTabTextColor(@NotNull SegmentedButtonConfiguration bg) {
        assert appearanceContext != null;
        AppearanceContext tabContext = appearanceContext.withSelected(bg.isSelected()).withState(bg.getState());
        return colors.getForeground(tabContext);
    }

    protected void paintIcon(Graphics g,
                             int tabPlacement,
                             int tabIndex,
                             Icon icon,
                             Rectangle iconRect,
                             @NotNull SegmentedButtonConfiguration bg) {
        if (icon != null) {
            icon.paintIcon(tabPane, g, iconRect.x, iconRect.y);
        }
    }

    protected boolean isPressedAt(int index) {
        return false;   // not needed, so not implemented
    }

    protected @NotNull Direction getDirection() {
        switch (tabPane.getTabPlacement()) {
            case SwingConstants.BOTTOM: return Direction.DOWN;
            case SwingConstants.LEFT: return Direction.LEFT;
            case SwingConstants.RIGHT: return Direction.RIGHT;
        }
        return Direction.UP;
    }

    protected static @NotNull Position getSegmentPosition(@NotNull AquaTabsView tv, int visualIndex) {
        if (tv.getCountExcludingScrollTabs() < 2) {
            return ONLY;
        }
        if (visualIndex == 0) {
            return FIRST;
        }
        if (visualIndex == tv.getCount()-1) {
            return LAST;
        }
        return MIDDLE;
    }

    protected @NotNull State getState() {
        if (AquaFocusHandler.isActive(tabPane)) {
            if (tabPane.isEnabled()) {
                return State.ACTIVE;
            } else {
                return State.DISABLED;
            }
        } else {
            if (tabPane.isEnabled()) {
                return State.INACTIVE;
            } else {
                return State.DISABLED_INACTIVE;
            }
        }
    }

    protected @NotNull State getTabState(int tab) {
        if (appearanceContext != null) {
            State state = appearanceContext.getState();
            if (state == State.INACTIVE || state == State.DISABLED || state == State.DISABLED_INACTIVE) {
                return state;
            }
        }
        if (!tabPane.isEnabled()) {
            return State.DISABLED;
        }
        if (pressedTab == tab) {
            return State.PRESSED;
        }
        return State.ACTIVE;
    }

    protected @NotNull Insets getContentBorderInsets(int tabPlacement) {
        Insets draw = getContentDrawingInsets(tabPlacement); // will be rotated

        rotateInsets(contentBorderInsets, currentContentBorderInsets, tabPlacement);

        currentContentBorderInsets.left += draw.left;
        currentContentBorderInsets.right += draw.right;
        currentContentBorderInsets.top += draw.top;
        currentContentBorderInsets.bottom += draw.bottom;

        return currentContentBorderInsets;
    }

    protected static void rotateInsets(Insets topInsets, Insets targetInsets, int targetPlacement) {
        switch (targetPlacement) {
            case LEFT:
                targetInsets.top = topInsets.right;
                targetInsets.left = topInsets.top;
                targetInsets.bottom = topInsets.left;
                targetInsets.right = topInsets.bottom;
                break;
            case BOTTOM:
                targetInsets.top = topInsets.bottom;
                targetInsets.left = topInsets.left;
                targetInsets.bottom = topInsets.top;
                targetInsets.right = topInsets.right;
                break;
            case RIGHT:
                targetInsets.top = topInsets.left;
                targetInsets.left = topInsets.bottom;
                targetInsets.bottom = topInsets.right;
                targetInsets.right = topInsets.top;
                break;
            case TOP:
            default:
                targetInsets.top = topInsets.top;
                targetInsets.left = topInsets.left;
                targetInsets.bottom = topInsets.bottom;
                targetInsets.right = topInsets.right;
        }
    }

    protected @NotNull Insets getContentDrawingInsets(int tabPlacement) {
        rotateInsets(contentDrawingInsets, currentContentDrawingInsets, tabPlacement);
        return currentContentDrawingInsets;
    }

    protected @Nullable Icon getIconForTab(int tabIndex) {
        Icon mainIcon = super.getIconForTab(tabIndex);
        if (mainIcon == null) {
            return null;
        }

        int iconHeight = mainIcon.getIconHeight();
        if (iconHeight <= maxIconSize) {
            return mainIcon;
        }
        float ratio = (float)maxIconSize / (float)iconHeight;
        int iconWidth = mainIcon.getIconWidth();
        return new AquaIcon.CachingScalingIcon((int)(iconWidth * ratio), maxIconSize) {
            Image createImage() {
                return AquaIcon.getImageForIcon(mainIcon);
            }
        };
    }

    private static boolean DEBUG_CUTOUT = false;

    protected void paintContentBorder(@NotNull Graphics g, int tabPlacement, int selectedIndex) {
        int width = tabPane.getWidth();
        int height = tabPane.getHeight();
        Insets insets = tabPane.getInsets();

        int x = insets.left;
        int y = insets.top;
        int w = width - insets.right - insets.left;
        int h = height - insets.top - insets.bottom;

        int tabBorderInset = fixedTabHeight / 2;

        // The tabbed pane group border has its own insets.

        tabBorderInset -= 5;

        switch (tabPlacement) {
            case TOP:
                y += tabBorderInset;
                h -= tabBorderInset;
                break;
            case BOTTOM:
                h -= tabBorderInset;
                break;
            case LEFT:
                x += tabBorderInset;
                w -= tabBorderInset;
                break;
            case RIGHT:
                w -= tabBorderInset;
                break;
        }

        Border border = getBorder();
        Shapes cutout = getContentBorderCutout();
        if (cutout != null) {
            // HiDPI apparently not needed
            Rectangle clipRect = g.getClipBounds();
            BufferedImage b = new BufferedImage(clipRect.width, clipRect.height, BufferedImage.TYPE_INT_ARGB);
            Graphics2D bg = b.createGraphics();
            bg.translate(-clipRect.x, -clipRect.y);
            border.paintBorder(tabPane, bg, x, y, w, h);
            bg.setComposite(AlphaComposite.Src);
            bg.setColor(DEBUG_CUTOUT ? new Color(255, 0, 0, 120) : AquaColors.CLEAR);
            cutout.fill(bg);
            bg.dispose();
            g.drawImage(b, clipRect.x, clipRect.y, clipRect.width, clipRect.height, null);
        } else {
            border.paintBorder(tabPane, g, x, y, w, h);
        }
    }

    private @NotNull Border getBorder() {
        Object o = tabPane.getClientProperty(CONTENT_BACKGROUND_KEY);
        if (o instanceof Color) {
            Color cb = (Color) o;
            return AquaGroupBorder.createTabbedPaneBorder(cb);
        }
        return AquaGroupBorder.getTabbedPaneGroupBorder();
    }

    /**
     * Identify a region of the content background that should not be painted. This feature is needed to support
     * tabbed panes that use translucent buttons (a feature originally used only in dark mode, but needed in light
     * mode starting with macOS 11). Without the cutout, the content group box shows through the tab buttons.
     */
    protected @Nullable Shapes getContentBorderCutout() {
        int tabCount = tabPane.getTabCount();
        if (tabCount > 0) {
            if (DEBUG_CUTOUT || isDark || OSVersion >= 1016) {

                Shapes shapes = new Shapes();

                if (visibleTabState.needsScrollTabs()) {
                    if (visibleTabState.needsLeftScrollTab()) {
                        Rectangle rect = visibleTabState.getLeftScrollTabRect();
                        addTabShape(shapes, LEFT_SCROLL_TAB, rect);
                    }

                    if (visibleTabState.needsRightScrollTab()) {
                        Rectangle rect = visibleTabState.getRightScrollTabRect();
                        addTabShape(shapes, RIGHT_SCROLL_TAB, rect);
                    }

                    for (int i = 0; i < visibleTabState.getTotal(); i++) {
                        int realIndex = visibleTabState.getIndex(i);
                        if (realIndex >= 0) {
                            Rectangle bounds = rects[realIndex];
                            addTabShape(shapes, realIndex, bounds);
                        }
                    }
                } else {
                    for (int i = 0; i < tabCount; i++) {
                        Rectangle bounds = rects[i];
                        addTabShape(shapes, i, bounds);
                    }
                }

                if (!shapes.isEmpty()) {
                    // The individual shapes leave gaps between the buttons that may cause problems.
                    // This is not a general solution, but it works where needed.
                    Rectangle bounds = shapes.getOuterBounds();
                    assert bounds != null;
                    RoundRectangle2D rr = new RoundRectangle2D.Double(bounds.getX(), bounds.getY(),
                      bounds.getWidth(), bounds.getHeight(), 8, 8);
                    shapes = new Shapes();
                    shapes.add(rr);
                    return shapes;
                }
            }
        }
        return null;
    }

    private void addTabShape(@NotNull Shapes shapes, int tabIndex, @NotNull Rectangle rect) {
        Shape s = createFocusRingOutline(tabIndex, rect);
        shapes.add(s);
    }

    private static class Shapes {

        private final @NotNull List<Shape> shapes = new ArrayList<>();
        private @Nullable Rectangle outerBounds;

        public void add(@NotNull Shape s) {
            Rectangle bounds = s.getBounds();
            if (bounds.width > 0 && bounds.height > 0) {
                shapes.add(s);
                if (outerBounds == null) {
                    outerBounds = bounds;
                } else {
                    outerBounds.add(bounds);
                }
            }
        }

        public boolean isEmpty() {
            return shapes.isEmpty();
        }

        public @Nullable Rectangle getOuterBounds()
        {
            return outerBounds;
        }

        public void fill(@NotNull Graphics2D g) {
            for (Shape s : shapes) {
                g.fill(s);
            }
        }
    }

    public boolean isTabVisible(int index) {
        if (!visibleTabState.needsScrollTabs()) {
            return true;
        }
        if (isScrollArrowTab(index)) {
            return true;
        }
        for (int i = 0; i < visibleTabState.getTotal(); i++) {
            if (visibleTabState.getIndex(i) == index) {
                return true;
            }
        }
        return false;
    }

    protected @NotNull SegmentedButtonLayoutConfiguration getTabLayoutConfiguration(int tab) {
        Position position = getTabPosition(tab);
        return new SegmentedButtonLayoutConfiguration(buttonWidget, sizeVariant, position);
    }

    protected @NotNull Position getTabPosition(int tab) {
        AquaTabsView tv = getPaneModel().getView(false, true);
        int visualIndex = tv.find(tab);
        if (visualIndex < 0) {
            // not visible
            return MIDDLE;
        }
        int tabCount = tv.getCount();
        if (tabCount < 2) {
            return ONLY;
        } else if (visualIndex == 0) {
            return FIRST;
        } else if (visualIndex == tabCount - 1) {
            return LAST;
        } else {
            return MIDDLE;
        }
    }

    @Override
    protected @NotNull Insets getTabInsets(int tabPlacement, int tab) {
        Position position = getTabPosition(tab);
        switch (position) {
            case ONLY:
                return onlyTabInsets;
            case FIRST:
                return leftTabInsets;
            case LAST:
                return rightTabInsets;
            case MIDDLE:
                return middleTabInsets;
        }
        return new Insets(3, 10, 3, 10);
    }

    /**
     * Returns the bounds of the specified tab index. The bounds are with respect to the JTabbedPane's coordinate space.
     * If the tab at this index is not currently visible in the UI, then returns null.
     */
    @Override
    public @Nullable Rectangle getTabBounds(@NotNull JTabbedPane pane, int i) {
        if (visibleTabState.needsScrollTabs()
          && (visibleTabState.isBefore(i) || visibleTabState.isAfter(i))) {
            return null;
        }
        return super.getTabBounds(pane, i);
    }

    /**
     * Identify the tab that intersects the specified point in the JTabbedPane's coordinate space.
     */
    public int tabForCoordinate(@NotNull JTabbedPane pane, int x, int y) {
        ensureCurrentLayout();
        Point p = new Point(x, y);
        if (visibleTabState.needsScrollTabs()) {
            for (int i = 0; i < visibleTabState.getTotal(); i++) {
                int realOffset = visibleTabState.getIndex(i);
                if (rects[realOffset].contains(p.x, p.y)) {
                    return realOffset;
                }
            }
//            if (visibleTabState.getRightScrollTabRect().contains(p.x, p.y)) {
//                return -1;
//            }
        } else {
            int tabCount = tabPane.getTabCount();
            for (int i = 0; i < tabCount; i++) {
                if (rects[i].contains(p.x, p.y)) {
                    return i;
                }
            }
        }
        return -1;
    }

    // This is the preferred size - the layout manager will ignore if it has to
    protected int calculateTabHeight(int tabPlacement, int tabIndex, int fontHeight) {
        // Constrain to what the Mac allows
        // int result = super.calculateTabHeight(tabPlacement, tabIndex, fontHeight);

        return fixedTabHeight;
    }

    // JBuilder requested this - it's against HI, but then so are multiple rows
    protected boolean shouldRotateTabRuns(int tabPlacement) {
        return false;
    }

    protected class TabbedPanePropertyChangeHandler extends PropertyChangeHandler {
        public void propertyChange(PropertyChangeEvent e) {
            String prop = e.getPropertyName();

            JTabbedPane comp = (JTabbedPane)e.getSource();
            if ("componentOrientation".equals(prop) || "tabPlacement".equals(prop)) {
                updateOrientation();
                super.propertyChange(e);  // in case a future JDK does something
                return;
            }

            if ("enabled".equals(prop)) {
                configureAppearanceContext(null, comp);
                return;
            }

            super.propertyChange(e);
        }
    }

    protected void updateOrientation()
    {
        int tabPlacement = tabPane.getTabPlacement();
        isReversed = tabPlacement == SwingConstants.LEFT
          || (tabPlacement == SwingConstants.TOP || tabPlacement == SwingConstants.BOTTOM)
          && !AquaUtils.isLeftToRight(tabPane);
        tabPane.revalidate();
        tabPane.repaint();
    }

    protected ChangeListener createChangeListener() {
        return new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                if (!isTabVisible(tabPane.getSelectedIndex())) {
                    scrollPositionChangeNeeded = true;
                }
                tabPane.revalidate();
                tabPane.repaint();
            }
        };
    }

    protected class FocusHandler extends FocusAdapter {
        Rectangle sWorkingRect = new Rectangle();

        public void focusGained(FocusEvent e) {
            if (isDefaultFocusReceiver(tabPane) && !hasAvoidedFirstFocus) {
                KeyboardFocusManager.getCurrentKeyboardFocusManager().focusNextComponent();
                hasAvoidedFirstFocus = true;
            }
            adjustPaintingRectForFocusRing(e);
        }

        public void focusLost(FocusEvent e) {
            adjustPaintingRectForFocusRing(e);
        }

        void adjustPaintingRectForFocusRing(FocusEvent e) {
            JTabbedPane pane = (JTabbedPane)e.getSource();
            int tabCount = pane.getTabCount();
            int selectedIndex = pane.getSelectedIndex();

            if (selectedIndex != -1 && tabCount > 0 && tabCount == rects.length) {
                sWorkingRect.setBounds(rects[selectedIndex]);
                sWorkingRect.grow(4, 4);
                pane.repaint(sWorkingRect);
            }
        }

        boolean isDefaultFocusReceiver(@NotNull JComponent component) {
            if (isDefaultFocusReceiver == null) {
                Component defaultFocusReceiver
                  = KeyboardFocusManager.getCurrentKeyboardFocusManager().getDefaultFocusTraversalPolicy()
                  .getDefaultComponent(getTopLevelFocusCycleRootAncestor(component));
                isDefaultFocusReceiver = defaultFocusReceiver != null && defaultFocusReceiver.equals(component);
            }
            return isDefaultFocusReceiver;
        }

        Container getTopLevelFocusCycleRootAncestor(Container container) {
            Container ancestor;
            while ((ancestor = container.getFocusCycleRootAncestor()) != null) {
                container = ancestor;
            }
            return container;
        }
    }

    public class MouseHandler extends MouseInputAdapter implements ActionListener {
        protected int trackingTab = NO_TAB;
        protected Timer popupTimer = new Timer(500, this);

        public MouseHandler() {
            popupTimer.setRepeats(false);
        }

        public void mousePressed(MouseEvent e) {
            if (!tabPane.isEnabled()) {
                trackingTab = NO_TAB;
                return;
            }

            Point p = e.getPoint();
            trackingTab = identifyTabAtPoint(p);
            if (trackingTab == NO_TAB || (!shouldRepaintSelectedTabOnMouseDown() && trackingTab == tabPane.getSelectedIndex())) {
                trackingTab = NO_TAB;
                return;
            }

            if (isScrollArrowTab(trackingTab)) {
                popupTimer.start();
            }

            pressedTab = trackingTab;
            repaint(pressedTab);
        }

        public void mouseDragged(MouseEvent e) {
            if (trackingTab == NO_TAB) {
                return;
            }

            int currentTab = identifyTabAtPoint(e.getPoint());

            if (currentTab != trackingTab) {
                pressedTab = NO_TAB;
            } else {
                pressedTab = trackingTab;
            }

            if (isScrollArrowTab(trackingTab)) {
                popupTimer.start();
            }

            repaint(trackingTab);
        }

        public void mouseReleased(MouseEvent e) {
            if (trackingTab == NO_TAB) {
                return;
            }

            popupTimer.stop();

            Point p = e.getPoint();
            int currentTab = identifyTabAtPoint(p);

            if (currentTab == trackingTab) {
                if (isScrollArrowTab(trackingTab)) {
                    int offset;
                    if (trackingTab == LEFT_SCROLL_TAB) {
                        offset = isReversed ? 1 : -1;
                    } else {
                        offset = isReversed ? -1 : 1;
                    }
                    tabPane.setSelectedIndex(tabPane.getSelectedIndex() + offset);
                } else if (trackingTab >= 0) {
                    tabPane.setSelectedIndex(trackingTab);
                }
            }

            repaint(trackingTab);

            pressedTab = NO_TAB;
            trackingTab = NO_TAB;
        }

        public void actionPerformed(ActionEvent e) {
            if (trackingTab != pressedTab) {
                return;
            }

            if (isScrollArrowTab(trackingTab)) {
                showFullPopup(trackingTab);
                trackingTab = NO_TAB;
            }
        }
    }

    int identifyTabAtPoint(Point p) {
        int tabIndex = tabForCoordinate(tabPane, p.x, p.y);
        if (tabIndex >= 0 && tabPane.isEnabledAt(tabIndex)) {
            return tabIndex;
        }
        if (visibleTabState.needsLeftScrollTab() && visibleTabState.getLeftScrollTabRect().contains(p)) {
            return LEFT_SCROLL_TAB;
        }
        if (visibleTabState.needsRightScrollTab() && visibleTabState.getRightScrollTabRect().contains(p)) {
            return RIGHT_SCROLL_TAB;
        }
        return NO_TAB;
    }

    void repaint(int tab) {
        Rectangle r = getRectForTab(tab);
        if (r != null) {
            tabPane.repaint(r);
        }
    }

    @Nullable Rectangle getRectForTab(int tab) {
        if (tab == LEFT_SCROLL_TAB) {
            return visibleTabState.getLeftScrollTabRect();
        }
        if (tab == RIGHT_SCROLL_TAB) {
            return visibleTabState.getRightScrollTabRect();
        }
        if (tab >= 0) {
            return rects[tab];
        }
        return null;
    }

    void showFullPopup(int scrollTab) {
        boolean isFirstTab = (scrollTab == LEFT_SCROLL_TAB);

        JPopupMenu popup = new JPopupMenu();

        for (int i = 0; i < tabPane.getTabCount(); i++) {
            if (isFirstTab ? visibleTabState.isBefore(i) : visibleTabState.isAfter(i)) {
                popup.add(createMenuItem(i));
            }
        }

        if (isFirstTab) {
            Rectangle leftScrollTabRect = visibleTabState.getLeftScrollTabRect();
            Dimension popupRect = popup.getPreferredSize();
            popup.show(tabPane, leftScrollTabRect.x - popupRect.width, leftScrollTabRect.y + 7);
        } else {
            Rectangle rightScrollTabRect = visibleTabState.getRightScrollTabRect();
            popup.show(tabPane, rightScrollTabRect.x + rightScrollTabRect.width, rightScrollTabRect.y + 7);
        }

        popup.addPopupMenuListener(new PopupMenuListener() {
            public void popupMenuCanceled(PopupMenuEvent e) {
            }
            public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
            }
            public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                pressedTab = NO_TAB;
                tabPane.repaint(visibleTabState.getLeftScrollTabRect());
                tabPane.repaint(visibleTabState.getRightScrollTabRect());
            }
        });
    }

    @NotNull JMenuItem createMenuItem(int i) {
        Component component = getTabComponentAt(i);
        JMenuItem menuItem;
        if (component == null) {
            menuItem = new JMenuItem(tabPane.getTitleAt(i), tabPane.getIconAt(i));
        } else {
            @SuppressWarnings("serial") // anonymous class
            JMenuItem tmp = new JMenuItem() {
                public void paintComponent(Graphics g) {
                    super.paintComponent(g);
                    Dimension size = component.getSize();
                    component.setSize(getSize());
                    component.validate();
                    component.paint(g);
                    component.setSize(size);
                }

                public Dimension getPreferredSize() {
                    return component.getPreferredSize();
                }
            };
            menuItem = tmp;
        }

        Color background = tabPane.getBackgroundAt(i);
        if (!(background instanceof UIResource)) {
            menuItem.setBackground(background);
        }

        menuItem.setForeground(tabPane.getForegroundAt(i));
        // for <rdar://problem/3520267> make sure to disable items that are disabled in the tab.
        if (!tabPane.isEnabledAt(i)) menuItem.setEnabled(false);

        int fOffset = i;
        menuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                //boolean visible = isTabVisible(fOffset);
                tabPane.setSelectedIndex(fOffset);
//                if (!visible) {
//                    scrollPositionChangeNeeded = true;
//                    tabPane.invalidate();
//                    tabPane.repaint();
//                }
            }
        });

        return menuItem;
    }

    protected class AquaTruncatingTabbedPaneLayout extends TabbedPaneLayout {
        // fix for Radar #3346131
        protected int preferredTabAreaWidth(int tabPlacement, int height) {
            // Our superclass wants to stack tabs, but we rotate them,
            // so when tabs are on the left or right we know that
            // our width is actually the "height" of a tab which is then
            // rotated.
            if (tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT) {
                return super.preferredTabAreaHeight(tabPlacement, height);
            }

            return super.preferredTabAreaWidth(tabPlacement, height);
        }

        protected int preferredTabAreaHeight(int tabPlacement, int width) {
            if (tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT) {
                return super.preferredTabAreaWidth(tabPlacement, width);
            }

            return super.preferredTabAreaHeight(tabPlacement, width);
        }

        protected void calculateTabRects(int tabPlacement, int tabCount) {
            if (tabCount <= 0) return;

            superCalculateTabRects(tabPlacement, tabCount); // does most of the hard work

            // If they haven't been padded (which they only do when there are multiple rows) we should center them
            if (rects.length == 0) {
                return;
            }

            Dimension size = tabPane.getSize();
            Insets insets = tabPane.getInsets();
            Insets localTabAreaInsets = getTabAreaInsets(tabPlacement);
            int rightMargin = size.width - (insets.right + localTabAreaInsets.right);
            visibleTabState.alignRectsRunFor(rects, size, rightMargin, tabPlacement, isReversed);
        }

        protected void padTabRun(int tabPlacement, int start, int end, int max) {
            if (tabPlacement == SwingConstants.TOP || tabPlacement == SwingConstants.BOTTOM) {
                super.padTabRun(tabPlacement, start, end, max);
                return;
            }

            Rectangle lastRect = rects[end];
            int runHeight = (lastRect.y + lastRect.height) - rects[start].y;
            int deltaHeight = max - (lastRect.y + lastRect.height);
            float factor = (float)deltaHeight / (float)runHeight;
            for (int i = start; i <= end; i++) {
                Rectangle pastRect = rects[i];
                if (i > start) {
                    pastRect.y = rects[i - 1].y + rects[i - 1].height;
                }
                pastRect.height += Math.round(pastRect.height * factor);
            }
            lastRect.height = max - lastRect.y;
        }

        /**
         * This is a massive routine and I left it like this because the bulk of the code comes
         * from the BasicTabbedPaneUI class. Here is what it does:
         * 1. Calculate rects for the tabs - we have to play tricks here because our right and left tabs
         *    should get widths calculated the same way as top and bottom, but they will be rotated so the
         *    calculated width is stored as the rect height.
         * 2. Decide if we can fit all the tabs.
         * 3. When we cannot fit all the tabs we create a tab popup, and then layout the new tabs until
         *    we can't fit them anymore. Laying them out is a matter of adding them into the visible list
         *    and shifting them horizontally to the correct location.
         */
        protected synchronized void superCalculateTabRects(int tabPlacement, int tabCount) {
            Dimension size = tabPane.getSize();
            Insets insets = tabPane.getInsets();
            Insets localTabAreaInsets = getTabAreaInsets(tabPlacement);

            // Calculate bounds within which a tab run must fit
            int tabsLength;
            int x, y;
            switch (tabPlacement) {
                case SwingConstants.LEFT:
                    maxTabWidth = calculateMaxTabHeight(tabPlacement);
                    x = insets.left + localTabAreaInsets.left;
                    y = insets.top + localTabAreaInsets.top;
                    tabsLength = size.height - (insets.bottom + localTabAreaInsets.bottom);
                    break;
                case SwingConstants.RIGHT:
                    maxTabWidth = calculateMaxTabHeight(tabPlacement);
                    x = size.width - insets.right - localTabAreaInsets.right - maxTabWidth - 1;
                    y = insets.top + localTabAreaInsets.top;
                    tabsLength = size.height - (insets.bottom + localTabAreaInsets.bottom);
                    break;
                case SwingConstants.BOTTOM:
                    maxTabHeight = calculateMaxTabHeight(tabPlacement);
                    x = insets.left + localTabAreaInsets.left;
                    y = size.height - insets.bottom - localTabAreaInsets.bottom - maxTabHeight;
                    tabsLength = size.width - (insets.right + localTabAreaInsets.right);
                    break;
                case SwingConstants.TOP:
                default:
                    maxTabHeight = calculateMaxTabHeight(tabPlacement);
                    x = insets.left + localTabAreaInsets.left;
                    y = insets.top + localTabAreaInsets.top;
                    tabsLength = size.width - (insets.right + localTabAreaInsets.right);
                    break;
            }

            tabRunOverlay = getTabRunOverlay(tabPlacement);

            runCount = 0;
            selectedRun = 0;

            if (tabCount == 0) {
                return;
            }

            FontMetrics metrics = getFontMetrics();
            boolean isVertical = (tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT);
            int selectedIndex = tabPane.getSelectedIndex();

            // calculate all the widths
            // if they all fit we are done, if not
            // we have to do the dance of figuring out which ones to show.
            visibleTabState.setNeedsScrollers(false);
            for (int i = 0; i < tabCount; i++) {
                Rectangle rect = rects[i];

                if (isVertical) {
                    calculateVerticalTabRunRect(rect, metrics, tabPlacement, i, x, y);

                    // test if we need to scroll!
                    if (rect.y + rect.height > tabsLength) {
                        visibleTabState.setNeedsScrollers(true);
                    }
                } else {
                    calculateHorizontalTabRunRect(rect, metrics, tabPlacement, i, x, y);

                    // test if we need to scroll!
                    if (rect.x + rect.width > tabsLength) {
                        visibleTabState.setNeedsScrollers(true);
                    }
                }
            }

            visibleTabState.relayoutForScrolling(rects, tabsLength, selectedIndex, isVertical, tabCount);
        }

        private void calculateHorizontalTabRunRect(@NotNull Rectangle rect, FontMetrics metrics, int tabPlacement, int i, int x, int y) {
            // Tabs on TOP or BOTTOM....
            if (i > 0) {
                rect.x = rects[i - 1].x + rects[i - 1].width;
            } else {
                tabRuns[0] = 0;
                runCount = 1;
                maxTabWidth = 0;
                rect.x = x;
            }

            rect.width = calculateTabWidth(tabPlacement, i, metrics);
            maxTabWidth = Math.max(maxTabWidth, rect.width);

            rect.y = y;
            rect.height = maxTabHeight;
        }

        private void calculateVerticalTabRunRect(@NotNull Rectangle rect,
                                                 @NotNull FontMetrics metrics,
                                                 int tabPlacement, int i, int x, int y) {
            // Tabs on LEFT or RIGHT...
            if (i > 0) {
                rect.y = rects[i - 1].y + rects[i - 1].height;
            } else {
                tabRuns[0] = 0;
                runCount = 1;
                maxTabHeight = 0;
                rect.y = y;
            }

            rect.height = calculateTabWidth(tabPlacement, i, metrics);
            maxTabHeight = Math.max(maxTabHeight, rect.height);

            rect.x = x;
            rect.width = maxTabWidth;
        }

        protected void layoutTabComponents() {
            Container tabContainer = getTabContainer();
            if (tabContainer == null) {
                return;
            }

            int placement = tabPane.getTabPlacement();
            Rectangle rect = new Rectangle();
            Point delta = new Point(-tabContainer.getX(), -tabContainer.getY());

            for (int i = 0; i < tabPane.getTabCount(); i++) {
                Component c = getTabComponentAt(i);
                if (c == null) {
                    continue;
                }

                getTabBounds(i, rect);
                Insets insets = getTabInsets(tabPane.getTabPlacement(), i);
                boolean isSelected = i == tabPane.getSelectedIndex();

                if (placement == SwingConstants.TOP || placement == SwingConstants.BOTTOM) {
                    rect.x += insets.left + delta.x + getTabLabelShiftX(placement, i, isSelected);
                    rect.y += insets.top + delta.y + getTabLabelShiftY(placement, i, isSelected);
                    rect.width -= insets.left + insets.right;
                    rect.height -= insets.top + insets.bottom - 1;
                } else {
                    rect.x += insets.top + delta.x + getTabLabelShiftY(placement, i, isSelected);
                    rect.y += insets.left + delta.y + getTabLabelShiftX(placement, i, isSelected);
                    rect.width -= insets.top + insets.bottom - 1;
                    rect.height -= insets.left + insets.right;
                }

                c.setBounds(rect);
            }
        }
    }
}
