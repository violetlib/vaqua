/*
 * Changes Copyright (c) 2015-2018 Alan Snyder.
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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.View;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.*;
import org.violetlib.jnr.aqua.SegmentedButtonConfiguration;
import org.violetlib.jnr.aqua.SegmentedButtonLayoutConfiguration;

import static org.violetlib.jnr.aqua.AquaUIPainter.Position.*;
import static org.violetlib.jnr.aqua.SegmentedButtonConfiguration.DividerState;

public class AquaTabbedPaneUI extends AquaTabbedPaneCopyFromBasicUI
        implements AquaUtilControlSize.Sizeable, FocusRingOutlineProvider, AquaComponentUI {

    // Create PLAF
    public static ComponentUI createUI(JComponent c) {
        return new AquaTabbedPaneUI();
    }

    private static final int kSmallTabHeight = 20; // height of a small tab
    private static final int kLargeTabHeight = 23; // height of a large tab
    private static final int kMaxIconSize = kLargeTabHeight - 7;

    private static final double kNinetyDegrees = (Math.PI / 2.0); // used for rotation

    protected SegmentedButtonWidget buttonWidget = SegmentedButtonWidget.BUTTON_TAB;

    protected final Insets currentContentDrawingInsets = new Insets(0, 0, 0, 0);
    protected final Insets currentContentBorderInsets = new Insets(0, 0, 0, 0);
    protected final Insets contentDrawingInsets = new Insets(0, 0, 0, 0);

    protected int pressedTab = -3; // -2 is right scroller, -1 is left scroller
    protected boolean popupSelectionChanged;

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
    protected boolean isLeftToRight;
    protected boolean isDark;

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

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

        updateLayoutParameters();
        contentDrawingInsets.set(0, 10, 10, 10);
        LookAndFeel.installProperty(tabPane, "opaque", false);
        configureAppearanceContext(null, tabPane);
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
        AquaFullKeyboardFocusableHandler.addListener(tabPane);
        AppearanceManager.installListener(tabPane);
    }

    @Override
    protected void uninstallListeners() {

        if (mouseListener != null) {
            tabPane.removeMouseMotionListener((MouseMotionListener) mouseListener);
        }

        AppearanceManager.uninstallListener(tabPane);
        AquaUtilControlSize.removeSizePropertyListener(tabPane);
        AquaFullKeyboardFocusableHandler.removeListener(tabPane);

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
        // Therefore the First position is always on the left side.

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
    }

    protected Insets getTabInsets(Position pos) {
        SegmentedButtonLayoutConfiguration g = new SegmentedButtonLayoutConfiguration(buttonWidget, sizeVariant, pos);
        Insetter s = painter.getLayoutInfo().getSegmentedButtonLabelInsets(g);
        Insets n = s.asInsets();
        if (n == null) {
            n = new Insets(3, 0, 3, 0);
        }
        AquaButtonExtendedTypes.WidgetInfo info = AquaButtonExtendedTypes.getTabWidgetInfo(sizeVariant, pos);
        int margin = info.getMargin();
        return new Insets(n.top, n.left + margin, n.bottom, n.right + margin);
    }

    protected void assureRectsCreated(int tabCount) {
        visibleTabState.init(tabCount);
        super.assureRectsCreated(tabCount);
    }

    // Paint Methods
    // Cache for performance
    Rectangle fContentRect = new Rectangle();
    Rectangle fIconRect = new Rectangle();
    Rectangle fTextRect = new Rectangle();

    // UI Rendering

    @Override
    public void update(Graphics g, JComponent c) {
        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        if (c.isOpaque()) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    public void paint(Graphics g, JComponent c) {
        int tabPlacement = tabPane.getTabPlacement();
        int selectedIndex = tabPane.getSelectedIndex();
        paintContentBorder(g, tabPlacement, selectedIndex);

        // we want to call ensureCurrentLayout, but it's private
        ensureCurrentLayout();
        Rectangle clipRect = g.getClipBounds();

        isLeftToRight = (tabPane.getComponentOrientation().isLeftToRight() || tabPlacement == LEFT) && tabPlacement != RIGHT;

        // Paint tabRuns of tabs from back to front
        if (visibleTabState.needsScrollTabs()) {
            paintScrollingTabs(g, clipRect, tabPlacement, selectedIndex);
            return;
        }

        // old way
        paintAllTabs(g, clipRect, tabPlacement, selectedIndex);
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

        // paint the selected tab last.
        if (drawSelectedLast && rects[selectedIndex].intersects(clipRect)) {
            paintTabNormal(g, tabPlacement, selectedIndex, true);
        }
    }

    protected void paintScrollingTabs(Graphics g, Rectangle clipRect, int tabPlacement, int selectedIndex) {
//        Graphics g2 = g.create();
//        g2.setColor(Color.cyan);
//        Rectangle r = new Rectangle();
//        for (int i = 0; i < visibleTabState.getTotal(); i++) {
//            r.add(rects[visibleTabState.getIndex(i)]);
//        }
//        g2.fillRect(r.x, r.y, r.width, r.height);
//        g2.dispose();
//        System.out.println(r);

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
            paintTabNormalFromRect(g, tabPlacement, leftScrollTabRect, -2, false, fIconRect, fTextRect);
        }

        Rectangle rightScrollTabRect = visibleTabState.getRightScrollTabRect();
        if (visibleTabState.needsRightScrollTab() && rightScrollTabRect.intersects(clipRect)) {
            paintTabNormalFromRect(g, tabPlacement, rightScrollTabRect, -1, false, fIconRect, fTextRect);
        }

        if (selectedIndex >= 0) { // && rects[selectedIndex].intersects(clipRect)) {
            paintTabNormal(g, tabPlacement, selectedIndex, true);
        }
    }

    @Override
    public Shape getFocusRingOutline(JComponent c) {
        // On Yosemite, a tabbed pane is focusable in Full Keyboard Access mode. The selected tab displays a focus ring.
        // The individual tabs are not focusable.

        int selectedIndex = tabPane.getSelectedIndex();
        if (selectedIndex >= 0 && selectedIndex < rects.length) {
            Rectangle bounds = rects[selectedIndex];
            return createFocusRingOutline(selectedIndex, bounds);
        }

        return null;
    }

    protected Shape createFocusRingOutline(int tabIndex, Rectangle bounds) {
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

    private static boolean isScrollTabIndex(int index) {
        return index == -1 || index == -2;
    }

    protected static void transposeRect(Rectangle r) {
        int temp = r.y;
//        r.y = r.x;
//        r.x = temp;
        temp = r.width;
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

    protected @Nullable Icon getIconForScrollTab(int tabPlacement, int tabIndex, boolean enabled) {
        boolean shouldFlip = !AquaUtils.isLeftToRight(tabPane);
        if (tabPlacement == RIGHT) shouldFlip = false;
        if (tabPlacement == LEFT) shouldFlip = true;

        int direction = tabIndex == -1 ? EAST : WEST;
        if (shouldFlip) {
            if (direction == EAST) {
                direction = WEST;
            } else if (direction == WEST) {
                direction = EAST;
            }
        }

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

    protected void paintTabNormal(Graphics g, int tabPlacement, int tabIndex, boolean isSelected) {
        paintTabNormalFromRect(g, tabPlacement, rects[tabIndex], tabIndex, isSelected, fIconRect, fTextRect);
    }

    protected void paintTabNormalFromRect(Graphics g, int tabPlacement, Rectangle tabRect, int nonRectIndex,
                                          boolean isSelected,
                                          Rectangle iconRect, Rectangle textRect) {
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

        paintTabBackground(g, tabRect, isSelected, isLeftToRight, nonRectIndex);

        Insets labelInsets = getTabInsets(tabPlacement, nonRectIndex);
        fContentRect.setBounds(tabRect.x + labelInsets.left, tabRect.y + labelInsets.top,
                tabRect.width - labelInsets.left - labelInsets.right, tabRect.height - labelInsets.top - labelInsets.bottom);

        paintContents(g, tabPlacement, nonRectIndex, tabRect, iconRect, textRect, isSelected);

        if (isVertical) {
            g.dispose();
            transposeRect(tabRect);
        }
    }

    protected void paintTabBackground(@NotNull Graphics g,
                                      @NotNull Rectangle tabRect,
                                      boolean isSelected,
                                      boolean isLeftToRight,
                                      int nonRectIndex) {
        AquaUtils.configure(painter, tabPane, tabRect.width, tabRect.height);
        SegmentedButtonConfiguration bg = getConfiguration(isSelected, isLeftToRight, nonRectIndex);
        Painter p = painter.getPainter(bg);
        p.paint(g, tabRect.x, tabRect.y);
    }

    protected SegmentedButtonConfiguration getConfiguration(boolean isSelected,
                                                            boolean isLeftToRight,
                                                            int nonRectIndex) {

        int tabCount = tabPane.getTabCount();

        boolean needsLeftScrollTab = visibleTabState.needsLeftScrollTab();
        boolean needsRightScrollTab = visibleTabState.needsRightScrollTab();

        // first or last
        boolean first = nonRectIndex == 0;
        boolean last = nonRectIndex == tabCount - 1;
        if (needsLeftScrollTab || needsRightScrollTab) {
            if (nonRectIndex == -1) {
                first = false;
                last = true;
            } else if (nonRectIndex == -2) {
                first = true;
                last = false;
            } else {
                if (needsLeftScrollTab) first = false;
                if (needsRightScrollTab) last = false;
            }
        }

        Direction direction = getDirection();
        if (direction == Direction.LEFT || direction == Direction.RIGHT) {
            boolean tempSwap = last;
            last = first;
            first = tempSwap;
        }

        State state = getTabState(nonRectIndex, isSelected);
        boolean showSelected = isSelected;
        boolean showLeftNeighborSelected = false;   // TBD
        boolean showRightNeighborSelected = false;  // TBD

        Position segmentPosition = getSegmentPosition(first, last, isLeftToRight);
        int selectedIndex = tabPane.getSelectedIndex();
        boolean segmentTrailingSeparator = getSegmentTrailingSeparator(nonRectIndex, selectedIndex, isLeftToRight);
        boolean segmentLeadingSeparator = getSegmentLeadingSeparator(nonRectIndex, selectedIndex, isLeftToRight);
        boolean isFocused = tabPane.hasFocus() && isSelected;
        DividerState leftState = AquaSegmentedButtonBorder.getDividerState(segmentLeadingSeparator, showLeftNeighborSelected);
        DividerState rightState = AquaSegmentedButtonBorder.getDividerState(segmentTrailingSeparator, showRightNeighborSelected);
        return new SegmentedButtonConfiguration(buttonWidget, sizeVariant, state, showSelected,
                isFocused, Direction.UP, segmentPosition, leftState, rightState);
    }

    protected void paintContents(@NotNull Graphics g,
                                 int tabPlacement,
                                 int tabIndex,
                                 Rectangle tabRect,
                                 Rectangle iconRect,
                                 Rectangle textRect,
                                 boolean isSelected) {
        String title;
        Icon icon;

        if (isScrollTabIndex(tabIndex)) {
            title = null;
            icon = getIconForScrollTab(tabPlacement, tabIndex, tabPane.isEnabled());
        } else {
            Component component = getTabComponentAt(tabIndex);
            if (component != null) {
                return;
            }
            title = tabPane.getTitleAt(tabIndex);
            icon = getIconForTab(tabIndex);
        }

        Shape temp = g.getClip();
        g.clipRect(fContentRect.x, fContentRect.y, fContentRect.width, fContentRect.height);

        Font font = tabPane.getFont();
        FontMetrics metrics = g.getFontMetrics(font);

        // our scrolling tabs
        layoutLabel(tabPlacement, metrics, tabIndex < 0 ? 0 : tabIndex, title, icon, fContentRect, iconRect, textRect, false); // Never give it "isSelected" - ApprMgr handles this

        // from super.paintText - its normal text painting is totally wrong for the Mac
        if (!(g instanceof Graphics2D)) {
            g.setClip(temp);
            return;
        }
        Graphics2D g2d = (Graphics2D) g;

        // not for the scrolling tabs
        if (tabIndex >= 0) {
            paintTitle(g2d, font, metrics, textRect, tabIndex, isSelected, title);
        }

        if (icon != null) {
            paintIcon(g, tabPlacement, tabIndex, icon, iconRect, isSelected);
        }

        g.setClip(temp);
    }

    protected void paintTitle(@NotNull Graphics2D g2d,
                              Font font,
                              FontMetrics metrics,
                              Rectangle textRect,
                              int tabIndex,
                              boolean isSelected,
                              String title) {
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
            g2d.setColor(getTabTextColor(tabIndex, isSelected));
        } else {
            g2d.setColor(color);
        }

        g2d.setFont(font);
        JavaSupport.drawString(tabPane, g2d, title, textRect.x, textRect.y + metrics.getAscent());
    }

    protected Color getTabTextColor(int tabIndex, boolean isSelected) {
        assert appearanceContext != null;
        AppearanceContext tabContext = appearanceContext;
        if (appearanceContext.getState() == State.ACTIVE) {
            if (!tabPane.isEnabledAt(tabIndex)) {
                tabContext = tabContext.withState(State.DISABLED);
            }  else if (isPressedAt(tabIndex)) {
                tabContext = tabContext.withState(State.PRESSED);
            }
        }
        tabContext = tabContext.withSelected(isSelected);

        return colors.getForeground(tabContext);
    }

    protected boolean isPressedAt(int index) {
        return false;   // not needed, so not implemented
    }

    protected Direction getDirection() {
        switch (tabPane.getTabPlacement()) {
            case SwingConstants.BOTTOM: return Direction.DOWN;
            case SwingConstants.LEFT: return Direction.LEFT;
            case SwingConstants.RIGHT: return Direction.RIGHT;
        }
        return Direction.UP;
    }

    protected static Position getSegmentPosition(boolean first,
                                                 boolean last,
                                                 boolean isLeftToRight) {
        if (first && last) return ONLY;
        if (first) return isLeftToRight ? FIRST : LAST;
        if (last) return isLeftToRight ? LAST : FIRST;
        return MIDDLE;
    }

    protected boolean getSegmentTrailingSeparator(int index, int selectedIndex, boolean isLeftToRight) {
        return true;
    }

    protected boolean getSegmentLeadingSeparator(int index, int selectedIndex, boolean isLeftToRight) {
        return false;
    }

    protected boolean isTabBeforeSelectedTab(int index, int selectedIndex, boolean isLeftToRight) {
        if (index == -2 && visibleTabState.getIndex(0) == selectedIndex) return true;
        int indexBeforeSelectedIndex = isLeftToRight ? selectedIndex - 1 : selectedIndex + 1;
        return index == indexBeforeSelectedIndex ? true : false;
    }

    protected State getState() {
        if (AquaFocusHandler.isActive(tabPane)) {
            if (tabPane.isEnabled()) {
                return State.ACTIVE;
            } else {
                return State.DISABLED;
            }
        } else {
            return State.INACTIVE;
        }
    }

    protected State getTabState(int index, boolean isSelected) {
        if (appearanceContext != null) {
            State state = appearanceContext.getState();
            if (state == State.INACTIVE || state == State.DISABLED || state == State.DISABLED_INACTIVE) {
                return state;
            }
        }

        if (!tabPane.isEnabled()) return State.DISABLED;
        if (isSelected) return State.ACTIVE;    // TBD: is this right?
        if (pressedTab == index) return State.PRESSED;
        return State.ACTIVE;
    }

    protected Insets getContentBorderInsets(int tabPlacement) {
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

    protected Insets getContentDrawingInsets(int tabPlacement) {
        rotateInsets(contentDrawingInsets, currentContentDrawingInsets, tabPlacement);
        return currentContentDrawingInsets;
    }

    protected Icon getIconForTab(int tabIndex) {
        Icon mainIcon = super.getIconForTab(tabIndex);
        if (mainIcon == null) return null;

        int iconHeight = mainIcon.getIconHeight();

        if (iconHeight <= maxIconSize) return mainIcon;
        float ratio = (float)maxIconSize / (float)iconHeight;

        int iconWidth = mainIcon.getIconWidth();
        return new AquaIcon.CachingScalingIcon((int)(iconWidth * ratio), maxIconSize) {
            Image createImage() {
                return AquaIcon.getImageForIcon(mainIcon);
            }
        };
    }

    protected void paintContentBorder(Graphics g, int tabPlacement, int selectedIndex) {
        int width = tabPane.getWidth();
        int height = tabPane.getHeight();
        Insets insets = tabPane.getInsets();

        int x = insets.left;
        int y = insets.top;
        int w = width - insets.right - insets.left;
        int h = height - insets.top - insets.bottom;

        int tabBorderInset = fixedTabHeight / 2;

        // The tabbed pane group border has its own insets, which makes sense except where it overlaps the
        // tabs.

        tabBorderInset -= 5;

        switch (tabPlacement) {
            case TOP:
                y += tabBorderInset;
                h -= tabBorderInset;
                break;
            case BOTTOM:
                h -= tabBorderInset;// - 2;
                break;
            case LEFT:
                x += tabBorderInset;// - 5;
                w -= tabBorderInset;// + 1;
                break;
            case RIGHT:
                w -= tabBorderInset;// + 1;
                break;
        }

//        if (tabPane.isOpaque()) {
//            g.setColor(tabPane.getBackground());
//            g.fillRect(0, 0, width, height);
//        }

        AquaGroupBorder.getTabbedPaneGroupBorder().paintBorder(tabPane, g, x, y, w, h);
    }

    // see paintContentBorder
    protected void repaintContentBorderEdge() {
        int width = tabPane.getWidth();
        int height = tabPane.getHeight();
        Insets insets = tabPane.getInsets();
        int tabPlacement = tabPane.getTabPlacement();
        Insets localContentBorderInsets = getContentBorderInsets(tabPlacement);

        int x = insets.left;
        int y = insets.top;
        int w = width - insets.right - insets.left;
        int h = height - insets.top - insets.bottom;

        switch (tabPlacement) {
            case LEFT:
                x += calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
                w = localContentBorderInsets.left;
                break;
            case RIGHT:
                w = localContentBorderInsets.right;
                break;
            case BOTTOM:
                h = localContentBorderInsets.bottom;
                break;
            case TOP:
            default:
                y += calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
                h = localContentBorderInsets.top;
        }
        tabPane.repaint(x, y, w, h);
    }

    public boolean isTabVisible(int index) {
        if (index == -1 || index == -2) return true;
        for (int i = 0; i < visibleTabState.getTotal(); i++) {
            if (visibleTabState.getIndex(i) == index) return true;
        }
        return false;
    }

    protected SegmentedButtonLayoutConfiguration getTabLayoutConfiguration(int tabIndex) {

        int tabCount = tabPane.getTabCount();

        boolean isLeftToRight = tabPane.getComponentOrientation().isLeftToRight();

        Position position;
        if (tabCount < 2) {
            position = ONLY;
        } else if (tabIndex == 0) {
            position = isLeftToRight ? FIRST : LAST;
        } else if (tabIndex == tabCount-1) {
            position = isLeftToRight ? LAST : FIRST;
        } else {
            position = MIDDLE;
        }

        return new SegmentedButtonLayoutConfiguration(buttonWidget, sizeVariant, position);
    }

    @Override
    protected Insets getTabInsets(int tabPlacement, int tabIndex) {
        int tabCount = tabPane.getTabCount();

        boolean isLeftToRight = tabPane.getComponentOrientation().isLeftToRight();

        Position position;
        if (tabCount < 2) {
            position = ONLY;
        } else if (tabIndex == 0) {
            position = isLeftToRight ? FIRST : LAST;
        } else if (tabIndex == tabCount-1) {
            position = isLeftToRight ? LAST : FIRST;
        } else {
            position = MIDDLE;
        }

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
     * Returns the bounds of the specified tab index.  The bounds are
     * with respect to the JTabbedPane's coordinate space.  If the tab at this
     * index is not currently visible in the UI, then returns null.
     */
    @Override
    public Rectangle getTabBounds(JTabbedPane pane, int i) {
        if (visibleTabState.needsScrollTabs()
                && (visibleTabState.isBefore(i) || visibleTabState.isAfter(i))) {
            return null;
        }
        return super.getTabBounds(pane, i);
    }

    /**
     * Returns the tab index which intersects the specified point
     * in the JTabbedPane's coordinate space.
     */
    public int tabForCoordinate(JTabbedPane pane, int x, int y) {
        ensureCurrentLayout();
        Point p = new Point(x, y);
        if (visibleTabState.needsScrollTabs()) {
            for (int i = 0; i < visibleTabState.getTotal(); i++) {
                int realOffset = visibleTabState.getIndex(i);
                if (rects[realOffset].contains(p.x, p.y)) return realOffset;
            }
            if (visibleTabState.getRightScrollTabRect().contains(p.x, p.y)) return -1; //tabPane.getTabCount();
        } else {
            //old way
            int tabCount = tabPane.getTabCount();
            for (int i = 0; i < tabCount; i++) {
                if (rects[i].contains(p.x, p.y)) return i;
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

            if ("componentOrientation".equals(prop)) {
                comp.revalidate();
                comp.repaint();
                super.propertyChange(e);    // in case a future JDK does something
                return;
            }

            if (!AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(prop)) {
                super.propertyChange(e);
                return;
            }

            comp.repaint();

            // Repaint the "front" tab and the border
            int selected = tabPane.getSelectedIndex();
            Rectangle[] theRects = rects;
            if (selected >= 0 && selected < theRects.length) comp.repaint(theRects[selected]);
            repaintContentBorderEdge();
        }
    }

    protected ChangeListener createChangeListener() {
        return new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                if (!isTabVisible(tabPane.getSelectedIndex())) popupSelectionChanged = true;
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

        boolean isDefaultFocusReceiver(JComponent component) {
            if (isDefaultFocusReceiver == null) {
                Component defaultFocusReceiver = KeyboardFocusManager.getCurrentKeyboardFocusManager().getDefaultFocusTraversalPolicy().getDefaultComponent(getTopLevelFocusCycleRootAncestor(component));
                isDefaultFocusReceiver = new Boolean(defaultFocusReceiver != null && defaultFocusReceiver.equals(component));
            }
            return isDefaultFocusReceiver.booleanValue();
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
        protected int trackingTab = -3;
        protected Timer popupTimer = new Timer(500, this);

        public MouseHandler() {
            popupTimer.setRepeats(false);
        }

        public void mousePressed(MouseEvent e) {
            JTabbedPane pane = (JTabbedPane)e.getSource();
            if (!pane.isEnabled()) {
                trackingTab = -3;
                return;
            }

            Point p = e.getPoint();
            trackingTab = getCurrentTab(pane, p);
            if (trackingTab == -3 || (!shouldRepaintSelectedTabOnMouseDown() && trackingTab == pane.getSelectedIndex())) {
                trackingTab = -3;
                return;
            }

            if (trackingTab < 0 && trackingTab > -3) {
                popupTimer.start();
            }

            pressedTab = trackingTab;
            repaint(pane, pressedTab);
        }

        public void mouseDragged(MouseEvent e) {
            if (trackingTab < -2) return;

            JTabbedPane pane = (JTabbedPane)e.getSource();
            int currentTab = getCurrentTab(pane, e.getPoint());

            if (currentTab != trackingTab) {
                pressedTab = -3;
            } else {
                pressedTab = trackingTab;
            }

            if (trackingTab < 0 && trackingTab > -3) {
                popupTimer.start();
            }

            repaint(pane, trackingTab);
        }

        public void mouseReleased(MouseEvent e) {
            if (trackingTab < -2) return;

            popupTimer.stop();

            JTabbedPane pane = (JTabbedPane)e.getSource();
            Point p = e.getPoint();
            int currentTab = getCurrentTab(pane, p);

            if (trackingTab == -1 && currentTab == -1) {
                pane.setSelectedIndex(pane.getSelectedIndex() + 1);
            }

            if (trackingTab == -2 && currentTab == -2) {
                pane.setSelectedIndex(pane.getSelectedIndex() - 1);
            }

            if (trackingTab >= 0 && currentTab == trackingTab) {
                pane.setSelectedIndex(trackingTab);
            }

            repaint(pane, trackingTab);

            pressedTab = -3;
            trackingTab = -3;
        }

        public void actionPerformed(ActionEvent e) {
            if (trackingTab != pressedTab) {
                return;
            }

            if (trackingTab == -1) {
                showFullPopup(false);
                trackingTab = -3;
            }

            if (trackingTab == -2) {
                showFullPopup(true);
                trackingTab = -3;
            }
        }

        int getCurrentTab(JTabbedPane pane, Point p) {
            int tabIndex = tabForCoordinate(pane, p.x, p.y);
            if (tabIndex >= 0 && pane.isEnabledAt(tabIndex)) return tabIndex;

            if (visibleTabState.needsLeftScrollTab() && visibleTabState.getLeftScrollTabRect().contains(p)) return -2;
            if (visibleTabState.needsRightScrollTab() && visibleTabState.getRightScrollTabRect().contains(p)) return -1;

            return -3;
        }

        void repaint(JTabbedPane pane, int tab) {
            switch (tab) {
                case -1:
                    pane.repaint(visibleTabState.getRightScrollTabRect());
                    return;
                case -2:
                    pane.repaint(visibleTabState.getLeftScrollTabRect());
                    return;
                default:
                    if (trackingTab >= 0) pane.repaint(rects[trackingTab]);
                    return;
            }
        }

        void showFullPopup(boolean firstTab) {
            JPopupMenu popup = new JPopupMenu();

            for (int i = 0; i < tabPane.getTabCount(); i++) {
                if (firstTab ? visibleTabState.isBefore(i) : visibleTabState.isAfter(i)) {
                    popup.add(createMenuItem(i));
                }
            }

            if (firstTab) {
                Rectangle leftScrollTabRect = visibleTabState.getLeftScrollTabRect();
                Dimension popupRect = popup.getPreferredSize();
                popup.show(tabPane, leftScrollTabRect.x - popupRect.width, leftScrollTabRect.y + 7);
            } else {
                Rectangle rightScrollTabRect = visibleTabState.getRightScrollTabRect();
                popup.show(tabPane, rightScrollTabRect.x + rightScrollTabRect.width, rightScrollTabRect.y + 7);
            }

            popup.addPopupMenuListener(new PopupMenuListener() {
                public void popupMenuCanceled(PopupMenuEvent e) { }
                public void popupMenuWillBecomeVisible(PopupMenuEvent e) { }

                public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                    pressedTab = -3;
                    tabPane.repaint(visibleTabState.getLeftScrollTabRect());
                    tabPane.repaint(visibleTabState.getRightScrollTabRect());
                }
            });
        }

        JMenuItem createMenuItem(int i) {
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
                    boolean visible = isTabVisible(fOffset);
                    tabPane.setSelectedIndex(fOffset);
                    if (!visible) {
                        popupSelectionChanged = true;
                        tabPane.invalidate();
                        tabPane.repaint();
                    }
                }
            });

            return menuItem;
        }
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
            if (rects.length <= 0) return;

            visibleTabState.alignRectsRunFor(rects, tabPane.getSize(), tabPlacement, AquaUtils.isLeftToRight(tabPane));
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
            int returnAt;
            int x, y;
            switch (tabPlacement) {
                case SwingConstants.LEFT:
                    maxTabWidth = calculateMaxTabHeight(tabPlacement);
                    x = insets.left + localTabAreaInsets.left;
                    y = insets.top + localTabAreaInsets.top;
                    returnAt = size.height - (insets.bottom + localTabAreaInsets.bottom);
                    break;
                case SwingConstants.RIGHT:
                    maxTabWidth = calculateMaxTabHeight(tabPlacement);
                    x = size.width - insets.right - localTabAreaInsets.right - maxTabWidth - 1;
                    y = insets.top + localTabAreaInsets.top;
                    returnAt = size.height - (insets.bottom + localTabAreaInsets.bottom);
                    break;
                case SwingConstants.BOTTOM:
                    maxTabHeight = calculateMaxTabHeight(tabPlacement);
                    x = insets.left + localTabAreaInsets.left;
                    y = size.height - insets.bottom - localTabAreaInsets.bottom - maxTabHeight;
                    returnAt = size.width - (insets.right + localTabAreaInsets.right);
                    break;
                case SwingConstants.TOP:
                default:
                    maxTabHeight = calculateMaxTabHeight(tabPlacement);
                    x = insets.left + localTabAreaInsets.left;
                    y = insets.top + localTabAreaInsets.top;
                    returnAt = size.width - (insets.right + localTabAreaInsets.right);
                    break;
            }

            tabRunOverlay = getTabRunOverlay(tabPlacement);

            runCount = 0;
            selectedRun = 0;

            if (tabCount == 0) return;

            FontMetrics metrics = getFontMetrics();
            boolean verticalTabRuns = (tabPlacement == SwingConstants.LEFT || tabPlacement == SwingConstants.RIGHT);
            int selectedIndex = tabPane.getSelectedIndex();

            // calculate all the widths
            // if they all fit we are done, if not
            // we have to do the dance of figuring out which ones to show.
            visibleTabState.setNeedsScrollers(false);
            for (int i = 0; i < tabCount; i++) {
                Rectangle rect = rects[i];

                if (verticalTabRuns) {
                    calculateVerticalTabRunRect(rect, metrics, tabPlacement, returnAt, i, x, y);

                    // test if we need to scroll!
                    if (rect.y + rect.height > returnAt) {
                        visibleTabState.setNeedsScrollers(true);
                    }
                } else {
                    calculateHorizontalTabRunRect(rect, metrics, tabPlacement, returnAt, i, x, y);

                    // test if we need to scroll!
                    if (rect.x + rect.width > returnAt) {
                        visibleTabState.setNeedsScrollers(true);
                    }
                }
            }

            visibleTabState.relayoutForScrolling(rects, x, y, returnAt, selectedIndex, verticalTabRuns, tabCount, AquaUtils.isLeftToRight(tabPane));
            // Pad the selected tab so that it appears raised in front

            // if right to left and tab placement on the top or
            // the bottom, flip x positions and adjust by widths
            if (!AquaUtils.isLeftToRight(tabPane) && !verticalTabRuns) {
                int rightMargin = size.width - (insets.right + localTabAreaInsets.right);
                for (int i = 0; i < tabCount; i++) {
                    rects[i].x = rightMargin - rects[i].x - rects[i].width;
                }
            }
        }

        private void calculateHorizontalTabRunRect(Rectangle rect, FontMetrics metrics, int tabPlacement, int returnAt, int i, int x, int y) {
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

        private void calculateVerticalTabRunRect(Rectangle rect, FontMetrics metrics, int tabPlacement, int returnAt, int i, int x, int y) {
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
            if (tabContainer == null) return;

            int placement = tabPane.getTabPlacement();
            Rectangle rect = new Rectangle();
            Point delta = new Point(-tabContainer.getX(), -tabContainer.getY());

            for (int i = 0; i < tabPane.getTabCount(); i++) {
                Component c = getTabComponentAt(i);
                if (c == null) continue;

                getTabBounds(i, rect);
                Insets insets = getTabInsets(tabPane.getTabPlacement(), i);
                boolean isSelected = i == tabPane.getSelectedIndex();

                if (placement == SwingConstants.TOP || placement == SwingConstants.BOTTOM) {
                    rect.x += insets.left + delta.x + getTabLabelShiftX(placement, i, isSelected);
                    rect.y += insets.top + delta.y + getTabLabelShiftY(placement, i, isSelected);// + 1;
                    rect.width -= insets.left + insets.right;
                    rect.height -= insets.top + insets.bottom - 1;
                } else {
                    rect.x += insets.top + delta.x + getTabLabelShiftY(placement, i, isSelected);// + (placement == SwingConstants.LEFT ? 2 : 1);
                    rect.y += insets.left + delta.y + getTabLabelShiftX(placement, i, isSelected);
                    rect.width -= insets.top + insets.bottom - 1;
                    rect.height -= insets.left + insets.right;
                }

                c.setBounds(rect);
            }
        }
    }
}
