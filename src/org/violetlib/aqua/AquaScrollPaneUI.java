/*
 * Changes Copyright (c) 2015-2023 Alan Snyder.
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
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicScrollPaneUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

public class AquaScrollPaneUI extends BasicScrollPaneUI
        implements AquaUtilControlSize.Sizeable, AquaComponentUI, SystemPropertyChangeManager.SystemPropertyChangeListener {

    public static ComponentUI createUI(JComponent x) {
        return new AquaScrollPaneUI();
    }

    public static final String SCROLL_PANE_STYLE_KEY = "JScrollPane.style";
    public static final String SCROLL_PANE_THUMB_STYLE_KEY = "JScrollPane.thumbStyle";
    public static final String SCROLL_PANE_SMOOTH_SCROLLING = "JScrollPane.useSmoothScrolling";

    /**
     * A Boolean valued property managed by this UI for clients that need to react to the use of overlay scroll bars
     */
    public static final String SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY = "AquaOverlayScrollBars";

    public static final String THUMB_STYLE_DARK = "dark";
    public static final String THUMB_STYLE_LIGHT = "light";

    /**
     * the minimum cumulative opposite axis wheel scroll needed to switch to the opposite scroll bar
     */
    public static final int WHEEL_CHANGE_DIRECTION_MINIMUM = 3;

    /**
     * true if native scroll panes relocate the vertical scroll bar in RTL orientation
     */
    public static final boolean isRTLSupported = OSXSystemProperties.doScrollPanesSupportRTL();

    /**
     * enable or disable a gross hack to allow ordinary JScrollPanes to correctly paint overlay scroll bars
     */
    private static final boolean useViewportHack = true;

    protected JScrollBar originalHorizontalScrollBar;
    protected JScrollBar originalVerticalScrollBar;

    /**
     * the cached size variant for the scroll pane
     */
    protected AquaUIPainter.Size size;

    /**
     * true when overlay scroll bars are being used
     */
    protected boolean isOverlayScrollBars;

    /**
     * true when smooth scrolling is enabled
     */
    protected boolean isSmoothScrolling;

    /**
     * manages overlay scroll bars, installed as needed
     */
    protected AquaOverlayScrollPaneController overlayController;

    /**
     * keep track of opposite axis wheel scrolling to decide when to switch to the opposite scroll bar
     */
    protected int contraryScrollCount;

    /**
     * non-null when using the intermediate container hack to properly paint overlay scroll bars
     */
    protected OverlayScrollPaneHack overlayScrollPaneHack;

    /**
     * the layout manager used for legacy scroll bars
     */
    protected LayoutManager legacyLayoutManager;

    protected int defaultSmoothScrollingUnitIncrement = 16;
    protected @Nullable AppearanceContext appearanceContext;

    protected PropertyChangeListener propertyChangeListener;
    protected ComponentListener componentListener;

    protected MouseWheelListener createMouseWheelListener() {
        return new XYMouseWheelHandler();
    }

    @Override
    public void installUI(JComponent x) {
        super.installUI(x);
        legacyLayoutManager = scrollpane.getLayout();
        if (legacyLayoutManager == null) {
            legacyLayoutManager = new ScrollPaneLayout.UIResource();
        }
        setScrollBarStyle(shouldUseOverlayScrollBars());
        scrollpane.putClientProperty(SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY, isOverlayScrollBars);
        configureAppearanceContext(null);
        isSmoothScrolling = computeSmoothScrolling();
    }

    @Override
    protected void uninstallDefaults(JScrollPane c) {
        if (c.getHorizontalScrollBar() instanceof AquaScrollBar && originalHorizontalScrollBar != null) {
            c.setHorizontalScrollBar(originalHorizontalScrollBar);
        }
        if (c.getVerticalScrollBar() instanceof AquaScrollBar && originalVerticalScrollBar != null) {
            c.setVerticalScrollBar(originalVerticalScrollBar);
        }
        super.uninstallDefaults(c);
    }

    @Override
    public void uninstallUI(JComponent c) {
        setScrollBarStyle(false);
        JScrollPane sp = scrollpane;
        super.uninstallUI(c);
        sp.putClientProperty(SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY, null);
    }

    @Override
    protected void installDefaults(JScrollPane scrollpane) {
        Border b = scrollpane.getBorder();
        super.installDefaults(scrollpane);
        if (b instanceof AquaTextComponentBorder) {
            scrollpane.setBorder(b);
        }
        originalHorizontalScrollBar = scrollpane.getHorizontalScrollBar();
        originalVerticalScrollBar = scrollpane.getVerticalScrollBar();
        if (originalVerticalScrollBar instanceof UIResource) {
            scrollpane.setHorizontalScrollBar(new AquaScrollBar(JScrollBar.HORIZONTAL, defaultSmoothScrollingUnitIncrement));
        }
        if (originalVerticalScrollBar instanceof UIResource) {
            scrollpane.setVerticalScrollBar(new AquaScrollBar(JScrollBar.VERTICAL, defaultSmoothScrollingUnitIncrement));
        }
    }

    @Override
    protected void installListeners(JScrollPane c) {
        super.installListeners(c);
        componentListener = new MyComponentListener();
        c.addComponentListener(componentListener);
        propertyChangeListener = new MyPropertyChangeListener();
        c.addPropertyChangeListener(propertyChangeListener);
        OSXSystemProperties.register(c);
        AquaUtilControlSize.addSizePropertyListener(c);
        AppearanceManager.installListeners(c);
    }

    @Override
    protected void uninstallListeners(JComponent c) {
        if (overlayController != null) {
            overlayController.dispose();
            overlayController = null;
        }
        if (overlayScrollPaneHack != null) {
            overlayScrollPaneHack.dispose();
            overlayScrollPaneHack = null;
        }
        AppearanceManager.uninstallListeners(c);
        AquaUtilControlSize.removeSizePropertyListener(c);
        OSXSystemProperties.unregister(c);
        c.removePropertyChangeListener(propertyChangeListener);
        propertyChangeListener = null;
        c.removeComponentListener(componentListener);
        componentListener = null;
        super.uninstallListeners(c);
    }

    @Override
    public void applySizeFor(JComponent c, AquaUIPainter.Size size, boolean isDefaultSize) {
        this.size = size;
        if (c instanceof JScrollPane) {
            JScrollPane sp = (JScrollPane) c;
            updateScrollBar(sp.getHorizontalScrollBar());
            updateScrollBar(sp.getVerticalScrollBar());
        }
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(scrollpane);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        updateThumbStyle();
        scrollpane.repaint();
    }

    protected @NotNull AquaUIPainter.State getState() {
        return AquaUIPainter.State.ACTIVE;
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        if (c.isOpaque()) {
            Color background = AquaColors.getBackground(c, "controlBackground");
            AquaUtils.fillRect(g, c, background, AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {

        // This check is necessary because we are not informed when a new layout manager is installed in the scroll
        // pane. It may change the selected style.
        if (syncLayoutManager()) {
            scrollpane.validate();
        }

        // The following supports the translucent legacy scroll bar used for a sidebar tree
        boolean isSideBar = isSideBar();
        if (isSideBar && !isOverlayScrollBars) {
            setSidebarStyle(scrollpane.getHorizontalScrollBar(), true);
            setSidebarStyle(scrollpane.getVerticalScrollBar(), true);
        } else {
            setSidebarStyle(scrollpane.getHorizontalScrollBar(), false);
            setSidebarStyle(scrollpane.getVerticalScrollBar(), false);
        }

        Border b = c.getBorder();
        if (b instanceof AquaTextComponentBorder) {
            AquaTextComponentBorder tcb = (AquaTextComponentBorder) b;
            tcb.paintBackground(c, g, null);
        }

        super.paint(g, c);

        // If two legacy scroll bars are displayed, the corner must be painted to match.
        if (!isOverlayScrollBars && !isSideBar) {
            JScrollBar vb = scrollpane.getVerticalScrollBar();
            JScrollBar hb = scrollpane.getHorizontalScrollBar();
            if (vb != null && hb != null && vb.isVisible() && hb.isVisible()) {
                int x1 = vb.getX();
                int x2 = vb.getX() + vb.getWidth();
                int y1 = hb.getY();
                int y2 = hb.getY() + hb.getHeight();

                assert appearanceContext != null;
                AquaAppearance appearance = appearanceContext.getAppearance();

                Color trackColor = appearance.getColor("legacyScrollBarTrack");
                Color outerBorderColor = appearance.getColor("legacyScrollBarOuterBorder");

                int w = x2 - x1;
                int h = y2 - y1;

                g.setColor(trackColor);
                g.fillRect(x1, y1, w - 1, h - 1);

                g.setColor(outerBorderColor);
                g.fillRect(x1, y2 - 1, w, 1);

                if (AquaUtils.isLeftToRight(scrollpane) || !isRTLSupported) {
                    g.fillRect(x2 - 1, y1, 1, h - 1);
                    g.fillRect(x1, y1, 1, 1);
                } else {
                    g.fillRect(x1, y1, 1, h - 1);
                    g.fillRect(x2 - 1, y1, 1, 1);
                }
            }
        }
    }

    protected boolean isSideBar() {
        JViewport v = scrollpane.getViewport();
        if (v != null) {
            Component view = SwingUtilities.getUnwrappedView(v);
            if (view instanceof JTree) {
                AquaTreeUI treeUI = AquaUtils.getUI((JTree) view, AquaTreeUI.class);
                if (treeUI != null) {
                    if (treeUI.isSideBar()) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    protected void setSidebarStyle(JScrollBar sb, boolean b) {
        if (sb != null) {
            Object o = sb.getClientProperty(AquaScrollBarUI.INTERNAL_STYLE_CLIENT_PROPERTY_KEY);
            if (o == null) {
                if (b) {
                    sb.putClientProperty(AquaScrollBarUI.INTERNAL_STYLE_CLIENT_PROPERTY_KEY, "sidebar");
                }
            } else if (o.equals("sidebar")) {
                if (!b) {
                    sb.putClientProperty(AquaScrollBarUI.INTERNAL_STYLE_CLIENT_PROPERTY_KEY, null);
                }
            }
        }
    }

    public boolean isOverlayScrollBars() {
        return isOverlayScrollBars;
    }

    public boolean isSmoothScrolling() {
        return isSmoothScrolling;
    }

    protected boolean computeSmoothScrolling() {
        Object p = scrollpane.getClientProperty(SCROLL_PANE_SMOOTH_SCROLLING);
        return !Boolean.FALSE.equals(p);
    }

    @Override
    public void systemPropertyChanged(JComponent c, Object type) {
        if (type.equals(OSXSystemProperties.USER_PREFERENCE_CHANGE_TYPE)) {
            updateStyle();
        }
    }

    protected class MyPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String name = evt.getPropertyName();
            if (name != null) {
                if (name.equals(SCROLL_PANE_STYLE_KEY)) {
                    updateStyle();
                } else if (name.equals(SCROLL_PANE_THUMB_STYLE_KEY)) {
                    updateThumbStyle();
                } else if (name.equals("verticalScrollBar")) {
                    updateVerticalScrollBar();
                } else if (name.equals("horizontalScrollBar")) {
                    updateHorizontalScrollBar();
                } else if (name.equals(SCROLL_PANE_SMOOTH_SCROLLING)) {
                    isSmoothScrolling = computeSmoothScrolling();
                }
            }
        }

        protected void updateVerticalScrollBar() {
            if (overlayController != null) {
                overlayController.setVerticalScrollBar(scrollpane.getVerticalScrollBar());
            }
            updateScrollBar(scrollpane.getVerticalScrollBar());
        }

        protected void updateHorizontalScrollBar() {
            if (overlayController != null) {
                overlayController.setHorizontalScrollBar(scrollpane.getHorizontalScrollBar());
            }
            updateScrollBar(scrollpane.getHorizontalScrollBar());
        }
    }

    /**
     * Set the scroll bar style to the preferred style. This method is used to initialize and change the style.
     */
    protected void updateStyle() {
        boolean b = shouldUseOverlayScrollBars();
        if (b != isOverlayScrollBars) {
            setScrollBarStyle(b);
            scrollpane.putClientProperty(SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY, b);
        }
    }

    protected void updateThumbStyle() {
        // Thumb style used only with overlay scroll bars
        if (isOverlayScrollBars) {
            updateScrollBar(scrollpane.getHorizontalScrollBar());
            updateScrollBar(scrollpane.getVerticalScrollBar());
        }
    }

    protected boolean shouldUseOverlayScrollBars() {
        LayoutManager lm = scrollpane.getLayout();
        if (lm != null && !(lm instanceof UIResource)) {
            return false;
        }

        Object o = scrollpane.getClientProperty(SCROLL_PANE_STYLE_KEY);
        if ("overlay".equals(o)) {
            return true;
        }
        if ("legacy".equals(o)) {
            return false;
        }

        return OSXSystemProperties.useOverlayScrollBars();
    }

    /**
     * Set the scroll bar style. This method is used to initialize, change, and uninstall.
     *
     * @param isOverlay True if and only if overlay scroll bars are to be used.
     */
    protected void setScrollBarStyle(boolean isOverlay) {
        isOverlayScrollBars = isOverlay;
        syncLayoutManager();
        syncOverlayScrollPaneHack();

        if (isOverlayScrollBars) {
            if (overlayController == null) {
                overlayController = new AquaOverlayJScrollPaneController();
            }
        } else {
            if (overlayController != null) {
                overlayController.dispose();
                overlayController = null;
            }
        }

        updateScrollBar(scrollpane.getHorizontalScrollBar());
        updateScrollBar(scrollpane.getVerticalScrollBar());

        scrollpane.revalidate();
        scrollpane.repaint();
    }

    /**
     * Install or remove the overlay scroll pane hack, as needed.
     */
    protected void syncOverlayScrollPaneHack() {
        if (isOverlayScrollBars && useViewportHack && scrollpane.isOptimizedDrawingEnabled()) {
            if (overlayScrollPaneHack == null) {
                overlayScrollPaneHack = new OverlayScrollPaneHack(scrollpane);
            }
        } else if (overlayScrollPaneHack != null) {
            overlayScrollPaneHack.dispose();
            overlayScrollPaneHack = null;
        }
    }

    /**
     * If there is a viewport holder, set its size to the size of the scroll pane.
     */
    public void syncOverlayScrollPaneViewportHolderSize() {
        if (overlayScrollPaneHack != null) {
            overlayScrollPaneHack.syncScrollPaneSize();
        }
    }

    protected void updateScrollBar(JScrollBar bar) {
        // Called to initialize and when the scroll pane size variant or style or thumb style may have changed.
        // An appearance change may change the default thumb style.
        if (bar != null) {
            bar.revalidate();
            bar.repaint();
            bar.putClientProperty(AquaUtilControlSize.CLIENT_PROPERTY_KEY, AquaUtilControlSize.getStringFromSize(size));
            if (isOverlayScrollBars) {
                Object o = scrollpane.getClientProperty(SCROLL_PANE_THUMB_STYLE_KEY);
                if (THUMB_STYLE_LIGHT.equals(o)) {
                    bar.putClientProperty(AquaScrollBarUI.INTERNAL_THUMB_STYLE_CLIENT_PROPERTY_KEY, "overlayLight");
                } else if (THUMB_STYLE_DARK.equals(o)) {
                    bar.putClientProperty(AquaScrollBarUI.INTERNAL_THUMB_STYLE_CLIENT_PROPERTY_KEY, "overlayDark");
                } else {
                    AquaAppearance appearance = AppearanceManager.ensureAppearance(scrollpane);
                    String style = appearance.isDark() ? "overlayLight" : "overlayDark";
                    bar.putClientProperty(AquaScrollBarUI.INTERNAL_THUMB_STYLE_CLIENT_PROPERTY_KEY, style);
                }
            } else {
                bar.putClientProperty(AquaScrollBarUI.INTERNAL_THUMB_STYLE_CLIENT_PROPERTY_KEY, null);
            }
        }
    }

    /**
     * Sync the scroll bar style with the scroll pane layout manager. The layout manager will be replaced if necessary
     * and if possible. If not possible, the scroll bar style will be changed. If a legacy layout manager is currently
     * installed, it is saved for future use. This method is called when the scroll bar style changes. It is also called
     * to check for a new layout manager, as there is no change event that would report such a change.
     *
     * @return true if a layout manager was installed.
     */
    protected boolean syncLayoutManager() {
        LayoutManager m = scrollpane.getLayout();
        if (m instanceof AquaOverlayScrollPaneLayout) {
            // Our overlay scroll bar layout is installed
            if (!isOverlayScrollBars) {
                scrollpane.setLayout(legacyLayoutManager);
                return true;
            }
        } else {
            // The legacy layout manager is installed
            legacyLayoutManager = m;
            if (isOverlayScrollBars) {
                if (m instanceof UIResource) {
                    // OK to replace
                    scrollpane.setLayout(new AquaOverlayScrollPaneLayout());
                    // Visibility of legacy scroll bars is controlled by the layout manager.
                    // Visibility of overlay scroll bars is determined by the controller, but is initially off.
                    hideScrollBar(scrollpane.getHorizontalScrollBar());
                    hideScrollBar(scrollpane.getVerticalScrollBar());
                    return true;
                } else {
                    // Not OK to replace; revert to legacy scroll bars
                    updateStyle();
                }
            }
        }
        return false;
    }

    protected void hideScrollBar(JScrollBar sb) {
        if (sb != null) {
            sb.setVisible(false);
        }
    }

    protected class AquaOverlayJScrollPaneController extends AquaOverlayScrollPaneController {
        public AquaOverlayJScrollPaneController() {
            setVerticalScrollBar(scrollpane.getVerticalScrollBar());
            setHorizontalScrollBar(scrollpane.getHorizontalScrollBar());
        }

        @Override
        protected void reconfigure(JScrollBar sb, String which) {

            if (overlayScrollPaneHack != null) {
                overlayScrollPaneHack.reconfigure(sb, which);
            } else {
                // Ensure that the components are ordered so that the scroll bar will be painted after (on top of) the
                // viewport.
                scrollpane.remove(sb);
                scrollpane.add(sb, which, 0);
            }
        }
    }

    protected class MyComponentListener extends ComponentAdapter {
        @Override
        public void componentShown(ComponentEvent e) {
            syncLayoutManager();
        }
    }

    // This is a grody hack to trick BasicScrollPaneUI into scrolling horizontally
    // when we notice that the shift key is down. This should be removed when AWT/Swing
    // becomes aware of multi-axis scroll wheels.

    // It also supports the OS X behavior that wheel scrolling is enabled even when the corresponding scroll bar is not
    // shown.

    protected class XYMouseWheelHandler extends BasicScrollPaneUI.MouseWheelHandler {
        public void mouseWheelMoved(MouseWheelEvent e) {

            boolean isHorizontalScroll = e.isShiftDown();

            // Ignore motions along an axis that is not scrollable
            if (!isAxisScrollable(isHorizontalScroll)) {
                contraryScrollCount = 0;
                return;
            }

            if (overlayController != null) {
                // Ignore small motions that would flip to the other scroll bar.
                int axis = overlayController.getActiveAxis();
                if (axis == SwingConstants.HORIZONTAL && !isHorizontalScroll
                        || axis == SwingConstants.VERTICAL && isHorizontalScroll) {

                    contraryScrollCount += getUnitsToScroll(e, isSmoothScrolling);
                    if (Math.abs(contraryScrollCount) < WHEEL_CHANGE_DIRECTION_MINIMUM) {
                        return;
                    }
                } else {
                    contraryScrollCount = 0;
                }
            }

            JScrollBar vScrollBar = scrollpane.getVerticalScrollBar();
            boolean vScrollBarWasVisible = vScrollBar != null && vScrollBar.isVisible();
            JScrollBar hScrollBar = scrollpane.getHorizontalScrollBar();
            boolean hScrollBarWasVisible = hScrollBar != null && hScrollBar.isVisible();

            boolean overlayControlInvoked = false;

            if (vScrollBar != null) {
                vScrollBar.setVisible(!isHorizontalScroll);
                if (!isHorizontalScroll && overlayController != null) {
                    overlayController.activateScrollBar(e, true);
                    overlayControlInvoked = true;
                }
            }

            if (hScrollBar != null) {
                hScrollBar.setVisible(isHorizontalScroll);
                if (isHorizontalScroll && overlayController != null) {
                    overlayController.activateScrollBar(e, false);
                    overlayControlInvoked = true;
                }
            }

            basicMouseWheelMoved(e);

            if (!overlayControlInvoked) {
                if (vScrollBar != null) {
                    vScrollBar.setVisible(vScrollBarWasVisible);
                }
                if (hScrollBar != null) {
                    hScrollBar.setVisible(hScrollBarWasVisible);
                }
            }

            // Consume the event even when the scrollBar is invisible
            // see #7124320
            e.consume();
        }

        // Copied from basic, changing unit scroll behavior to implement smooth scrolling
        private void basicMouseWheelMoved(@NotNull MouseWheelEvent e) {
            if (scrollpane.isWheelScrollingEnabled() && e.getWheelRotation() != 0) {
                JScrollBar toScroll = scrollpane.getVerticalScrollBar();
                int direction = e.getWheelRotation() < 0 ? -1 : 1;
                int orientation = SwingConstants.VERTICAL;

                // find which scrollbar to scroll, or return if none
                if (toScroll == null || !toScroll.isVisible() || e.isShiftDown()) {
                    toScroll = scrollpane.getHorizontalScrollBar();
                    if (toScroll == null || !toScroll.isVisible()) {
                        return;
                    }
                    orientation = SwingConstants.HORIZONTAL;
                }

                e.consume();

                if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) {
                    JViewport vp = scrollpane.getViewport();
                    if (vp == null) {
                        return;
                    }
                    Component comp = vp.getView();
                    double units = Math.abs(getUnitsToScroll(e, isSmoothScrolling));

                    // When the scrolling speed is set to maximum, it's possible for a single wheel click to scroll by
                    // more units than will fit in the visible area. This makes it hard/impossible to get to certain
                    // parts of the scrolling Component with the wheel. To make for more accurate low-speed scrolling,
                    // we limit scrolling to the block increment if the wheel was only rotated one click.
                    boolean limitScroll = Math.abs(e.getWheelRotation()) <= 1;

                    // Check if we should use the visibleRect trick
                    Object fastWheelScroll = toScroll.getClientProperty( "JScrollBar.fastWheelScrolling");
                    if (Boolean.TRUE.equals(fastWheelScroll) && comp instanceof Scrollable && !isSmoothScrolling) {
                        scrollByUnitsFast(toScroll, orientation, direction, units, limitScroll);
                    } else {
                        scrollByUnits(toScroll, direction, units, limitScroll);
                    }
                } else if (e.getScrollType() == MouseWheelEvent.WHEEL_BLOCK_SCROLL) {
                    scrollByBlock(toScroll, direction);
                }
            }
        }

        protected boolean isAxisScrollable(boolean isHorizontal) {
            JScrollBar sb = isHorizontal ? scrollpane.getHorizontalScrollBar() : scrollpane.getVerticalScrollBar();
            if (sb == null) {
                return false;
            }
            int valueRange = Math.max(0, sb.getMaximum() - sb.getMinimum());
            int extent = sb.getModel().getExtent();
            return extent > 0 && extent < valueRange;
        }
    }

    private void scrollByUnitsFast(@NotNull JScrollBar toScroll,
                                   int orientation,
                                   int direction,
                                   double units,
                                   boolean limitToBlock)
    {
        // Used for non-smooth wheel scrolling when applicable.

        // 5078454: Under maximum acceleration, we may scroll by many 100s of units in ~1 second.

        // BasicScrollBarUI.scrollByUnits() can bog down the EDT with repaints in this situation. However, the
        // Scrollable interface allows us to pass in an arbitrary visibleRect. This allows us to accurately calculate
        // the total scroll amount, and then update the GUI once. This technique provides much faster accelerated wheel
        // scrolling.

        JViewport vp = scrollpane.getViewport();
        assert vp != null;
        Component comp = vp.getView();
        Scrollable scrollComp = (Scrollable) comp;
        Rectangle viewRect = vp.getViewRect();
        int startingX = viewRect.x;
        boolean leftToRight = ((Component) scrollComp).getComponentOrientation().isLeftToRight();
        int scrollMin = toScroll.getMinimum();
        int scrollMax = toScroll.getMaximum() - toScroll.getModel().getExtent();

        if (limitToBlock) {
            int blockIncr = scrollComp.getScrollableBlockIncrement(viewRect, orientation, direction);
            if (direction < 0) {
                scrollMin = Math.max(scrollMin, toScroll.getValue() - blockIncr);
            } else {
                scrollMax = Math.min(scrollMax, toScroll.getValue() + blockIncr);
            }
        }

        while (units > 0) {
            double thisUnits = Math.min(units, 1);
            int unitIncr = (int)(thisUnits * scrollComp.getScrollableUnitIncrement(viewRect, orientation, direction));
            // Modify the visible rect for the next unit, and check to see if we're at the end already.
            if (orientation == SwingConstants.VERTICAL) {
                if (direction < 0) {
                    viewRect.y -= unitIncr;
                    if (viewRect.y <= scrollMin) {
                        viewRect.y = scrollMin;
                        break;
                    }
                } else {
                    viewRect.y += unitIncr;
                    if (viewRect.y >= scrollMax) {
                        viewRect.y = scrollMax;
                        break;
                    }
                }
            } else {
                // Scroll left
                if ((leftToRight && direction < 0) || (!leftToRight && direction > 0)) {
                    viewRect.x -= unitIncr;
                    if (leftToRight) {
                        if (viewRect.x < scrollMin) {
                            viewRect.x = scrollMin;
                            break;
                        }
                    }
                }
                // Scroll right
                else if ((leftToRight && direction > 0) || (!leftToRight && direction < 0)) {
                    viewRect.x += unitIncr;
                    if (leftToRight) {
                        if (viewRect.x > scrollMax) {
                            viewRect.x = scrollMax;
                            break;
                        }
                    }
                } else {
                    assert false : "Nonsensical component orientation / scroll direction";
                }
            }
            units = units - thisUnits;
        }
        // Set the final view position on the scrollbar
        if (orientation == SwingConstants.VERTICAL) {
            toScroll.setValue(viewRect.y);
        } else {
            if (leftToRight) {
                toScroll.setValue(viewRect.x);
            } else {
                // rightToLeft scrollbars are oriented with minValue on the right and maxValue on the left.
                int newPos = toScroll.getValue() - (viewRect.x - startingX);
                if (newPos < scrollMin) {
                    newPos = scrollMin;
                } else if (newPos > scrollMax) {
                    newPos = scrollMax;
                }
                toScroll.setValue(newPos);
            }
        }
    }

    private void scrollByUnits(JScrollBar scrollbar, int direction, double units, boolean limitToBlock) {
        // Used for wheel scrolling (the general case)
        int limit = -1;

        if (limitToBlock) {
            if (direction < 0) {
                limit = scrollbar.getValue() - scrollbar.getBlockIncrement(direction);
            } else {
                limit = scrollbar.getValue() + scrollbar.getBlockIncrement(direction);
            }
        }

        boolean isFirst = true;
        while (units > 0) {
            double thisUnits = isSmoothScrolling ? units : Math.min(units, 1);
            int delta = (int) (thisUnits * getWheelScrollUnitIncrement(scrollbar, direction));
            if (direction < 0) {
                delta = -delta;
            }

            int oldValue = scrollbar.getValue();
            int newValue = oldValue + delta;

            // Check for overflow.
            if (delta > 0 && newValue < oldValue) {
                newValue = scrollbar.getMaximum();
            } else if (delta < 0 && newValue > oldValue) {
                newValue = scrollbar.getMinimum();
            }
            if (oldValue == newValue) {
                break;
            }

            if (limitToBlock && !isFirst) {
                assert limit != -1;
                if ((direction < 0 && newValue < limit) || (direction > 0 && newValue > limit)) {
                    break;
                }
            }
            scrollbar.setValue(newValue);
            units = units - thisUnits;
            isFirst = false;
        }
    }

    static void scrollByBlock(JScrollBar scrollbar, int direction) {
        // This method is called from BasicScrollPaneUI to implement wheel
        // scrolling, and also from scrollByBlock().
        int oldValue = scrollbar.getValue();
        int blockIncrement = scrollbar.getBlockIncrement(direction);
        int delta = blockIncrement * ((direction > 0) ? +1 : -1);
        int newValue = oldValue + delta;

        // Check for overflow.
        if (delta > 0 && newValue < oldValue) {
            newValue = scrollbar.getMaximum();
        } else if (delta < 0 && newValue > oldValue) {
            newValue = scrollbar.getMinimum();
        }

        scrollbar.setValue(newValue);
    }

    protected int getWheelUnitScrollIncrement(@NotNull Scrollable s,
                                              @NotNull Rectangle viewRect,
                                              int orientation,
                                              int direction) {
        if (isSmoothScrolling) {
            return getSmoothScrollingIncrement((Component) s, direction);
        }
        return s.getScrollableUnitIncrement(viewRect, orientation, direction);
    }

    protected int getWheelScrollUnitIncrement(@NotNull JScrollBar b, int direction) {
        if (isSmoothScrolling) {
            return defaultSmoothScrollingUnitIncrement;
        }
        return b.getUnitIncrement(direction);
    }

    protected int getSmoothScrollingIncrement(@NotNull Component c, int orientation) {
        if (orientation == JScrollBar.VERTICAL) {
            if (c instanceof JList) {
                JList list = (JList) c;
                int rowHeight = list.getFixedCellHeight();
                return rowHeight > 0 ? rowHeight : defaultSmoothScrollingUnitIncrement;
            }
            if (c instanceof JTree) {
                JTree tree = (JTree) c;
                int rowHeight = tree.getRowHeight();
                return rowHeight > 0 ? rowHeight : defaultSmoothScrollingUnitIncrement;
            }
            if (c instanceof JTable) {
                JTable table = (JTable) c;
                int rowHeight = table.getRowHeight();
                return rowHeight > 0 ? rowHeight : defaultSmoothScrollingUnitIncrement;
            }
        }
        return defaultSmoothScrollingUnitIncrement;
    }

    public static double getUnitsToScroll(@NotNull MouseWheelEvent e, boolean isSmooth) {
        return isSmooth ? e.getUnitsToScroll() * e.getPreciseWheelRotation() : e.getUnitsToScroll();
    }
}
