/*
 * Changes Copyright (c) 2015 Alan Snyder.
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicScrollPaneUI;

import org.violetlib.jnr.aqua.AquaUIPainter;

public class AquaScrollPaneUI extends BasicScrollPaneUI implements AquaUtilControlSize.Sizeable {

    public static ComponentUI createUI(final JComponent x) {
        return new AquaScrollPaneUI();
    }

    public static final String SCROLL_PANE_STYLE_KEY = "JScrollPane.style";
    public static final String SCROLL_PANE_THUMB_STYLE_KEY = "JScrollPane.thumbStyle";

    /** A Boolean valued property managed by this UI for clients that need to react to the use of overlay scroll bars */
    public static final String SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY = "AquaOverlayScrollBars";

    public static final String THUMB_STYLE_DARK = "dark";
    public static final String THUMB_STYLE_LIGHT = "light";

    /** the minimum cumulative opposite axis wheel scroll needed to switch to the opposite scroll bar */
    public static final int WHEEL_CHANGE_DIRECTION_MINIMUM = 3;

    /** true if native scroll panes relocate the vertical scroll bar in RTL orientation */
    public static final boolean isRTLSupported = OSXSystemProperties.doScrollPanesSupportRTL();

    /** enable or disable a gross hack to allow ordinary JScrollPanes to correctly paint overlay scroll bars */
    private static final boolean useViewportHack = true;

    /** the cached size variant for the scroll pane */
    protected AquaUIPainter.Size size;

    /** true when overlay scroll bars are being used */
    protected boolean isOverlayScrollBars;

    /** manages overlay scroll bars, installed as needed */
    protected AquaOverlayScrollPaneController overlayController;

    /** keep track of opposite axis wheel scrolling to decide when to switch to the opposite scroll bar */
    protected int contraryScrollCount;

    /** non-null when using the intermediate container hack to properly paint overlay scroll bars */
    protected OverlayScrollPaneHack overlayScrollPaneHack;



    /** the layout manager used for legacy scroll bars */
    protected LayoutManager legacyLayoutManager;

    protected PropertyChangeListener propertyChangeListener;
    protected ChangeListener preferenceChangeListener;
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
    }

    @Override
    public void uninstallUI(JComponent c) {
        setScrollBarStyle(false);
        JScrollPane sp = scrollpane;
        super.uninstallUI(c);
        sp.putClientProperty(SCROLL_PANE_AQUA_OVERLAY_SCROLL_BARS_KEY, null);
    }

    @Override
    protected void installListeners(JScrollPane c) {
        super.installListeners(c);
        componentListener = new MyComponentListener();
        c.addComponentListener(componentListener);
        propertyChangeListener = new MyPropertyChangeListener();
        c.addPropertyChangeListener(propertyChangeListener);
        preferenceChangeListener = new PreferenceChangeListener();
        OSXSystemProperties.addChangeListener(preferenceChangeListener);
        AquaUtilControlSize.addSizePropertyListener(c);
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
        AquaUtilControlSize.removeSizePropertyListener(c);
        OSXSystemProperties.removeChangeListener(preferenceChangeListener);
        preferenceChangeListener = null;
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

        // If using the sidebar style, must erase the background to allow the vibrant background to show.
        if (isSideBar && g instanceof Graphics2D) {
            AquaUtils.fillRect((Graphics2D) g, null, 0, 0, c.getWidth(), c.getHeight());
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
                g.setColor(new Color(250, 250, 250));
                g.fillRect(x1, y1, x2 - x1, y2 - y1);
                g.setColor(new Color(237, 237, 237));
                g.fillRect(x1, y2 - 1, x2 - x1, 1);

                if (AquaUtils.isLeftToRight(scrollpane) || !isRTLSupported) {
                    g.fillRect(x2-1, y1, 1, y2-y1);
                    g.fillRect(x1, y1, 1, 1);
                } else {
                    g.fillRect(x1, y1, 1, y2 - y1);
                    g.fillRect(x2-1, y1, 1, 1);
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
            Object o = sb.getClientProperty(AquaScrollBarUI.STYLE_CLIENT_PROPERTY_KEY);
            if (o == null) {
                if (b) {
                    sb.putClientProperty(AquaScrollBarUI.STYLE_CLIENT_PROPERTY_KEY, "sidebar");
                }
            } else if (o.equals("sidebar")) {
                if (!b) {
                    sb.putClientProperty(AquaScrollBarUI.STYLE_CLIENT_PROPERTY_KEY, null);
                }
            }
        }
    }

    public boolean isOverlayScrollBars() {
        return isOverlayScrollBars;
    }

    protected class PreferenceChangeListener implements ChangeListener {
        @Override
        public void stateChanged(ChangeEvent e) {
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

    protected void updateScrollBar(JScrollBar bar) {
        // Called to initialize and when the scroll pane size variant or style or thumb style may have changed
        if (bar != null) {
            bar.revalidate();
            bar.repaint();
            bar.putClientProperty(AquaUtilControlSize.CLIENT_PROPERTY_KEY, AquaUtilControlSize.getStringFromSize(size));
            if (isOverlayScrollBars) {
                Object o = scrollpane.getClientProperty(SCROLL_PANE_THUMB_STYLE_KEY);
                if (THUMB_STYLE_LIGHT.equals(o)) {
                    bar.putClientProperty(AquaScrollBarUI.THUMB_STYLE_CLIENT_PROPERTY_KEY, "overlayLight");
                } else {
                    bar.putClientProperty(AquaScrollBarUI.THUMB_STYLE_CLIENT_PROPERTY_KEY, "overlayDark");
                }
            } else {
                bar.putClientProperty(AquaScrollBarUI.THUMB_STYLE_CLIENT_PROPERTY_KEY, null);
            }
        }
    }

    /**
     * Sync the scroll bar style with the scroll pane layout manager. The layout manager will be replaced if necessary
     * and if possible. If not possible, the scroll bar style will be changed. If a legacy layout manager is currently
     * installed, it is saved for future use. This method is called when the scroll bar style changes. It is also called
     * to check for a new layout manager, as there is no change event that would report such a change.
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

        @Override
        public void componentResized(ComponentEvent e) {
            if (overlayScrollPaneHack != null) {
                overlayScrollPaneHack.syncScrollPaneSize();
            }
        }
    }

    // This is a grody hack to trick BasicScrollPaneUI into scrolling horizontally
    // when we notice that the shift key is down. This should be removed when AWT/Swing
    // becomes aware of multi-axis scroll wheels.

    // It also supports the OS X behavior that wheel scrolling is enabled even when the corresponding scroll bar is not
    // shown.

    protected class XYMouseWheelHandler extends BasicScrollPaneUI.MouseWheelHandler {
        public void mouseWheelMoved(final MouseWheelEvent e) {

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

                    contraryScrollCount += e.getUnitsToScroll();
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

            super.mouseWheelMoved(e);

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
