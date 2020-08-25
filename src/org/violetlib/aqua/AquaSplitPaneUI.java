/*
 * Changes Copyright (c) 2015-2016 Alan Snyder.
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

import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.SplitPaneDividerLayoutConfiguration;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.*;

public class AquaSplitPaneUI extends BasicSplitPaneUI implements MouseListener, ContainerListener, PropertyChangeListener {
    static final String DIVIDER_PAINTER_KEY = "JSplitPane.dividerPainter";

    public static final String SPLIT_PANE_STYLE_KEY = "JSplitPane.style";
    public static final String QUAQUA_SPLIT_PANE_STYLE_KEY = "Quaqua.SplitPane.style";

    public enum SplitPaneStyle { THIN, THICK, PANE_SPLITTER}

    final AquaUIPainter painter = AquaPainting.create();

    protected static SplitPaneStyle defaultStyle = SplitPaneStyle.THIN;
    protected SplitPaneStyle style = defaultStyle;

    private boolean isReorderingComponents;
    private boolean initialDividerUpdatePerformed;
    private boolean ignoreDividerLocationChange;
    private boolean isInLayout;

    public AquaSplitPaneUI() {
        super();
    }

    public static ComponentUI createUI(final JComponent x) {
        return new AquaSplitPaneUI();
    }

    public BasicSplitPaneDivider createDefaultDivider() {
        return new AquaSplitPaneDivider(this);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        splitPane.setOneTouchExpandable(false);
        SplitPaneStyle specifiedStyle = getClientSpecifiedStyle();
        if (specifiedStyle != null) {
            style = specifiedStyle;
        }
        updateDividerSize();
    }

    protected void installListeners() {
        super.installListeners();
        splitPane.addPropertyChangeListener(DIVIDER_PAINTER_KEY, this);
        splitPane.addPropertyChangeListener(SPLIT_PANE_STYLE_KEY, this);
        splitPane.addPropertyChangeListener(QUAQUA_SPLIT_PANE_STYLE_KEY, this);
        splitPane.addContainerListener(this);
    }

    protected void uninstallListeners() {
        splitPane.removeContainerListener(this);
        splitPane.removePropertyChangeListener(DIVIDER_PAINTER_KEY, this);
        splitPane.removePropertyChangeListener(SPLIT_PANE_STYLE_KEY, this);
        splitPane.removePropertyChangeListener(QUAQUA_SPLIT_PANE_STYLE_KEY, this);
        super.uninstallListeners();
    }

    protected void updateStyle() {
        SplitPaneStyle newStyle = getClientSpecifiedStyle();
        if (newStyle == null) {
            newStyle = defaultStyle;
        }

        if (style != newStyle) {
            style = newStyle;
            updateDividerSize();
            divider.repaint();
        }
    }

    protected void updateDividerSize() {
        int size = getFixedDividerSize();
        if (size > 0) {
            LookAndFeel.installProperty(splitPane, "dividerSize", size);
            divider.setDividerSize(splitPane.getDividerSize());
            dividerSize = divider.getDividerSize();
        }
    }

    public AquaUIPainter.DividerWidget getWidget() {
        switch (style) {
            case PANE_SPLITTER:
                return AquaUIPainter.DividerWidget.PANE_SPLITTER;
            case THICK:
                return AquaUIPainter.DividerWidget.THICK_DIVIDER;
            case THIN:
            default:
                return AquaUIPainter.DividerWidget.THIN_DIVIDER;
        }
    }

    public int getFixedDividerSize() {
        final boolean isVerticalDivider = splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT;
        AquaUIPainter.DividerWidget w = getWidget();
        AquaUIPainter.Orientation o = isVerticalDivider ? AquaUIPainter.Orientation.VERTICAL : AquaUIPainter.Orientation.HORIZONTAL;
        SplitPaneDividerLayoutConfiguration g = new SplitPaneDividerLayoutConfiguration(w, o, 0);
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
        return (int) (isVerticalDivider ? layoutInfo.getFixedVisualWidth() : layoutInfo.getFixedVisualHeight());
    }

    /**
     * The divider extension is the "width" of the transparent area to add to either side of the divider to make it
     * easier to grab when dragging.
     */
    public int getDividerExtension() {
        AquaUIPainter.DividerWidget w = getWidget();
        if (w == AquaUIPainter.DividerWidget.THIN_DIVIDER) {
            return 2;
        }
        return 0;
    }

    /**
     * Return the style specified by the client via client properties.
     * @return the specified style, or null if no valid style was specified.
     */
    protected SplitPaneStyle getClientSpecifiedStyle() {
        Object o = splitPane.getClientProperty(SPLIT_PANE_STYLE_KEY);
        if (o != null) {
            if (o instanceof String) {
                String s = (String) o;
                if (s.equals("thin")) {
                    return SplitPaneStyle.THIN;
                } else if (s.equals("thick")) {
                    return SplitPaneStyle.THICK;
                } else if (s.equals("paneSplitter")) {
                    return SplitPaneStyle.PANE_SPLITTER;
                }
            }
        } else {
            o = splitPane.getClientProperty(QUAQUA_SPLIT_PANE_STYLE_KEY);
            if (o instanceof String) {
                String s = (String) o;
                if (s.equals("bar")) {
                    return SplitPaneStyle.PANE_SPLITTER;
                } else if (s.equals("thumb")) {
                    return SplitPaneStyle.THICK;
                }
            }
        }
        return null;
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, SPLIT_PANE_STYLE_KEY, QUAQUA_SPLIT_PANE_STYLE_KEY);
    }

    @Override
    public boolean isContinuousLayout() {
        // We do not support non-continuous layout. In particular, we do not support a "hidden" divider.
        return true;
    }

    @Override
    public Component getNonContinuousLayoutDivider() {
        // We do not support non-continuous layout. In particular, we do not support a "hidden" divider.
        return null;
    }

    @Override
    public void componentAdded(ContainerEvent e) {
        ensureComponentOrder();
    }

    @Override
    public void componentRemoved(ContainerEvent e) {
    }

    /**
     * Ensure that the divider is on top of the other components. The divider may have transparent areas that make it
     * easier to grab.
     */
    protected void ensureComponentOrder() {
        if (!isReorderingComponents) {
            isReorderingComponents = true;
            try {
                Component leftC = splitPane.getLeftComponent();
                Component rightC = splitPane.getRightComponent();
                int lastLocation = splitPane.getDividerLocation();
                if (leftC != null) {
                    splitPane.setLeftComponent(null);
                }
                if (rightC != null) {
                    splitPane.setRightComponent(null);
                }
                splitPane.remove(divider);
                splitPane.add(divider, JSplitPane.DIVIDER);
                splitPane.setLeftComponent(leftC);
                splitPane.setRightComponent(rightC);
                splitPane.setDividerLocation(lastLocation);
            } finally {
                isReorderingComponents = false;
            }
        }
    }

    /**
     * Resets the layout manager based on orientation and messages it
     * with invalidateLayout to pull in appropriate Components.
     */
    protected void resetLayoutManager() {
        super.resetLayoutManager();

        LayoutManager2 manager = (LayoutManager2) splitPane.getLayout();
        LayoutManager myLayoutManager = new MyLayoutManager(manager);
        splitPane.setLayout(myLayoutManager);
    }

    /**
     * The only thing we need to do in our custom layout manager is extend the "width" of the divider as appropriate.
     */
    protected class MyLayoutManager implements LayoutManager2 {
        LayoutManager2 delegate;

        public MyLayoutManager(LayoutManager2 delegate) {
            this.delegate = delegate;
        }

        @Override
        public void addLayoutComponent(Component comp, Object constraints) {
            delegate.addLayoutComponent(comp, constraints);
        }

        @Override
        public Dimension maximumLayoutSize(Container target) {
            return delegate.maximumLayoutSize(target);
        }

        @Override
        public float getLayoutAlignmentX(Container target) {
            return delegate.getLayoutAlignmentX(target);
        }

        @Override
        public float getLayoutAlignmentY(Container target) {
            return delegate.getLayoutAlignmentY(target);
        }

        @Override
        public void invalidateLayout(Container target) {
            delegate.invalidateLayout(target);
        }

        @Override
        public void addLayoutComponent(String name, Component comp) {
            delegate.addLayoutComponent(name, comp);
        }

        @Override
        public void layoutContainer(Container parent) {
            isInLayout = true;
            delegate.layoutContainer(parent);
            isInLayout = false;
            Dimension containerSize = parent.getSize();
            if (containerSize.height <= 0 || containerSize.width <= 0 ) {
                return;
            }
            updateDividerBounds();
        }

        @Override
        public Dimension minimumLayoutSize(Container parent) {
            return delegate.minimumLayoutSize(parent);
        }

        @Override
        public Dimension preferredLayoutSize(Container parent) {
            return delegate.preferredLayoutSize(parent);
        }

        @Override
        public void removeLayoutComponent(Component comp) {
            delegate.removeLayoutComponent(comp);
        }
    }

    /**
     * Sets the location of the divider to location.
     */
    public void setDividerLocation(JSplitPane jc, int location) {
        if (!ignoreDividerLocationChange) {
            super.setDividerLocation(jc, location);
        }
    }

    /**
     * Assuming that the divider is in its nominal state (no extension has been added), add the extension.
     */
    protected void updateDividerBounds() {
        if (divider != null) {
            initialDividerUpdatePerformed = true;
            int extension = getDividerExtension();
            if (extension > 0) {
                int x = divider.getX();
                int y = divider.getY();
                int w = divider.getWidth();
                int h = divider.getHeight();
                if (splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT) {
                    // the divider is vertical
                    divider.setBounds(x - extension, y, w + 2*extension, h);
                } else {
                    // the divider is horizontal
                    divider.setBounds(x, y-extension, w, h + 2*extension);
                }
            }
        }
    }

    /**
     * Returns the nominal location of the divider, which is the location of the portion of the divider exclusing any
     * divider extension.
     */
    @Override
    public int getDividerLocation(JSplitPane jc) {
        int extension = isInLayout ? 0 : getDividerExtension();
        if (splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT) {
            return divider.getLocation().x + extension;
        } else {
            return divider.getLocation().y + extension;
        }
    }

    @Override
    public void dragDividerTo(int location) {
        super.dragDividerTo(location);
    }

    @Override
    public void finishDraggingTo(int location) {
        super.finishDraggingTo(location);
    }

    @Override
    public void update(Graphics g, JComponent c) {
        if (c.isOpaque()) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent jc) {
        if (!initialDividerUpdatePerformed) {
            updateDividerBounds();
            ignoreDividerLocationChange = true;
            splitPane.setDividerLocation(getDividerLocation(splitPane));
            ignoreDividerLocationChange = false;
        }

        super.paint(g, jc);
    }

    public void mouseClicked(final MouseEvent e) {
        if (e.getClickCount() < 2) return;
        if (!splitPane.isOneTouchExpandable()) return;

        final double resizeWeight = splitPane.getResizeWeight();
        final int minLocation = splitPane.getMinimumDividerLocation();
        final int maxLocation = splitPane.getMaximumDividerLocation();
        final int divLocation = splitPane.getDividerLocation();
        final int lastDivLocation = splitPane.getLastDividerLocation();

        // if we are at the far edge
        if (divLocation >= maxLocation - 5) {
            splitPane.setDividerLocation(lastDivLocation);
            return;
        }

        // if we are at the starting edge
        if (divLocation < minLocation + 5) {
            splitPane.setDividerLocation(lastDivLocation);
            return;
        }

        // otherwise, jump to the most "appropriate" end
        if (resizeWeight > 0.5) {
            splitPane.setDividerLocation(0);
        } else {
            splitPane.setDividerLocation(maxLocation);
        }
    }

    public void mouseEntered(final MouseEvent e) { }
    public void mouseExited(final MouseEvent e) { }
    public void mousePressed(final MouseEvent e) { }
    public void mouseReleased(final MouseEvent e) { }

    public void propertyChange(final PropertyChangeEvent evt) {
        String prop = evt.getPropertyName();
        if (prop != null) {
            if (prop.equals(DIVIDER_PAINTER_KEY)) {
                final Object value = evt.getNewValue();
                if (value instanceof Border) {
                    divider.setBorder((Border)value);
                } else {
                    divider.setBorder(null);
                }
            } else if (isStyleProperty(prop)) {
                updateStyle();
            }
        }
    }
}
