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
import java.awt.geom.Rectangle2D;
import java.beans.*;

import javax.swing.*;
import javax.swing.Timer;
import javax.swing.event.*;
import javax.swing.plaf.*;

import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.Orientation;
import org.violetlib.jnr.aqua.AquaUIPainter.ScrollBarWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.ScrollBarKnobWidget;

public class AquaScrollBarUI extends ScrollBarUI {

    public static final String STYLE_CLIENT_PROPERTY_KEY = "JScrollBar.style";
    public static final String THUMB_STYLE_CLIENT_PROPERTY_KEY = "JScrollBar.thumbStyle";

    // As arrows were removed in 10.7, I'm not sure if it is worth keeping code to support them.

    public enum ScrollBarPart {
        NONE,
        THUMB,
        TRACK_MIN,
        TRACK_MAX,
        ARROW_MIN,
        ARROW_MAX
    }

    private static final int kInitialDelay = 300;
    private static final int kNormalDelay = 100;

    // tracking state
    protected float currentThumbPosition;
    protected boolean fIsDragging;
    protected boolean fIsOverThumb;
    protected boolean fisRolloverDisplay;   // true to use the rollover display style
    protected Timer fScrollTimer;
    protected ScrollListener fScrollListener;
    protected TrackListener fTrackListener;
    protected ScrollBarPart fTrackHighlight = ScrollBarPart.NONE;   // not used since Yosemite
    protected ScrollBarPart fMousePart = ScrollBarPart.NONE;        // not used since Yosemite

    protected JScrollBar fScrollBar;
    protected ModelListener fModelListener;
    protected PropertyChangeListener fPropertyChangeListener;

    protected float alpha = 1;

    protected final AquaUIPainter painter = AquaPainting.create();

    // Create PLAF
    public static ComponentUI createUI(final JComponent c) {
        return new AquaScrollBarUI();
    }

    public AquaScrollBarUI() { }

    @Override
    public void installUI(final JComponent c) {
        fScrollBar = (JScrollBar)c;
        installListeners();
        configureScrollBarColors();
    }

    @Override
    public void uninstallUI(final JComponent c) {
        uninstallListeners();
        fScrollBar = null;
    }

    protected void configureScrollBarColors() {
        LookAndFeel.installColors(fScrollBar, "ScrollBar.background", "ScrollBar.foreground");
    }

    protected TrackListener createTrackListener() {
        return new TrackListener();
    }

    protected ScrollListener createScrollListener() {
        return new ScrollListener();
    }

    protected void installListeners() {
        fTrackListener = createTrackListener();
        fModelListener = createModelListener();
        fPropertyChangeListener = createPropertyChangeListener();
        fScrollBar.addMouseListener(fTrackListener);
        fScrollBar.addMouseMotionListener(fTrackListener);
        fScrollBar.getModel().addChangeListener(fModelListener);
        fScrollBar.addPropertyChangeListener(fPropertyChangeListener);
        fScrollListener = createScrollListener();
        fScrollTimer = new Timer(kNormalDelay, fScrollListener);
        fScrollTimer.setInitialDelay(kInitialDelay); // default InitialDelay?
    }

    protected void uninstallListeners() {
        fScrollTimer.stop();
        fScrollTimer = null;
        fScrollBar.getModel().removeChangeListener(fModelListener);
        fScrollBar.removeMouseListener(fTrackListener);
        fScrollBar.removeMouseMotionListener(fTrackListener);
        fScrollBar.removePropertyChangeListener(fPropertyChangeListener);
    }

    protected PropertyChangeListener createPropertyChangeListener() {
        return new PropertyChangeHandler();
    }

    protected ModelListener createModelListener() {
        return new ModelListener();
    }

    @Override
    public void paint(Graphics g, JComponent c) {

        if (alpha == 0) {
            return;
        }

        Graphics2D gg = null;

        if (alpha < 1) {
            gg = (Graphics2D) g.create();
            gg.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha));
            g = gg;
        }

        // Since 10.7, there are no arrow buttons, just a thumb in a track.

        int width = fScrollBar.getWidth();
        int height = fScrollBar.getHeight();
        ScrollBarConfiguration bg = getConfiguration(false);
        int x = 0;
        int y = 0;

        // If the scroll bar is an overlay scroll bar in non-rollover mode, we may need to offset the rendering to abut
        // the outer edge of the scroll bar

        if (bg.getWidget() == ScrollBarWidget.OVERLAY) {
            int thickness = getScrollBarThickness(bg);
            if (bg.getOrientation() == Orientation.HORIZONTAL) {
                y = height - thickness;
                height = thickness;
            } else if (!AquaScrollPaneUI.isRTLSupported || AquaUtils.isLeftToRight(fScrollBar)) {
                x = width - thickness;
                width = thickness;
            } else {
                width = thickness;
            }
        }

        painter.configure(width, height);
        Painter p = painter.getPainter(bg);

        p.paint(g, x, y);

        if (gg != null) {
            gg.dispose();
        }
    }

    protected ScrollBarConfiguration getConfiguration(boolean isForLayoutSize) {
        ScrollBarWidget sw = getScrollBarWidget(isForLayoutSize);
        ScrollBarKnobWidget kw = getScrollBarKnobWidget(sw);
        Size size = getScrollBarSize();
        State state = getScrollBarState();
        Orientation o = getScrollBarOrientation();

        float thumbPosition = getCurrentThumbPosition();
        float thumbExtent = getCurrentThumbExtent();

        // Do not display a thumb if scrolling is not possible
        if (thumbExtent >= 0.999) {
            kw = ScrollBarKnobWidget.NONE;
        }

        Object styleProperty = fScrollBar.getClientProperty(STYLE_CLIENT_PROPERTY_KEY);
        boolean noTrack = "sidebar".equals(styleProperty);
        return new ScrollBarConfiguration(sw, kw, size, state, o, thumbPosition, thumbExtent, noTrack);
    }

    /**
     * Return the thumb position corresponding to the scroll bar value.
     * The thumb position is zero when the scroll bar value is at its minimum.
     * The thumb position is one when the scroll bar value is at its maximum.
     * If scrolling is not possible (the extent is full or the value range is empty), then the thumb position is zero.
     */
    protected float getCurrentThumbPosition() {
        float valueRange = Math.max(0, fScrollBar.getMaximum() - fScrollBar.getMinimum());
        float scrollingRange = valueRange - fScrollBar.getModel().getExtent();
        currentThumbPosition = scrollingRange <= 0 ? 0 : (fScrollBar.getValue() - fScrollBar.getMinimum()) / scrollingRange;
        return currentThumbPosition;
    }

    /**
     * Return the scroll bar extent as a fraction of the total range of possible scroll bar values.
     * The result is in the range zero to one, inclusive.
     * If the value range is empty, the result is zero.
     */
    protected float getCurrentThumbExtent() {
        float valueRange = Math.max(0, fScrollBar.getMaximum() - fScrollBar.getMinimum());
        return valueRange <= 0 ? 0 : fScrollBar.getModel().getExtent() / valueRange;
    }

    protected int getValueFromThumbPosition(float extendedThumbPosition) {
        int minimum = fScrollBar.getMinimum();
        int maximum = fScrollBar.getMaximum();
        int extent = fScrollBar.getModel().getExtent();
        if (extendedThumbPosition <= 0) {
            return minimum;
        } else if (extendedThumbPosition >= 1) {
            return maximum - extent;
        } else {
            float valueRange = Math.max(0, maximum - minimum);
            float scrollingRange = valueRange - extent;
            return Math.round(minimum + extendedThumbPosition * scrollingRange);
        }
    }

    protected ScrollBarWidget getScrollBarWidget(boolean isForLayoutSize) {
        if (isOverlayStyle()) {
            // Use OVERLAY_ROLLOVER for layout because it is wider
            return fisRolloverDisplay || isForLayoutSize ? ScrollBarWidget.OVERLAY_ROLLOVER : ScrollBarWidget.OVERLAY;
        }

        return ScrollBarWidget.LEGACY;
    }

    protected boolean isOverlayStyle() {
        Object o = fScrollBar.getClientProperty(THUMB_STYLE_CLIENT_PROPERTY_KEY);
        if (o instanceof String) {
            String style = (String) o;
            return style.equals("overlayDark") || style.equals("overlayLight");
        }
        return false;
    }

    public boolean isDragging() {
        return fIsDragging;
    }

    protected ScrollBarKnobWidget getScrollBarKnobWidget(ScrollBarWidget sw) {
        if (sw == ScrollBarWidget.LEGACY) {
            return ScrollBarKnobWidget.DEFAULT; // default is the only option for legacy scroll bars
        }

        Object o = fScrollBar.getClientProperty(THUMB_STYLE_CLIENT_PROPERTY_KEY);
        if (o instanceof String) {
            String style = (String) o;
            if (style.equals("overlayLight")) {
                return ScrollBarKnobWidget.LIGHT;
            }
        }
        return ScrollBarKnobWidget.DARK;
    }

    protected Size getScrollBarSize() {
        Size sz = AquaUtilControlSize.getUserSizeFrom(fScrollBar);
        return sz == Size.REGULAR ? Size.REGULAR : Size.SMALL;
    }

    protected State getScrollBarState() {
        if (!fScrollBar.isEnabled()) {
            return State.DISABLED;
        }

        if (!AquaFocusHandler.isActive(fScrollBar)) {
            return State.INACTIVE;
        }

        if (fIsDragging) {
            return State.PRESSED;
        }

        if (fIsOverThumb) {
            return State.ROLLOVER;
        }

        return State.ACTIVE;
    }

    protected Orientation getScrollBarOrientation() {
        return isHorizontal() ? Orientation.HORIZONTAL : Orientation.VERTICAL;
    }

    /**
     * Return the major axis coordinate of the leading edge of the thumb.
     */
    protected int getThumbTrackPosition() {
        int width = fScrollBar.getWidth();
        int height = fScrollBar.getHeight();
        ScrollBarConfiguration g = getConfiguration(false);
        Rectangle bounds = new Rectangle(0, 0, width, height);
        AquaUILayoutInfo uiLayout = painter.getLayoutInfo();
        Rectangle2D thumbBounds = uiLayout.getScrollBarThumbBounds(bounds, g);
        return (int) (g.getOrientation() == Orientation.VERTICAL ? thumbBounds.getY() : thumbBounds.getX());
    }

    /**
     * Return the scroll part corresponding to a component location. The result is based only on the coordinate of the
     * primary axis. Locations outside the bounds of the scroll bar are not given special treatment.
     */
    protected ScrollBarPart getPartHit(final int x, final int y) {
        float value = getExtendedTrackPosition(x, y);
        if (value < 0) {
            return ScrollBarPart.TRACK_MIN;
        } else if (value > 1) {
            return ScrollBarPart.TRACK_MAX;
        } else {
            int width = fScrollBar.getWidth();
            int height = fScrollBar.getHeight();
            painter.configure(width, height);
            ScrollBarConfiguration g = getConfiguration(false);
            boolean isHorizontal = isHorizontal();
            int c = isHorizontal ? x : y;
            ScrollBarThumbConfiguration tg = new ScrollBarThumbConfiguration(g, c);
            int pos = painter.getScrollBarThumbHit(tg);
            switch (pos) {
                case -1:        return ScrollBarPart.TRACK_MIN;
                case 0:         return ScrollBarPart.THUMB;
                case 1:         return ScrollBarPart.TRACK_MAX;
                default:        return ScrollBarPart.NONE;
            }
        }
    }

    /**
     * Return the position relative to the track length corresponding to a component location. If the location is within
     * the track, the returned value is between 0 and 1 (inclusive). If the location is on the low side of the track, a
     * negative value is returned. If the location is on the high side of the track, a value greater than 1 is returned.
     */
    private float getExtendedTrackPosition(final int x, final int y) {
        int width = fScrollBar.getWidth();
        int height = fScrollBar.getHeight();
        boolean isHorizontal = isHorizontal();
        int c = isHorizontal ? x : y;

        ScrollBarWidget sw = getScrollBarWidget(false);
        Size size = getScrollBarSize();
        Orientation o = getScrollBarOrientation();
        float extent = getCurrentThumbExtent();
        ScrollBarThumbLayoutConfiguration g = new ScrollBarThumbLayoutConfiguration(sw, size, o, extent, c);
        painter.configure(width, height);
        return painter.getScrollBarThumbPosition(g, false);
    }

    // Layout Methods
    // Layout is controlled by the user in the Appearance Control Panel
    // Theme will redraw correctly for the current layout
    public void layoutContainer(final Container fScrollBarContainer) {
        fScrollBar.repaint();
        fScrollBar.revalidate();
    }

    protected Rectangle getTrackBounds() {
        return new Rectangle(0, 0, fScrollBar.getWidth(), fScrollBar.getHeight());
    }

    protected void setOverThumb(boolean b) {
        if (b != fIsOverThumb) {
            fIsOverThumb = b;
            fScrollBar.repaint();
        }
    }

    protected void startTimer(final boolean initial) {
        fScrollTimer.setInitialDelay(initial ? kInitialDelay : kNormalDelay); // default InitialDelay?
        fScrollTimer.start();
    }

    protected void scrollByBlock(final int direction) {
        synchronized(fScrollBar) {
            final int oldValue = fScrollBar.getValue();
            final int blockIncrement = fScrollBar.getBlockIncrement(direction);
            final int delta = blockIncrement * ((direction > 0) ? +1 : -1);

            fScrollBar.setValue(oldValue + delta);
            fTrackHighlight = direction > 0 ? ScrollBarPart.TRACK_MAX : ScrollBarPart.TRACK_MIN;
            fScrollBar.repaint();
            fScrollListener.setDirection(direction);
            fScrollListener.setScrollByBlock(true);
        }
    }

    protected void scrollByUnit(final int direction) {
        synchronized(fScrollBar) {
            int delta = fScrollBar.getUnitIncrement(direction);
            if (direction <= 0) delta = -delta;

            fScrollBar.setValue(delta + fScrollBar.getValue());
            fScrollBar.repaint();
            fScrollListener.setDirection(direction);
            fScrollListener.setScrollByBlock(false);
        }
    }

    protected class PropertyChangeHandler implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent e) {
            final String propertyName = e.getPropertyName();

            if ("model".equals(propertyName)) {
                final BoundedRangeModel oldModel = (BoundedRangeModel)e.getOldValue();
                final BoundedRangeModel newModel = (BoundedRangeModel)e.getNewValue();
                oldModel.removeChangeListener(fModelListener);
                newModel.addChangeListener(fModelListener);
                fScrollBar.repaint();
                fScrollBar.revalidate();
            } else if (AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(propertyName)) {
                fScrollBar.repaint();
            }
        }
    }

    protected class ModelListener implements ChangeListener {
        public void stateChanged(final ChangeEvent e) {
            layoutContainer(fScrollBar);
        }
    }

    // Track mouse motion
    protected class TrackListener extends MouseAdapter implements MouseMotionListener {
        protected transient int fCurrentMouseX, fCurrentMouseY;
        protected transient boolean fInArrows;              // are we currently tracking arrows?
        protected transient boolean fStillInArrow = false;  // Whether mouse is in an arrow during arrow tracking
        protected transient boolean fStillInTrack = false;  // Whether mouse is in the track during pageup/down tracking
        protected transient int fFirstThumbTrackPosition;   // Major axis coordinate of leading edge of thumb at start of drag
        protected transient int fHitTrackPosition;          // Major axis coordinate of mouse pointer at start of drag

        public void mouseReleased(final MouseEvent e) {
            if (!fScrollBar.isEnabled()) return;
            if (fInArrows) {
                mouseReleasedInArrows(e);
            } else {
                mouseReleasedInTrack(e);
            }

            fInArrows = false;
            fStillInArrow = false;
            fStillInTrack = false;

            fScrollBar.repaint();
            fScrollBar.revalidate();
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            fisRolloverDisplay = true;
            updateOverThumb(e);
            fScrollBar.repaint();
        }

        @Override
        public void mouseExited(MouseEvent e) {
            setOverThumb(false);
            fScrollBar.repaint();
        }

        @Override
        public void mouseMoved(MouseEvent e) {
            updateOverThumb(e);
        }

        protected void updateOverThumb(MouseEvent e) {
            if (fScrollBar.isEnabled()) {
                ScrollBarPart part = getPartHit(e.getX(), e.getY());
                setOverThumb(part == ScrollBarPart.THUMB);
            } else {
                setOverThumb(false);
            }
        }

        public void mousePressed(final MouseEvent e) {
            if (!fScrollBar.isEnabled()) return;

            final ScrollBarPart part = getPartHit(e.getX(), e.getY());
            fInArrows = HitUtil.isArrow(part);
            if (fInArrows) {
                mousePressedInArrows(e, part);
            } else {
                if (part == ScrollBarPart.NONE) {
                    fTrackHighlight = ScrollBarPart.NONE;
                } else {
                    mousePressedInTrack(e, part);
                }
            }
        }

        public void mouseDragged(final MouseEvent e) {
            if (!fScrollBar.isEnabled()) return;

            if (fInArrows) {
                mouseDraggedInArrows(e);
            } else if (fIsDragging) {
                mouseDraggedInTrack(e);
            } else {
                // In pageup/down zones

                // check that thumb has not been scrolled under the mouse cursor
                final ScrollBarPart previousPart = getPartHit(fCurrentMouseX, fCurrentMouseY);
                if (!HitUtil.isTrack(previousPart)) {
                    fStillInTrack = false;
                }

                fCurrentMouseX = e.getX();
                fCurrentMouseY = e.getY();

                final ScrollBarPart part = getPartHit(e.getX(), e.getY());
                final boolean temp = HitUtil.isTrack(part);
                if (temp == fStillInTrack) return;

                fStillInTrack = temp;
                if (!fStillInTrack) {
                    fScrollTimer.stop();
                } else {
                    fScrollListener.actionPerformed(new ActionEvent(fScrollTimer, 0, ""));
                    startTimer(false);
                }
            }
        }

        /**
         * Map a mouse coordinate to an extended thumb position.
         * @param x The X mouse coordinate.
         * @param y The Y mouse coordinate.
         * @param useExtent If true, the coordinate is interpreted as the location of the leading edge of the thumb,
         *                  for the purpose of repositioning the thumb. If false, the coordinate is interpreted as
         *                  a fraction of the full track, for the purpose of scroll-to-here.
         * @return the extended thumb position (values outside the 0 to 1 range indicate that the coordinate is
         *         outside the normal range)
         */
        int getDragValue(int x, int y, boolean useExtent) {

            final float valueRange = Math.max(0, fScrollBar.getMaximum() - fScrollBar.getMinimum());
            final float scrollingRange = valueRange - fScrollBar.getModel().getExtent();
            if (scrollingRange <= 0) {
                return fScrollBar.getMinimum();
            }

            ScrollBarWidget sw = getScrollBarWidget(false);
            Size size = getScrollBarSize();
            float extent = getCurrentThumbExtent();
            final Orientation o = getScrollBarOrientation();
            final boolean isHoriz = o == Orientation.HORIZONTAL;
            final int coordinate = isHoriz ? x : y;

            int deltaTrackPosition = coordinate - fHitTrackPosition;
            int newThumbTrackPosition = fFirstThumbTrackPosition + deltaTrackPosition;
            painter.configure(fScrollBar.getWidth(), fScrollBar.getHeight());
            ScrollBarThumbLayoutConfiguration g = new ScrollBarThumbLayoutConfiguration(sw, size, o, extent, newThumbTrackPosition);
            float newExtendedThumbPosition = painter.getScrollBarThumbPosition(g, useExtent);
            return getValueFromThumbPosition(newExtendedThumbPosition);
        }

        /**
         * Arrow Listeners
         */
        // Because we are handling both mousePressed and Actions
        // we need to make sure we don't fire under both conditions.
        // (keyfocus on scrollbars causes action without mousePress
        void mousePressedInArrows(final MouseEvent e, final ScrollBarPart part) {
            final int direction = HitUtil.isIncrement(part) ? 1 : -1;

            fStillInArrow = true;
            scrollByUnit(direction);
            fScrollTimer.stop();
            fScrollListener.setDirection(direction);
            fScrollListener.setScrollByBlock(false);

            fMousePart = part;
            startTimer(true);
        }

        void mouseReleasedInArrows(final MouseEvent e) {
            fScrollTimer.stop();
            fMousePart = ScrollBarPart.NONE;
            fScrollBar.setValueIsAdjusting(false);
        }

        void mouseDraggedInArrows(final MouseEvent e) {
            final ScrollBarPart whichPart = getPartHit(e.getX(), e.getY());

            if ((fMousePart == whichPart) && fStillInArrow) return; // Nothing has changed, so return

            if (fMousePart != whichPart && !HitUtil.isArrow(whichPart)) {
                // The mouse is not over the arrow we mouse pressed in, so stop the timer and mark as
                // not being in the arrow
                fScrollTimer.stop();
                fStillInArrow = false;
                fScrollBar.repaint();
            } else {
                // We are in the arrow we mouse pressed down in originally, but the timer was stopped so we need
                // to start it up again.
                fMousePart = whichPart;
                fScrollListener.setDirection(HitUtil.isIncrement(whichPart) ? 1 : -1);
                fStillInArrow = true;
                fScrollListener.actionPerformed(new ActionEvent(fScrollTimer, 0, ""));
                startTimer(false);
            }

            fScrollBar.repaint();
        }

        void mouseReleasedInTrack(final MouseEvent e) {
            if (fTrackHighlight != ScrollBarPart.NONE) {
                fScrollBar.repaint();
            }

            fTrackHighlight = ScrollBarPart.NONE;
            fIsDragging = false;
            fScrollTimer.stop();
            fScrollBar.setValueIsAdjusting(false);
        }

        /**
         * Adjust the fScrollBars value based on the result of hitTestTrack
         */
        void mousePressedInTrack(final MouseEvent e, final ScrollBarPart part) {
            fScrollBar.setValueIsAdjusting(true);
            boolean isHorizontal = isHorizontal();

            // If option-click, toggle scroll-to-here
            boolean shouldScrollToHere = (part != ScrollBarPart.THUMB) && OSXSystemProperties.isScrollToClick();
            if (e.isAltDown()) shouldScrollToHere = !shouldScrollToHere;

            // pretend the mouse was dragged from a point in the current thumb to the current mouse point in one big jump
            if (shouldScrollToHere) {
                fFirstThumbTrackPosition = getThumbTrackPosition();
                fHitTrackPosition = isHorizontal ? e.getX() : e.getY();
                moveToMouse(e, false);

                // OK, now we're in the thumb - any subsequent dragging should move it
                fTrackHighlight = ScrollBarPart.THUMB;
                fIsDragging = true;
                return;
            }

            fCurrentMouseX = e.getX();
            fCurrentMouseY = e.getY();

            int direction = 0;
            if (part == ScrollBarPart.TRACK_MIN) {
                fTrackHighlight = ScrollBarPart.TRACK_MIN;
                direction = -1;
            } else if (part == ScrollBarPart.TRACK_MAX) {
                fTrackHighlight = ScrollBarPart.TRACK_MAX;
                direction = 1;
            } else {
                fFirstThumbTrackPosition = getThumbTrackPosition();
                fHitTrackPosition = isHorizontal ? e.getX() : e.getY();
                fTrackHighlight = ScrollBarPart.THUMB;
                fIsDragging = true;
                return;
            }

            fIsDragging = false;
            fStillInTrack = true;

            scrollByBlock(direction);
            // Check the new location of the thumb
            // stop scrolling if the thumb is under the mouse??

            final ScrollBarPart newPart = getPartHit(fCurrentMouseX, fCurrentMouseY);
            if (newPart == ScrollBarPart.TRACK_MIN || newPart == ScrollBarPart.TRACK_MAX) {
                fScrollTimer.stop();
                fScrollListener.setDirection(((newPart == ScrollBarPart.TRACK_MAX) ? 1 : -1));
                fScrollListener.setScrollByBlock(true);
                startTimer(true);
            }
        }

        /**
         * Set the models value to the position of the top/left
         * of the thumb relative to the origin of the track.
         */
        void mouseDraggedInTrack(final MouseEvent e) {
            moveToMouse(e, true);
        }

        // For normal mouse dragging or click-to-here
        // fCurrentMouseX, fCurrentMouseY, and fFirstThumbPosition must be set
        void moveToMouse(MouseEvent e, boolean useExtent) {
            fCurrentMouseX = e.getX();
            fCurrentMouseY = e.getY();

            final int oldValue = fScrollBar.getValue();
            final int newValue = getDragValue(fCurrentMouseX, fCurrentMouseY, useExtent);
            if (newValue == oldValue) return;

            fScrollBar.setValue(newValue);
            final Rectangle dirtyRect = getTrackBounds();
            fScrollBar.repaint(dirtyRect.x, dirtyRect.y, dirtyRect.width, dirtyRect.height);
        }
    }

    /**
     * Listener for scrolling events initiated in the ScrollPane.
     */
    protected class ScrollListener implements ActionListener {
        boolean fUseBlockIncrement;
        int fDirection = 1;

        void setDirection(final int direction) {
            this.fDirection = direction;
        }

        void setScrollByBlock(final boolean block) {
            this.fUseBlockIncrement = block;
        }

        public void actionPerformed(final ActionEvent e) {
            if (fUseBlockIncrement) {
                ScrollBarPart newPart = getPartHit(fTrackListener.fCurrentMouseX, fTrackListener.fCurrentMouseY);

                if (newPart == ScrollBarPart.TRACK_MIN || newPart == ScrollBarPart.TRACK_MAX) {
                    final int newDirection = (newPart == ScrollBarPart.TRACK_MAX ? 1 : -1);
                    if (fDirection != newDirection) {
                        fDirection = newDirection;
                    }
                }

                scrollByBlock(fDirection);
                newPart = getPartHit(fTrackListener.fCurrentMouseX, fTrackListener.fCurrentMouseY);

                if (newPart == ScrollBarPart.THUMB) {
                    ((Timer)e.getSource()).stop();
                }
            } else {
                scrollByUnit(fDirection);
            }

            if (fDirection > 0 && fScrollBar.getValue() + fScrollBar.getVisibleAmount() >= fScrollBar.getMaximum()) {
                ((Timer)e.getSource()).stop();
            } else if (fDirection < 0 && fScrollBar.getValue() <= fScrollBar.getMinimum()) {
                ((Timer)e.getSource()).stop();
            }
        }
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        int t = getScrollBarThickness();
        return isHorizontal() ? new Dimension(96, t) : new Dimension(t, 96);
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        int t = getScrollBarThickness();
        return isHorizontal() ? new Dimension(54, t) : new Dimension(t, 54);
    }

    @Override
    public Dimension getMaximumSize(final JComponent c) {
        int t = getScrollBarThickness();
        return isHorizontal() ? new Dimension(100000, t) : new Dimension(t, 100000);
    }

    public int getScrollBarThickness() {
        LayoutConfiguration g = getConfiguration(true);
        return getScrollBarThickness(g);
    }

    protected int getScrollBarThickness(LayoutConfiguration g) {
        AquaUILayoutInfo uiLayout = painter.getLayoutInfo();
        LayoutInfo layoutInfo = uiLayout.getLayoutInfo(g);
        float f = isHorizontal() ? layoutInfo.getMinimumVisualHeight() : layoutInfo.getMinimumVisualWidth();
        return (int) Math.ceil(f);
    }

    public void setRolloverDisplayState(boolean b) {
        if (fisRolloverDisplay != b) {
            fisRolloverDisplay = b;
            fScrollBar.repaint();
        }
        fIsOverThumb = false;   // may be wrong but does not matter for overlay scroll bars
    }

    public void setAlpha(float a) {
        if (a != alpha) {
            alpha = a;
            fScrollBar.repaint();
        }
    }

    protected boolean isHorizontal() {
        return fScrollBar.getOrientation() == Adjustable.HORIZONTAL;
    }

    static class HitUtil {
        static boolean isIncrement(final ScrollBarPart hit) {
            return hit == ScrollBarPart.ARROW_MAX;
        }

        static boolean isDecrement(final ScrollBarPart hit) {
            return hit == ScrollBarPart.ARROW_MIN;
        }

        static boolean isArrow(final ScrollBarPart hit) {
            return isIncrement(hit) || isDecrement(hit);
        }

        static boolean isTrack(final ScrollBarPart hit) {
            return (hit == ScrollBarPart.TRACK_MAX) || (hit == ScrollBarPart.TRACK_MIN);
        }
    }
}
