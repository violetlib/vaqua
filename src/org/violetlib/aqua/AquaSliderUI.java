/*
 * Changes copyright (c) 2015 Alan Snyder.
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
import java.awt.event.MouseEvent;
import java.util.Dictionary;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSliderUI;

import org.violetlib.geom.ExpandableOutline;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.SliderWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.TickMarkPosition;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.SliderPainter;
import org.violetlib.jnr.aqua.SliderConfiguration;
import org.violetlib.jnr.aqua.SliderLayoutConfiguration;

public class AquaSliderUI extends BasicSliderUI implements AquaUtilControlSize.Sizeable, FocusRingOutlineProvider {

    final AquaUIPainter painter = AquaPainting.create();

    protected Size sizeVariant = Size.REGULAR;
    protected int fixedWidth;
    protected int fixedHeight;

    protected int leftTrackBuffer;  // for left and top
    protected int rightTrackBuffer; // for right and bottom

    protected Color tickColor;

    private boolean oldRequestFocusEnabled;

    protected transient boolean fIsDragging = false;

    // Create PLAF
    public static ComponentUI createUI(final JComponent c) {
        return new AquaSliderUI((JSlider)c);
    }

    public AquaSliderUI(final JSlider b) {
        super(b);
    }

    public void installUI(final JComponent c) {
        super.installUI(c);

        LookAndFeel.installProperty(slider, "opaque", Boolean.FALSE);
        tickColor = UIManager.getColor("Slider.tickColor");
    }

    protected BasicSliderUI.TrackListener createTrackListener(final JSlider s) {
        return new TrackListener();
    }

    @Override
    protected void installDefaults(JSlider slider) {
        super.installDefaults(slider);
        oldRequestFocusEnabled = slider.isRequestFocusEnabled();
        slider.setRequestFocusEnabled(false);
    }

    @Override
    protected void uninstallDefaults(JSlider slider) {
        slider.setRequestFocusEnabled(oldRequestFocusEnabled);
        super.uninstallDefaults(slider);
    }

    protected void installListeners(final JSlider s) {
        super.installListeners(s);
        AquaFocusHandler.install(s);
        AquaUtilControlSize.addSizePropertyListener(s);
        AquaFullKeyboardFocusableHandler.addListener(s);
    }

    protected void uninstallListeners(final JSlider s) {
        AquaUtilControlSize.removeSizePropertyListener(s);
        AquaFocusHandler.uninstall(s);
        super.uninstallListeners(s);
        AquaFullKeyboardFocusableHandler.removeListener(s);
    }

    @Override
    public void applySizeFor(JComponent c, Size size, boolean isDefaultSize) {
        sizeVariant = size;
        // Avoid setting the font before the component has been fully configured
        if (focusRect != null) {
            if (size == Size.SMALL) {
                size = Size.MINI;
            }
            AquaUtilControlSize.configureFontFromSize(c, size);
        }
    }

    protected void updateFixedDimension() {
        int valueRange = (slider.getMaximum() - slider.getMinimum());

        int tickCount = 0;
        if (slider.getPaintTicks()) {
            int tickSpacing = getTickSpacing();
            if (tickSpacing > 0) {
                tickCount = valueRange / tickSpacing;
            }
        }

        boolean isHorizontal = slider.getOrientation() == SwingConstants.HORIZONTAL;
        TickMarkPosition tickPosition = isHorizontal ? TickMarkPosition.BELOW : TickMarkPosition.RIGHT;
        SliderWidget widget = getSliderWidget();

        SliderLayoutConfiguration sg = new SliderLayoutConfiguration(widget, sizeVariant, tickCount, tickPosition);
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(sg);
        fixedWidth = (int) Math.ceil(layoutInfo.getFixedVisualWidth());
        fixedHeight = (int) Math.ceil(layoutInfo.getFixedVisualHeight());

        // the following should be unnecessary

        if (isHorizontal) {
            if (fixedHeight == 0) {
                fixedHeight = 23;
            }
        } else {
            if (fixedWidth == 0) {
                fixedWidth = 23;
            }
        }
    }

    protected SliderWidget getSliderWidget() {
        // TBD: support circular

        boolean isHorizontal = slider.getOrientation() == SwingConstants.HORIZONTAL;
        boolean isInverted = drawInverted();

        return isHorizontal ?
                isInverted ? SliderWidget.SLIDER_HORIZONTAL_RIGHT_TO_LEFT : SliderWidget.SLIDER_HORIZONTAL
                : isInverted ? SliderWidget.SLIDER_UPSIDE_DOWN : SliderWidget.SLIDER_VERTICAL;
    }

    @Override
    public Shape getFocusRingOutline(JComponent c) {
        // The focus ring goes around the knob. The outline thus depends upon the thumb position as well as,
        // potentially, the slider style and the component orientation, and whether tick marks are painted.

        SliderPainter p = getConfiguredPainter();
        Shape s = p.getThumbOutline();
        return ExpandableOutline.createTranslatedShape(s, trackRect.x, trackRect.y);
    }

    // Paint Methods
    public void paint(final Graphics g, final JComponent c) {
        // We have to override paint of BasicSliderUI because we need slight differences.
        // We don't paint focus the same way - it is part of the thumb.
        // We also need to repaint the whole track when the thumb moves.
        recalculateIfInsetsChanged();
        final Rectangle clip = g.getClipBounds();

        // We cannot implement the clip optimizations

        if (slider.getPaintTrack()) {
            // This is needed for when this is used as a renderer. It is the same as BasicSliderUI.java
            // and is missing from our reimplementation.
            //
            // <rdar://problem/3721898> JSlider in TreeCellRenderer component not painted properly.
            //
            final boolean trackIntersectsClip = clip.intersects(trackRect);
            if (!trackIntersectsClip) {
                calculateGeometry();
            }
        }

        Painter p = getConfiguredPainter();
        p.paint(g, trackRect.x, trackRect.y);

        if (slider.getPaintLabels() && clip.intersects(labelRect)) {
            paintLabels(g);
        }
    }

    protected SliderPainter getConfiguredPainter() {
        painter.configure(trackRect.width, trackRect.height);
        SliderConfiguration sg = getConfiguration();
        return (SliderPainter) painter.getPainter(sg);
    }

    protected SliderConfiguration getConfiguration() {
        final State state = getState();

        int valueRange = (slider.getMaximum() - slider.getMinimum());

        int tickCount = 0;
        if (slider.getPaintTicks() && valueRange > 0) {
            int tickSpacing = getTickSpacing();
            if (tickSpacing > 0) {
                tickCount = valueRange / tickSpacing + 1;
            }
        }

        double thumbPosition = valueRange > 0 ? slider.getValue() / ((double) valueRange) : 0;
        SliderWidget widget = getSliderWidget();
        TickMarkPosition tickPosition = getTickMarkPosition();
        boolean isFocused = slider.hasFocus();

        // TBD: support tick color

         return new SliderConfiguration(widget, sizeVariant, state, isFocused, thumbPosition, tickCount, tickPosition);
    }

    TickMarkPosition getTickMarkPosition() {

        // TBD: should be an option for tick mark position

        boolean isLeftToRight = slider.getComponentOrientation().isLeftToRight();
        boolean isHorizontal = slider.getOrientation() == SwingConstants.HORIZONTAL;

        if (isHorizontal) {
            return TickMarkPosition.BELOW;
        } else {
            return isLeftToRight ? TickMarkPosition.RIGHT : TickMarkPosition.LEFT;
        }
    }

    State getState() {
        if (!slider.isEnabled()) {
            return AquaFocusHandler.isActive(slider) ? State.DISABLED : State.DISABLED_INACTIVE;
        }

        if (fIsDragging) {
            return State.PRESSED;
        }

        if (!AquaFocusHandler.isActive(slider)) {
            return State.INACTIVE;
        }

        return State.ACTIVE;
    }

    public void paintTrack(Graphics g) {
    }

    public void paintThumb(Graphics g) {
    }

    public void paintTicks(final Graphics g) {
    }

    // Layout Methods

    @Override
    protected void calculateGeometry() {
        updateFixedDimension();
        super.calculateGeometry();
        AquaFocusRingManager.focusRingOutlineChanged(slider);
    }

    @Override
    protected int getTickLength() {
        // Because the native renderer is drawing tick marks, the tick rect can have zero thickness.
        return 0;
    }

    @Override
    protected void calculateThumbLocation() {
        super.calculateThumbLocation(); // needed for its implementation of snapToTicks
        SliderPainter p = getConfiguredPainter();
        thumbRect.setBounds(AquaUtils.toRectangle(p.getThumbBounds()));
        AquaFocusRingManager.focusRingOutlineChanged(slider);
    }

    @Override
    protected void calculateThumbSize() {
        // The thumb size is used to calculate the track rectangle.

        if (slider.getOrientation() == SwingConstants.HORIZONTAL) {
            thumbRect.x = 0;
            thumbRect.y = 0;
            thumbRect.width = 20;   // does not matter
            thumbRect.height = fixedHeight > 0 ? fixedHeight : 23;
        } else {
            thumbRect.x = 0;
            thumbRect.y = 0;
            thumbRect.width = fixedWidth > 0 ? fixedWidth : 23;
            thumbRect.height = 20;  // does not matter
        }
    }

    // changed to use left and right track buffers
    protected void calculateTrackRect() {
         int centerSpacing; // used to center sliders added using BorderLayout.CENTER (bug 4275631)
         if ( slider.getOrientation() == JSlider.HORIZONTAL ) {
             centerSpacing = thumbRect.height;
             if ( slider.getPaintTicks() ) centerSpacing += getTickLength();
             if ( slider.getPaintLabels() ) centerSpacing += getHeightOfTallestLabel();
             trackRect.x = contentRect.x + leftTrackBuffer;
             trackRect.y = contentRect.y + (contentRect.height - centerSpacing - 1)/2;
             trackRect.width = contentRect.width - leftTrackBuffer - rightTrackBuffer;
             trackRect.height = thumbRect.height;
         }
         else {
             centerSpacing = thumbRect.width;
             if (slider.getComponentOrientation().isLeftToRight()) {
                 if ( slider.getPaintTicks() ) centerSpacing += getTickLength();
                 if ( slider.getPaintLabels() ) centerSpacing += getWidthOfWidestLabel();
             } else {
                 if ( slider.getPaintTicks() ) centerSpacing -= getTickLength();
                 if ( slider.getPaintLabels() ) centerSpacing -= getWidthOfWidestLabel();
             }
             trackRect.x = contentRect.x + (contentRect.width - centerSpacing - 1)/2;
             trackRect.y = contentRect.y + leftTrackBuffer;
             trackRect.width = thumbRect.width;
             trackRect.height = contentRect.height - leftTrackBuffer - rightTrackBuffer;
         }
     }

    protected void calculateTrackBuffer() {

        // Because the native rendering includes enough room to display the knob in any position, we only need a
        // track buffer to leave room for the labels at the extreme positions.

        leftTrackBuffer = 0;
        rightTrackBuffer = 0;

        if ( slider.getPaintLabels() && slider.getLabelTable() != null ) {

            // The painter is configured with the track width, which has not yet been calculated

            trackRect.width = contentRect.width;
            trackRect.height = contentRect.height;

            SliderPainter p = getConfiguredPainter();

            int lowValue = getLowestValue();
            int highValue = getHighestValue();
            double lowThumbPosition = toThumbPosition(lowValue);
            double highThumbPosition = toThumbPosition(highValue);

            Component lowLabel = (Component)slider.getLabelTable().get(lowValue);
            Rectangle lowBounds = AquaUtils.toRectangle(p.getLabelBounds(lowThumbPosition, lowLabel.getSize()));

            Component highLabel = (Component)slider.getLabelTable().get(highValue);
            Rectangle highBounds = AquaUtils.toRectangle(p.getLabelBounds(highThumbPosition, highLabel.getSize()));

            boolean isInverted = drawInverted();
            boolean isHorizontal = slider.getOrientation() == JSlider.HORIZONTAL;

            Rectangle leftBounds = isHorizontal ^ isInverted ? lowBounds : highBounds;
            Rectangle rightBounds = isHorizontal ^ isInverted ? highBounds : lowBounds;

            if (isHorizontal) {
                leftTrackBuffer = Math.max(leftTrackBuffer, -leftBounds.x);
                rightTrackBuffer = Math.max(rightTrackBuffer, rightBounds.x + rightBounds.width - trackRect.width);
            } else {
                leftTrackBuffer = Math.max(leftTrackBuffer, -leftBounds.y);
                rightTrackBuffer = Math.max(rightTrackBuffer, rightBounds.y + rightBounds.height - trackRect.height);
            }
        }

        trackBuffer = Math.max(leftTrackBuffer, rightTrackBuffer);

        // TBD: Having a distinct left and right buffer is useful in some cases but bad when you have multiple
        // sliders stacked. Should be a user option.

        boolean lockBuffers = true; // TBD: allow a client property to determine
        if (lockBuffers) {
            leftTrackBuffer = trackBuffer;
            rightTrackBuffer = trackBuffer;
        }
    }

    /**
     * Convert a slider value to a thumb position.
     */

    protected double toThumbPosition(int value) {
        int minimum = slider.getMinimum();
        int maximum = slider.getMaximum();
        double range = maximum - minimum;
        if (range > 0) {
            return minimum + value / range;
        }
        return 0;
    }

    // The renderer adds a separation when determining label bounds.
    // Instead of hard wiring this separation here, we could change computeLabelRect to asks the painter
    // for the bounds of labels.

    @Override
    protected int getWidthOfWidestLabel() {
        int w = super.getWidthOfWidestLabel();
        return w > 0 ? w + 2 : 0;
    }

    @Override
    protected int getHeightOfTallestLabel() {
        int h = super.getHeightOfTallestLabel();
        return h > 0 ? h + 2 : 0;
    }

    // Basic's preferred size doesn't allow for our focus ring, throwing off things like SwingSet2
    public Dimension getPreferredHorizontalSize() {
        return new Dimension(190, 21);
    }

    public Dimension getPreferredVerticalSize() {
        return new Dimension(21, 190);
    }

    @Override
    public void setThumbLocation(int x, int y) {
        // The superclass limited repaint is not working properly
        thumbRect.setLocation(x, y);
        slider.repaint();
        AquaFocusRingManager.focusRingOutlineChanged(slider);
    }

    protected ChangeListener createChangeListener(final JSlider s) {
        return new StateChangeListener();
    }

    protected class StateChangeListener implements ChangeListener {
        @Override
        public void stateChanged(ChangeEvent e) {
            if (fIsDragging) return;
            calculateThumbLocation();
            slider.repaint();
            AquaFocusRingManager.focusRingOutlineChanged(slider);
        }
    }

    // This is copied almost verbatim from superclass, except we changed things to use fIsDragging
    // instead of isDragging since isDragging was a private member.
    class TrackListener extends BasicSliderUI.TrackListener {
        protected transient int offset;
        protected transient int currentMouseX = -1, currentMouseY = -1;

        public void mouseReleased(final MouseEvent e) {
            if (!slider.isEnabled()) return;

            currentMouseX = -1;
            currentMouseY = -1;

            offset = 0;
            scrollTimer.stop();

            // This is the way we have to determine snap-to-ticks.  It's hard to explain
            // but since ChangeEvents don't give us any idea what has changed we don't
            // have a way to stop the thumb bounds from being recalculated.  Recalculating
            // the thumb bounds moves the thumb over the current value (i.e., snapping
            // to the ticks).
            if (slider.getSnapToTicks() /*|| slider.getSnapToValue()*/) {
                fIsDragging = false;
                slider.setValueIsAdjusting(false);
            } else {
                slider.setValueIsAdjusting(false);
                fIsDragging = false;
            }

            slider.repaint();
            AquaFocusRingManager.focusRingOutlineChanged(slider);
        }

        public void mousePressed(final MouseEvent e) {
            if (!slider.isEnabled()) return;

            // We should recalculate geometry just before
            // calculation of the thumb movement direction.
            // It is important for the case, when JSlider
            // is a cell editor in JTable. See 6348946.
            calculateGeometry();

            final boolean firstClick = (currentMouseX == -1) && (currentMouseY == -1);

            currentMouseX = e.getX();
            currentMouseY = e.getY();

            if (slider.isRequestFocusEnabled()) {
                slider.requestFocus();
            }

            boolean isMouseEventInThumb = thumbRect.contains(currentMouseX, currentMouseY);

            // we don't want to move the thumb if we just clicked on the edge of the thumb
            if (!firstClick || !isMouseEventInThumb) {
                slider.setValueIsAdjusting(true);
                updateSliderFromLocation(currentMouseX, currentMouseY);
                slider.setValueIsAdjusting(false);
                isMouseEventInThumb = true; // since we just moved it in there
            }

            // Clicked in the Thumb area?
            if (isMouseEventInThumb) {
                switch (slider.getOrientation()) {
                    case SwingConstants.VERTICAL:
                        offset = currentMouseY - thumbRect.y;
                        break;
                    case SwingConstants.HORIZONTAL:
                        offset = currentMouseX - thumbRect.x;
                        break;
                }

                fIsDragging = true;
                slider.repaint();   // display pressed state
                return;
            }

            fIsDragging = false;
        }

        public boolean shouldScroll(final int direction) {
            final Rectangle r = thumbRect;
            if (slider.getOrientation() == SwingConstants.VERTICAL) {
                if (drawInverted() ? direction < 0 : direction > 0) {
                    if (r.y + r.height <= currentMouseY) return false;
                } else {
                    if (r.y >= currentMouseY) return false;
                }
            } else {
                if (drawInverted() ? direction < 0 : direction > 0) {
                    if (r.x + r.width >= currentMouseX) return false;
                } else {
                    if (r.x <= currentMouseX) return false;
                }
            }

            if (direction > 0 && slider.getValue() + slider.getExtent() >= slider.getMaximum()) {
                return false;
            }

            if (direction < 0 && slider.getValue() <= slider.getMinimum()) {
                return false;
            }

            return true;
        }

        /**
         * Set the models value to the position of the top/left
         * of the thumb relative to the origin of the track.
         */
        public void mouseDragged(final MouseEvent e) {
            if (!slider.isEnabled()) return;

            currentMouseX = e.getX();
            currentMouseY = e.getY();

            if (!fIsDragging) return;

            slider.setValueIsAdjusting(true);
            updateSliderFromLocation(currentMouseX, currentMouseY);

            // enable live snap-to-ticks <rdar://problem/3165310>
            if (slider.getSnapToTicks()) {
                calculateThumbLocation();
                setThumbLocation(thumbRect.x, thumbRect.y); // need to call to refresh the repaint region
            }
        }

        public void mouseMoved(final MouseEvent e) { }
    }

    protected void updateSliderFromLocation(int x, int y) {
        SliderPainter p = getConfiguredPainter();
        double thumbPosition = p.getThumbPosition(x - trackRect.x, y - trackRect.y);
        double range = slider.getMaximum() - slider.getMinimum();
        int value = (int) (slider.getMinimum() + thumbPosition * range);
        slider.setValue(value);
        p = getConfiguredPainter();
        thumbRect.setBounds(AquaUtils.toRectangle(p.getThumbBounds()));
        slider.repaint();
        AquaFocusRingManager.focusRingOutlineChanged(slider);
    }

    public void paintLabels( Graphics g ) {

        double range = slider.getMaximum() - slider.getMinimum();
        if (range <= 0) {
            return;
        }

        Dictionary dictionary = slider.getLabelTable();
        if ( dictionary != null ) {

            SliderPainter p = getConfiguredPainter();

            Enumeration keys = dictionary.keys();
            int minValue = slider.getMinimum();
            int maxValue = slider.getMaximum();
            boolean enabled = slider.isEnabled();
            while ( keys.hasMoreElements() ) {
                Integer key = (Integer)keys.nextElement();
                int value = key.intValue();
                if (value >= minValue && value <= maxValue) {
                    JComponent label = (JComponent) dictionary.get(key);
                    label.setEnabled(enabled);

                    if (label instanceof JLabel) {
                        Icon icon = label.isEnabled() ? ((JLabel) label).getIcon() : ((JLabel) label).getDisabledIcon();

                        if (icon instanceof ImageIcon) {
                            // Register Slider as an image observer. It allows to catch notifications about
                            // image changes (e.g. gif animation)
                            Toolkit.getDefaultToolkit().checkImage(((ImageIcon) icon).getImage(), -1, -1, slider);
                        }
                    }

                    double thumbPosition = (value - slider.getMinimum()) / range;
                    Dimension labelSize = label.getPreferredSize();
                    Rectangle labelBounds = AquaUtils.toRectangle(p.getLabelBounds(thumbPosition, labelSize));
                    g.translate(trackRect.x + labelBounds.x, trackRect.y + labelBounds.y);
                    label.paint( g );
                    g.translate(-(trackRect.x + labelBounds.x), -(trackRect.y + labelBounds.y));
                }
            }
        }
    }

    /**
     * Called for every label in the label table.  Used to draw the labels for horizontal sliders.
     * The graphics have been translated to labelRect.y already.
     * @see JSlider#setLabelTable
     */
    protected void paintHorizontalLabel( Graphics g, int value, Component label ) {
        double range = slider.getMaximum() - slider.getMinimum();
        if (range > 0) {
            double thumbPosition = (value - slider.getMinimum()) / range;
            Dimension labelSize = label.getPreferredSize();
            SliderPainter p = getConfiguredPainter();
            Rectangle labelBounds = AquaUtils.toRectangle(p.getLabelBounds(thumbPosition, labelSize));
            g.translate(trackRect.x + labelBounds.x, trackRect.y + labelBounds.y - labelRect.y);
            label.paint( g );
            g.translate(- (trackRect.x + labelBounds.x), -(trackRect.y + labelBounds.y - labelRect.y));
        }
    }

    /**
     * Called for every label in the label table.  Used to draw the labels for vertical sliders.
     * The graphics have been translated to labelRect.x already.
     * @see JSlider#setLabelTable
     */
    protected void paintVerticalLabel( Graphics g, int value, Component label ) {
        double range = slider.getMaximum() - slider.getMinimum();
        if (range > 0) {
            double thumbPosition = (value - slider.getMinimum()) / range;
            Dimension labelSize = label.getPreferredSize();
            SliderPainter p = getConfiguredPainter();
            Rectangle labelBounds = AquaUtils.toRectangle(p.getLabelBounds(thumbPosition, labelSize));
            g.translate(trackRect.x + labelBounds.x - labelRect.x, trackRect.y + labelBounds.y);
            label.paint( g );
            g.translate(- (trackRect.x + labelBounds.x - labelRect.x), -(trackRect.y + labelBounds.y));
        }
    }

    // duplicated code - because this method is private in BasicSliderUI
    protected int getTickSpacing() {
        int majorTickSpacing = slider.getMajorTickSpacing();
        int minorTickSpacing = slider.getMinorTickSpacing();

        int result;

        if (minorTickSpacing > 0) {
            result = minorTickSpacing;
        } else if (majorTickSpacing > 0) {
            result = majorTickSpacing;
        } else {
            result = 0;
        }

        return result;
    }
}
