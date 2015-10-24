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
import java.awt.geom.AffineTransform;
import java.beans.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.*;

import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.Orientation;
import org.violetlib.jnr.aqua.AquaUIPainter.ProgressWidget;

import org.violetlib.aqua.AquaUtilControlSize.*;

public class AquaProgressBarUI extends ProgressBarUI implements ChangeListener, PropertyChangeListener, AncestorListener, Sizeable {

    public static final String PROGRESS_BAR_STYLE_KEY = "JProgressBar.style";

    private static final boolean ADJUSTTIMER = true;

    protected Size sizeVariant = Size.REGULAR;

    protected Color selectionForeground;

    private Animator animator;
    protected boolean isAnimating;
    protected boolean isCircular;
    protected int repaintInterval;  // depends upon isCircular

    protected final AquaUIPainter painter = AquaPainting.create();

    protected JProgressBar progressBar;

    public static ComponentUI createUI(final JComponent x) {
        return new AquaProgressBarUI();
    }

    protected AquaProgressBarUI() { }

    public void installUI(final JComponent c) {
        progressBar = (JProgressBar)c;
        installDefaults();
        installListeners();
    }

    public void uninstallUI(final JComponent c) {
        uninstallDefaults();
        uninstallListeners();
        stopAnimationTimer();
        progressBar = null;
    }

    protected void installDefaults() {
        LookAndFeel.installProperty(progressBar, "opaque", false);
        LookAndFeel.installBorder(progressBar, "ProgressBar.border");
        LookAndFeel.installColorsAndFont(progressBar, "ProgressBar.background", "ProgressBar.foreground", "ProgressBar.font");
        selectionForeground = UIManager.getColor("ProgressBar.selectionForeground");
    }

    protected void uninstallDefaults() {
        LookAndFeel.uninstallBorder(progressBar);
    }

    protected void installListeners() {
        progressBar.addChangeListener(this); // Listen for changes in the progress bar's data
        progressBar.addPropertyChangeListener(this); // Listen for changes between determinate and indeterminate state
        progressBar.addAncestorListener(this);
        AquaUtilControlSize.addSizePropertyListener(progressBar);
    }

    protected void uninstallListeners() {
        AquaUtilControlSize.removeSizePropertyListener(progressBar);
        progressBar.removeAncestorListener(this);
        progressBar.removePropertyChangeListener(this);
        progressBar.removeChangeListener(this);
    }

    public void stateChanged(final ChangeEvent e) {
        progressBar.repaint();
    }

    public void propertyChange(final PropertyChangeEvent e) {
        final String prop = e.getPropertyName();

        if (AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(prop)) {
            progressBar.repaint();
        } else if ("indeterminate".equals(prop)) {
            stopAnimationTimer();
            progressBar.repaint();
        } else if (isStyleProperty(prop)) {
            String style = getStyleProperty();
            isCircular = "circular".equalsIgnoreCase(style + "");
            repaintInterval = isCircular ? UIManager.getInt("ProgressBar.circularRepaintInterval") : UIManager.getInt("ProgressBar.repaintInterval");
            stopAnimationTimer();
            progressBar.revalidate();
            progressBar.repaint();
        } else if ("orientation".equals(prop)) {
            progressBar.revalidate();
            progressBar.repaint();
        }
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, PROGRESS_BAR_STYLE_KEY);
    }

    protected String getStyleProperty() {
        return AquaUtils.getProperty(progressBar, PROGRESS_BAR_STYLE_KEY);
    }

    // listen for Ancestor events to stop our timer when we are no longer visible
    // <rdar://problem/5405035> JProgressBar: UI in Aqua look and feel causes memory leaks
    public void ancestorRemoved(final AncestorEvent e) {
        stopAnimationTimer();
    }

    public void ancestorAdded(final AncestorEvent e) {
        if (!progressBar.isIndeterminate()) return;
        startAnimationTimer();
    }

    public void ancestorMoved(final AncestorEvent e) { }

    public void paint(final Graphics g, final JComponent c) {
        revalidateAnimationTimers(); // revalidate to turn on/off timers when values change

        // this is questionable. We may want the insets to mean something different.
        final Insets i = progressBar.getInsets();
        final int width = progressBar.getWidth() - (i.right + i.left);
        final int height = progressBar.getHeight() - (i.bottom + i.top);
        final int x = i.left;
        final int y = i.top;

        painter.configure(width, height);

        Configuration pg = getConfiguration();
        painter.getPainter(pg).paint(g, x, y);

        if (isCircular) {
            return;
        }

        if (progressBar.isStringPainted()) {
            paintString(g, i.left, i.top, width, height);
        }
    }

    protected ProgressIndicatorLayoutConfiguration getLayoutConfiguration() {
        Orientation orientation = isHorizontal() ? Orientation.HORIZONTAL : Orientation.VERTICAL;
        if (progressBar.isIndeterminate()) {
            AquaUIPainter.ProgressWidget w = isCircular ? ProgressWidget.SPINNER : ProgressWidget.INDETERMINATE_BAR;
            return new ProgressIndicatorLayoutConfiguration(w, sizeVariant, orientation);
        } else {
            return new ProgressIndicatorLayoutConfiguration(ProgressWidget.BAR, sizeVariant, orientation);
        }
    }

    protected Configuration getConfiguration() {
        State state = getState(progressBar);
        Orientation orientation = isHorizontal() ? Orientation.HORIZONTAL : Orientation.VERTICAL;
        if (progressBar.isIndeterminate()) {
            int frameCount = isCircular ? 15 : 90;
            long intervals = System.currentTimeMillis() / (repaintInterval > 0 ? repaintInterval : 100);
            int speed = isCircular ? 1 : 4;
            int animationFrame = (int) (speed * intervals % frameCount);
            AquaUIPainter.ProgressWidget w = isCircular ? ProgressWidget.SPINNER : ProgressWidget.INDETERMINATE_BAR;
            return new IndeterminateProgressIndicatorConfiguration(w, sizeVariant, state, orientation, animationFrame);
        } else {
            double value = checkValue(progressBar.getPercentComplete());
            AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(progressBar);
            return new ProgressIndicatorConfiguration(AquaUIPainter.ProgressWidget.BAR, sizeVariant, state, orientation, value, ld);
        }
    }

    static double checkValue(final double value) {
        return Double.isNaN(value) ? 0 : value;
    }

    protected State getState(final JComponent c) {
        if (!c.isEnabled()) return State.INACTIVE;
        if (!AquaFocusHandler.isActive(c)) return State.INACTIVE;
        return State.ACTIVE;
    }

    protected void paintString(final Graphics g, final int x, final int y, final int width, final int height) {
        if (!(g instanceof Graphics2D)) return;

        final Graphics2D g2 = (Graphics2D)g;
        final String progressString = progressBar.getString();
        g2.setFont(progressBar.getFont());
        final Point renderLocation = getStringPlacement(g2, progressString, x, y, width, height);
        final Rectangle oldClip = g2.getClipBounds();

        if (isHorizontal()) {
            g2.setColor(selectionForeground);
            AquaUtils.drawString(progressBar, g2, progressString, renderLocation.x, renderLocation.y);
        } else { // VERTICAL
            // We rotate it -90 degrees, then translate it down since we are going to be bottom up.
            final AffineTransform savedAT = g2.getTransform();
            g2.transform(AffineTransform.getRotateInstance(0.0f - (Math.PI / 2.0f), 0, 0));
            g2.translate(-progressBar.getHeight(), 0);

            // 0,0 is now the bottom left of the viewable area, so we just draw our image at
            // the render location since that calculation knows about rotation.
            g2.setColor(selectionForeground);
            AquaUtils.drawString(progressBar, g2, progressString, renderLocation.x, renderLocation.y);

            g2.setTransform(savedAT);
        }

        g2.setClip(oldClip);
    }

    /**
     * Designate the place where the progress string will be painted. This implementation places it at the center of the
     * progress bar (in both x and y). Override this if you want to right, left, top, or bottom align the progress
     * string or if you need to nudge it around for any reason.
     */
    protected Point getStringPlacement(final Graphics g, final String progressString, int x, int y, int width, int height) {
        final FontMetrics fontSizer = progressBar.getFontMetrics(progressBar.getFont());
        final int stringWidth = fontSizer.stringWidth(progressString);

        if (!isHorizontal()) {
            // Calculate the location for the rotated text in real component coordinates.
            // swapping x & y and width & height
            final int oldH = height;
            height = width;
            width = oldH;

            final int oldX = x;
            x = y;
            y = oldX;
        }

        return new Point(x + Math.round(width / 2 - stringWidth / 2), y + ((height + fontSizer.getAscent() - fontSizer.getLeading() - fontSizer.getDescent()) / 2) - 1);
    }

    protected Dimension getCircularPreferredSize() {
        LayoutConfiguration g = getLayoutConfiguration();
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
        int width = (int) layoutInfo.getFixedVisualWidth();
        int height = (int) layoutInfo.getFixedVisualHeight();
        if (width == 0) {
            width = 32;
        }
        if (height == 0) {
            height = 32;
        }
        return new Dimension(width, height);
    }

    public Dimension getPreferredSize(final JComponent c) {
        if (isCircular) {
            return getCircularPreferredSize();
        }

        final FontMetrics metrics = progressBar.getFontMetrics(progressBar.getFont());
        ProgressIndicatorLayoutConfiguration g = getLayoutConfiguration();
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);

        final Dimension size = isHorizontal() ? getPreferredHorizontalSize(layoutInfo, metrics) : getPreferredVerticalSize(layoutInfo, metrics);
        final Insets insets = progressBar.getInsets();

        size.width += insets.left + insets.right;
        size.height += insets.top + insets.bottom;
        return size;
    }

    protected Dimension getPreferredHorizontalSize(LayoutInfo layoutInfo, final FontMetrics metrics) {
        int width = sizeVariant == Size.REGULAR ? 146 : 140;
        int height = sizeVariant == Size.REGULAR ? 20 : 14;
        height = (int) Math.max(height, layoutInfo.getMinimumVisualHeight());
        final Dimension size = new Dimension(width, height);
        if (!progressBar.isStringPainted()) return size;

        // Ensure that the progress string will fit
        final String progString = progressBar.getString();
        final int stringWidth = metrics.stringWidth(progString);
        if (stringWidth > size.width) {
            size.width = stringWidth;
        }

        // This uses both Height and Descent to be sure that
        // there is more than enough room in the progress bar
        // for everything.
        // This does have a strange dependency on
        // getStringPlacememnt() in a funny way.
        final int stringHeight = metrics.getHeight() + metrics.getDescent();
        if (stringHeight > size.height) {
            size.height = stringHeight;
        }
        return size;
    }

    protected Dimension getPreferredVerticalSize(LayoutInfo layoutInfo, final FontMetrics metrics) {
        int width = sizeVariant == Size.REGULAR ? 20 : 14;
        int height = sizeVariant == Size.REGULAR ? 146 : 140;
        width = (int) Math.max(width, layoutInfo.getMinimumVisualWidth());
        final Dimension size = new Dimension(width, height);
        if (!progressBar.isStringPainted()) return size;

        // Ensure that the progress string will fit.
        final String progString = progressBar.getString();
        final int stringHeight = metrics.getHeight() + metrics.getDescent();
        if (stringHeight > size.width) {
            size.width = stringHeight;
        }

        // This is also for completeness.
        final int stringWidth = metrics.stringWidth(progString);
        if (stringWidth > size.height) {
            size.height = stringWidth;
        }
        return size;
    }

    public Dimension getMinimumSize(final JComponent c) {
        if (isCircular) {
            return getCircularPreferredSize();
        }

        final Dimension pref = getPreferredSize(progressBar);

        // The Minimum size for this component is 10.
        // The rationale here is that there should be at least one pixel per 10 percent.
        if (isHorizontal()) {
            pref.width = 10;
        } else {
            pref.height = 10;
        }

        return pref;
    }

    public Dimension getMaximumSize(final JComponent c) {
        if (isCircular) {
            return getCircularPreferredSize();
        }

        final Dimension pref = getPreferredSize(progressBar);

        if (isHorizontal()) {
            pref.width = Short.MAX_VALUE;
        } else {
            pref.height = Short.MAX_VALUE;
        }

        return pref;
    }

    public void applySizeFor(JComponent c, Size size, boolean isDefaultSize) {
        sizeVariant = size;
        AquaUtilControlSize.configureFontFromSize(c, size);
        progressBar.revalidate();
        progressBar.repaint();
    }

    protected void startAnimationTimer() {
        if (animator == null) animator = new Animator();
        animator.start();
        isAnimating = true;
    }

    protected void stopAnimationTimer() {
        if (animator != null) animator.stop();
        isAnimating = false;
    }

    protected boolean isHorizontal() {
        return progressBar.getOrientation() == SwingConstants.HORIZONTAL;
    }

    protected void revalidateAnimationTimers() {
        if (!progressBar.isIndeterminate() || getState(progressBar) == State.INACTIVE) {
            stopAnimationTimer();
        } else {
            if (!isAnimating) {
                startAnimationTimer();
            }
        }
    }

    protected class Animator implements ActionListener {
        private static final int MINIMUM_DELAY = 5;
        private Timer timer;
        private long previousDelay; // used to tune the repaint interval
        private long lastCall; // the last time actionPerformed was called
        private final int repaintInterval;

        public Animator() {
            repaintInterval = AquaProgressBarUI.this.repaintInterval > 0 ? AquaProgressBarUI.this.repaintInterval : 100;
        }

        protected void start() {
            previousDelay = repaintInterval;
            lastCall = 0;

            if (timer == null) {
                timer = new Timer(repaintInterval, this);
            } else {
                timer.setDelay(repaintInterval);
            }

            if (ADJUSTTIMER) {
                timer.setRepeats(false);
                timer.setCoalesce(false);
            }

            timer.start();
        }

        protected void stop() {
            timer.stop();
        }

        public void actionPerformed(final ActionEvent e) {
            if (!ADJUSTTIMER) {
                progressBar.repaint();
                return;
            }

            final long time = System.currentTimeMillis();

            if (lastCall > 0) {
                // adjust nextDelay
                int nextDelay = (int)(previousDelay - time + lastCall + repaintInterval);
                if (nextDelay < MINIMUM_DELAY) {
                    nextDelay = MINIMUM_DELAY;
                }

                timer.setInitialDelay(nextDelay);
                previousDelay = nextDelay;
            }

            timer.start();
            lastCall = time;

            progressBar.repaint();
        }
    }
}
