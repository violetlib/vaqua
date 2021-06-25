/*
 * Changes Copyright (c) 2015-2021 Alan Snyder.
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ProgressBarUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtilControlSize.Sizeable;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.Orientation;
import org.violetlib.jnr.aqua.AquaUIPainter.ProgressWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

import static org.violetlib.aqua.OSXSystemProperties.OSVersion;

public class AquaProgressBarUI
        extends ProgressBarUI
        implements ChangeListener, PropertyChangeListener, AncestorListener, Sizeable, AquaComponentUI {

    public static final String PROGRESS_BAR_STYLE_KEY = "JProgressBar.style";

    private static final boolean ADJUSTTIMER = true;

    public static ComponentUI createUI(JComponent x) {
        return new AquaProgressBarUI();
    }

    protected JProgressBar progressBar;
    protected Size sizeVariant = Size.REGULAR;
    protected @NotNull BasicContextualColors colors;
    protected boolean isCircular;

    private Animator animator;
    protected boolean isAnimating;
    protected int repaintInterval;  // depends upon isCircular

    protected final AquaUIPainter painter = AquaPainting.create();

    protected AquaProgressBarUI() {
        colors = AquaColors.CLEAR_CONTROL_COLORS;
    }

    @Override
    public void installUI(JComponent c) {
        progressBar = (JProgressBar)c;
        installDefaults();
        installListeners();
    }

    @Override
    public void uninstallUI(JComponent c) {
        uninstallDefaults();
        uninstallListeners();
        stopAnimationTimer();
        progressBar = null;
    }

    protected void installDefaults() {
        LookAndFeel.installProperty(progressBar, "opaque", false);
        LookAndFeel.installBorder(progressBar, "ProgressBar.border");
        AquaUtils.installFont(progressBar, "ProgressBar.font");
        configureAppearanceContext(null);
    }

    protected void uninstallDefaults() {
        LookAndFeel.uninstallBorder(progressBar);
    }

    protected void installListeners() {
        progressBar.addChangeListener(this); // Listen for changes in the progress bar's data
        progressBar.addPropertyChangeListener(this); // Listen for changes between determinate and indeterminate state
        progressBar.addAncestorListener(this);
        AquaUtilControlSize.addSizePropertyListener(progressBar);
        AppearanceManager.installListeners(progressBar);
    }

    protected void uninstallListeners() {
        AppearanceManager.uninstallListeners(progressBar);
        AquaUtilControlSize.removeSizePropertyListener(progressBar);
        progressBar.removeAncestorListener(this);
        progressBar.removePropertyChangeListener(this);
        progressBar.removeChangeListener(this);
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        configureAppearanceContext(null);
    }

    @Override
    public void stateChanged(ChangeEvent e) {
        progressBar.repaint();
    }

    @Override
    public void propertyChange(PropertyChangeEvent e) {
        String prop = e.getPropertyName();

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
    @Override
    public void ancestorRemoved(AncestorEvent e) {
        stopAnimationTimer();
    }

    @Override
    public void ancestorAdded(AncestorEvent e) {
        if (!progressBar.isIndeterminate()) {
            return;
        }
        startAnimationTimer();
    }

    @Override
    public void ancestorMoved(AncestorEvent e) {
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(progressBar);
        }
        AquaUIPainter.State state = getState();
        AppearanceContext appearanceContext = new AppearanceContext(appearance, state, false, false);
        AquaColors.installColors(progressBar, appearanceContext, colors);
        progressBar.repaint();
    }

    protected @NotNull AquaUIPainter.State getState() {
        if (!progressBar.isEnabled()) {
            return State.INACTIVE;
        }
        if (!AquaFocusHandler.isActive(progressBar)) {
            return State.INACTIVE;
        }
        return State.ACTIVE;
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {

        revalidateAnimationTimers(); // revalidate to turn on/off timers when values change

        // this is questionable. We may want the insets to mean something different.
        Insets i = progressBar.getInsets();
        int width = progressBar.getWidth() - (i.right + i.left);
        int height = progressBar.getHeight() - (i.bottom + i.top);
        int x = i.left;
        int y = i.top;

        AquaUtils.configure(painter, progressBar, width, height);
        Configuration pg = getConfiguration();
        painter.getPainter(pg).paint(g, x, y);

        if (isCircular) {
            return;
        }

        if (progressBar.isStringPainted()) {
            paintString((Graphics2D) g, i.left, i.top, width, height);
        }
    }

    protected ProgressIndicatorLayoutConfiguration getLayoutConfiguration() {
        Orientation orientation = isHorizontal() ? Orientation.HORIZONTAL : Orientation.VERTICAL;
        if (progressBar.isIndeterminate()) {
            AquaUIPainter.ProgressWidget w = isCircular ? ProgressWidget.INDETERMINATE_SPINNER : ProgressWidget.INDETERMINATE_BAR;
            return new ProgressIndicatorLayoutConfiguration(w, sizeVariant, orientation);
        } else {
            AquaUIPainter.ProgressWidget w = isCircular ? ProgressWidget.SPINNER : ProgressWidget.BAR;
            return new ProgressIndicatorLayoutConfiguration(w, sizeVariant, orientation);
        }
    }

    protected @NotNull Configuration getConfiguration() {
        State state = getState();
        Orientation orientation = isHorizontal() ? Orientation.HORIZONTAL : Orientation.VERTICAL;
        if (progressBar.isIndeterminate()) {
            int frameCount = isCircular ? (OSVersion >= 1016 ? 24 : 15) : 90;
            long intervals = System.currentTimeMillis() / (repaintInterval > 0 ? repaintInterval : 100);
            int speed = isCircular ? (OSVersion >= 1016 ? 3 : 1) : 4;
            int animationFrame = (int) (speed * intervals % frameCount);
            AquaUIPainter.ProgressWidget w = isCircular ? ProgressWidget.INDETERMINATE_SPINNER : ProgressWidget.INDETERMINATE_BAR;
            return new IndeterminateProgressIndicatorConfiguration(w, sizeVariant, state, orientation, animationFrame);
        } else {
            double value = checkValue(progressBar.getPercentComplete());
            AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(progressBar);
            AquaUIPainter.ProgressWidget w = isCircular ? ProgressWidget.SPINNER : ProgressWidget.BAR;
            return new ProgressIndicatorConfiguration(w, sizeVariant, state, orientation, value, ld);
        }
    }

    private static double checkValue(double value) {
        return Double.isNaN(value) ? 0 : value;
    }

    protected void paintString(@NotNull Graphics2D g, int x, int y, int width, int height) {

        String progressString = progressBar.getString();
        g.setFont(progressBar.getFont());
        Point renderLocation = getStringPlacement(g, progressString, x, y, width, height);
        Rectangle oldClip = g.getClipBounds();
        g.setColor(progressBar.getForeground());

        if (isHorizontal()) {
            JavaSupport.drawString(progressBar, g, progressString, renderLocation.x, renderLocation.y);
        } else { // VERTICAL
            // We rotate it -90 degrees, then translate it down since we are going to be bottom up.
            AffineTransform savedAT = g.getTransform();
            g.transform(AffineTransform.getRotateInstance(0.0f - (Math.PI / 2.0f), 0, 0));
            g.translate(-progressBar.getHeight(), 0);

            // 0,0 is now the bottom left of the viewable area, so we just draw our image at
            // the render location since that calculation knows about rotation.
            JavaSupport.drawString(progressBar, g, progressString, renderLocation.x, renderLocation.y);

            g.setTransform(savedAT);
        }

        g.setClip(oldClip);
    }

    /**
     * Designate the place where the progress string will be painted. This implementation places it at the center of the
     * progress bar (in both x and y). Override this if you want to right, left, top, or bottom align the progress
     * string or if you need to nudge it around for any reason.
     */
    protected @NotNull Point getStringPlacement(Graphics g, String progressString, int x, int y, int width, int height) {
        FontMetrics fontSizer = progressBar.getFontMetrics(progressBar.getFont());
        int stringWidth = fontSizer.stringWidth(progressString);

        if (!isHorizontal()) {
            // Calculate the location for the rotated text in real component coordinates.
            // swapping x & y and width & height
            int oldH = height;
            height = width;
            width = oldH;

            int oldX = x;
            x = y;
            y = oldX;
        }

        return new Point(x + Math.round(width / 2 - stringWidth / 2), y + ((height + fontSizer.getAscent() - fontSizer.getLeading() - fontSizer.getDescent()) / 2) - 1);
    }

    protected @NotNull Dimension getCircularPreferredSize() {
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

    @Override
    public @NotNull Dimension getPreferredSize(JComponent c) {
        if (isCircular) {
            return getCircularPreferredSize();
        }

        FontMetrics metrics = progressBar.getFontMetrics(progressBar.getFont());
        ProgressIndicatorLayoutConfiguration g = getLayoutConfiguration();
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);

        Dimension size = isHorizontal() ? getPreferredHorizontalSize(layoutInfo, metrics) : getPreferredVerticalSize(layoutInfo, metrics);
        Insets insets = progressBar.getInsets();

        size.width += insets.left + insets.right;
        size.height += insets.top + insets.bottom;
        return size;
    }

    protected @NotNull Dimension getPreferredHorizontalSize(LayoutInfo layoutInfo, FontMetrics metrics) {
        int width = sizeVariant == Size.REGULAR ? 146 : 140;
        int height = sizeVariant == Size.REGULAR ? 20 : 14;
        height = (int) Math.max(height, layoutInfo.getMinimumVisualHeight());
        Dimension size = new Dimension(width, height);
        if (!progressBar.isStringPainted()) return size;

        // Ensure that the progress string will fit
        String progString = progressBar.getString();
        int stringWidth = metrics.stringWidth(progString);
        if (stringWidth > size.width) {
            size.width = stringWidth;
        }

        // This uses both Height and Descent to be sure that
        // there is more than enough room in the progress bar
        // for everything.
        // This does have a strange dependency on
        // getStringPlacememnt() in a funny way.
        int stringHeight = metrics.getHeight() + metrics.getDescent();
        if (stringHeight > size.height) {
            size.height = stringHeight;
        }
        return size;
    }

    protected @NotNull Dimension getPreferredVerticalSize(LayoutInfo layoutInfo, FontMetrics metrics) {
        int width = sizeVariant == Size.REGULAR ? 20 : 14;
        int height = sizeVariant == Size.REGULAR ? 146 : 140;
        width = (int) Math.max(width, layoutInfo.getMinimumVisualWidth());
        Dimension size = new Dimension(width, height);
        if (!progressBar.isStringPainted()) return size;

        // Ensure that the progress string will fit.
        String progString = progressBar.getString();
        int stringHeight = metrics.getHeight() + metrics.getDescent();
        if (stringHeight > size.width) {
            size.width = stringHeight;
        }

        // This is also for completeness.
        int stringWidth = metrics.stringWidth(progString);
        if (stringWidth > size.height) {
            size.height = stringWidth;
        }
        return size;
    }

    @Override
    public @NotNull Dimension getMinimumSize(JComponent c) {
        if (isCircular) {
            return getCircularPreferredSize();
        }

        Dimension pref = getPreferredSize(progressBar);

        // The Minimum size for this component is 10.
        // The rationale here is that there should be at least one pixel per 10 percent.
        if (isHorizontal()) {
            pref.width = 10;
        } else {
            pref.height = 10;
        }

        return pref;
    }

    @Override
    public @NotNull Dimension getMaximumSize(JComponent c) {
        if (isCircular) {
            return getCircularPreferredSize();
        }

        Dimension pref = getPreferredSize(progressBar);

        if (isHorizontal()) {
            pref.width = Short.MAX_VALUE;
        } else {
            pref.height = Short.MAX_VALUE;
        }

        return pref;
    }

    @Override
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
        if (!progressBar.isIndeterminate() || getState() == State.INACTIVE) {
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

        public void actionPerformed(ActionEvent e) {
            if (!ADJUSTTIMER) {
                progressBar.repaint();
                return;
            }

            long time = System.currentTimeMillis();

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
