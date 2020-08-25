/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * A generic component for supporting vibrant backgrounds on a component.
 */
public class ComponentVibrantEffects extends VisualEffectView {

    protected SelectionBoundsTracker boundsTracker;

    /**
     * Create an object to support vibrant backgrounds on a component.
     * @param c The component.
     * @param style The vibrant background style. See constants in {@link AquaVibrantSupport}.
     * @param bt An optional selection bounds tracker, if the component has selected item regions that should display
     *              using a vibrant style. A consumer will be installed in the tracker.
     */
    public ComponentVibrantEffects(JComponent c, int style, SelectionBoundsTracker bt) {
        super(c, style, bt != null);
        this.boundsTracker = bt;

        if (bt != null) {
            bt.setConsumer(sd -> ComponentVibrantEffects.this.updateSelectionBackgrounds(sd));
        }
    }

    /**
     * Recompute the selection bounds (if a selection bounds tracker was provided).
     */
    public void update() {
        if (boundsTracker != null) {
            boundsTracker.update();
        }
    }

    @Override
    public void dispose() {
        super.dispose();
        if (boundsTracker != null) {
            boundsTracker.dispose();
            boundsTracker = null;
        }
    }

    @Override
    protected void windowChanged(Window newWindow) {
        super.windowChanged(newWindow);
        if (boundsTracker != null) {
            boundsTracker.reset();
        }
    }
}
