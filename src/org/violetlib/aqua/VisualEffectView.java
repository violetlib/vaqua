/*
 * Copyright (c) 2015-2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * This object manages a NSVisualEffectView or a collection of NSVisualEffectViews whose bounds tracks the bounds of a
 * component. A collection of views is used to support vibrant selection backgrounds. Each region that displays a
 * selection background has its own NSVisualEffectView to create the selection background.
 */
public class VisualEffectView {
    private final JComponent component;
    private final int style;
    private final boolean supportSelections;
    private VisualEffectViewPeer peer;
    private Rectangle oldBounds;
    private Window window;
    private int windowHeight;   // corresponds to bounds, needed to calculate AppKit Y origin
    private ComponentTracker tracker;

    /**
     * Create an object to manage a NSVisualEffectView or a collection of NSVisualEffectViews.
     * @param c The component that the NSVisualEffectView(s) will track.
     * @param style The style of the (master) NSVisualEffectView.
     * @param supportSelections If true, enable support for subregions with a distinct selection background.
     */
    public VisualEffectView(JComponent c, int style, boolean supportSelections) {
        this.component = c;
        this.style = style;
        this.supportSelections = supportSelections;
        tracker = new MyComponentTracker();
        tracker.attach(component);
    }

    public int getStyle() {
        return style;
    }

    public @NotNull JComponent getComponent() {
        return component;
    }

    public void dispose() {
        if (tracker != null) {
            tracker.attach(null);
            tracker = null;
        }
    }

    /**
     * Update the set of regions to display a selection background.
     * @param sd A description of the regions, or null if there are no regions.
     */
    public void updateSelectionBackgrounds(SelectionBoundsDescription sd) {
        if (peer != null && supportSelections) {
            peer.updateSelectionBackgrounds(sd);
        }
    }

    protected void windowChanged(@Nullable Window newWindow) {

        Window oldWindow = window;
        window = newWindow;

        // If the visual effect view is installed in a window that no longer owns this component or is no longer
        // displayable, then the old visual effect view is either gone or should be removed from the window.

        if (peer != null) {
            assert oldWindow != null;
            if (oldWindow != window || !oldWindow.isDisplayable()) {
                peer.dispose();
                peer = null;
                oldBounds = null;
            }
        }

        // If there is no visual effect view and the (new) window is displayable, create a visual effect view.
        if (peer == null && window != null && window.isDisplayable()) {
            peer = AquaVibrantSupport.createVisualEffectView(window, style, supportSelections);
            visibleBoundsChanged();
        }
    }

    protected void visibleBoundsChanged() {
        VisibleBounds vb = AquaUtils.getVisibleBoundsInWindow(component);
        if (vb != null) {
            // TBD: not considering occlusion if the component is clipped by a viewport
            Rectangle frame = vb.frame;
            setFrame(frame.x, frame.y, frame.width, frame.height);
        } else {
            setFrame(0, 0, 0, 0);
        }
    }

    private void setFrame(int x, int y, int w, int h) {
        int newWindowHeight = window != null ? window.getHeight() : 0;
        if (oldBounds == null || x != oldBounds.x || y != oldBounds.y
                || w != oldBounds.width || h != oldBounds.height || windowHeight != newWindowHeight) {
            oldBounds = new Rectangle(x, y, w, h);
            windowHeight = newWindowHeight;
            if (peer != null) {
                peer.setFrame(x, y, w, h);
            }
        }
    }

    private class MyComponentTracker extends ComponentTracker {
        @Override
        protected void attached(@Nullable Window w) {
            if (w != null) {
                VisualEffectView.this.windowChanged(w);
                VisualEffectView.this.visibleBoundsChanged();
            }
        }

        @Override
        protected void detached(@Nullable Window w) {
            if (window != null) {
                VisualEffectView.this.windowChanged(null);
            }
        }

        @Override
        protected void windowChanged(@Nullable Window oldWindow, @Nullable Window newWindow) {
            VisualEffectView.this.windowChanged(newWindow);
        }

        @Override
        protected void visibleBoundsChanged(@Nullable Window window) {
            if (window != null) {
                VisualEffectView.this.visibleBoundsChanged();
            }
        }
    }
}
