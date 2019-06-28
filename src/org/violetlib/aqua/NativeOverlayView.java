/*
 * Copyright (c) 2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Objects;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A native overlay view is a native view that is layered above the AWT/Swing view and whose bounds and visibility are
 * synchronized with the bounds and visibility of an AWT/Swing component.
 */

public class NativeOverlayView {

    private @Nullable Component trackingComponent;  // null after disposal
    private long vptr;                              // zero after disposal
    private @Nullable Window window;                // null if tracking component not visible and after disposal
    private @Nullable VisibleBounds bounds;         // null if tracking component not visible and after disposal
    private @Nullable ComponentTracker tracker;     // null after disposal

    /**
     * Create a native overlay view. The view will be displayed over the AWT/Swing window containing the specified
     * tracking component with bounds that match the bounds of the tracking component. The native view will be
     * displayed only while the tracking view is showing.
     *
     * @param trackingComponent The component that controls the location and visibility of the native view.
     * @param nativeViewPointer A pointer to the native view. The native view must be retained while in use by this
     *                          object. This object does not release the native view.
     */
    public NativeOverlayView(@NotNull Component trackingComponent, long nativeViewPointer)
            throws IllegalArgumentException {

        if (nativeViewPointer == 0) {
            throw new IllegalArgumentException("Native view pointer must not be zero");
        }

        this.trackingComponent = trackingComponent;
        this.vptr = nativeViewPointer;

        // We need to know when the tracking component bounds change, when its showing status changes, when its window
        // ancestor changes.

        tracker = new MyComponentTracker();
        tracker.attach(trackingComponent);
        update();
    }

    /**
     * Remove all connections to the native view.
     */
    public void dispose() {
        if (vptr != 0) {
            if (window != null) {
                hideNativeView(vptr);
            }
            vptr = 0;
            assert tracker != null;
            tracker.attach(null);
            tracker = null;
            window = null;
            bounds = null;
            trackingComponent = null;
        }
    }

    /**
     * Update the bounds and superview of the native view based on the corresponding attributes of the tracking view.
     */
    private void update() {
        if (vptr == 0) {
            return;
        }

        assert trackingComponent != null;
        Window newWindow = SwingUtilities.getWindowAncestor(trackingComponent);
        VisibleBounds newBounds = null;
        if (newWindow != null && trackingComponent.isShowing()) {
            newBounds = AquaUtils.getVisibleBoundsInContentView(trackingComponent);
        }
        if (!Objects.equals(window, newWindow) || !Objects.equals(bounds, newBounds)) {
            window = newWindow;
            bounds = newBounds;
            if (window != null && bounds != null) {
                AquaUtils.execute(window,
                        wptr -> showNativeView(vptr, wptr, bounds));
            } else {
                hideNativeView(vptr);
            }
        }
    }

    private static long showNativeView(long vptr, long wptr, @NotNull VisibleBounds bounds) {
        Rectangle frame = bounds.frame;
        if (bounds.isClipped()) {
            Rectangle viewport = bounds.visibleBounds;
            // Compute the frame of the view in the coordinate space of the viewport.
            int cx = frame.x - viewport.x;
            int cy = frame.y - viewport.y;
            int cw = frame.width;
            int ch = frame.height;
            showNativeViewClipped(vptr, wptr, cx, cy, cw, ch, viewport.x, viewport.y, viewport.width, viewport.height);
        } else {
            // Always show it clipped so that we do not rearrange the view hierarchy.
            showNativeViewClipped(vptr, wptr, 0, 0, frame.width, frame.height, frame.x, frame.y, frame.width, frame.height);
        }
        return 0;
    }

    private class MyComponentTracker
        extends ComponentTracker {

        @Override
        protected void attached(@Nullable Window w) {
            update();
        }

        @Override
        protected void detached(@Nullable Window w) {
            update();
        }

        @Override
        protected void windowChanged(@Nullable Window oldWindow, @Nullable Window newWindow) {
            update();
        }

        @Override
        protected void visibleBoundsChanged(@Nullable Window window) {
            update();
        }
    }

    private static native void hideNativeView(long vptr);
    private static native void showNativeView(long vptr, long wptr, int x, int y, int w, int h);
    private static native void showNativeViewClipped(long vptr, long wptr, int cx, int cy, int cw, int ch, int x, int y, int w, int h);
}
