/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.Component;
import java.awt.Container;
import java.awt.Rectangle;
import java.awt.Window;
import javax.swing.JComponent;
import javax.swing.JRootPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;

/**
 * This object manages a NSVisualEffectView whose bounds tracks the bounds of a component.
 */
public class VisualEffectView {
    private final JComponent component;
    private final int style;
    private long viewPointer;
    private Rectangle oldBounds;
    private Window window;
    private ComponentTracker tracker;

    /**
     * Create an object to manage a NSVisualEffectView.
     * @param c The component that the NSVisualEffectView will track.
     * @param style The style of the NSVisualEffectView.
     */
    public VisualEffectView(JComponent c, int style) {
        this.component = c;
        this.style = style;
        Window w = SwingUtilities.getWindowAncestor(c);
        if (w != null) {
            windowChanged(w);
        }
        tracker = new MyComponentTracker();
        tracker.attach(component);
    }

    public void dispose() {
        if (tracker != null) {
            tracker.attach(null);
            tracker = null;
        }
        if (window != null) {
            windowChanged(null);
        }
    }

    protected void windowChanged(Window newWindow) {
        // The new window must be displayable to install a visual effect view.
        if (newWindow != null && !newWindow.isDisplayable()) {
            newWindow = null;
        }

        if (newWindow != window) {
            if (window != null && viewPointer != 0) {
                int rc = AquaVibrantSupport.disposeVisualEffectView(viewPointer);
                if (rc != 0) {
                    System.err.println("disposeVisualEffectView failed");
                }
                viewPointer = 0;
            }

            if (newWindow != null) {
                window = newWindow;
                oldBounds = null;
                viewPointer = AquaVibrantSupport.createVisualEffectView(window, style);
                if (viewPointer == 0) {
                    System.err.println("createVisualEffectView failed");
                } else {
                    // remove the Java window background
                    AquaUtils.setTextured(window);
                }
            }
        }
    }

    protected void visibleBoundsChanged() {
        if (window != null && window.isValid()) {
            Rectangle bounds = getVisibleBounds(component);
            if (bounds != null) {
                setFrame(bounds.x, bounds.y, bounds.width, bounds.height);
            } else {
                setFrame(0, 0, 0, 0);
            }
        }
    }

    private void setFrame(int x, int y, int w, int h) {
        if (oldBounds == null || x != oldBounds.x || y != oldBounds.y || w != oldBounds.width || h != oldBounds.height) {
            oldBounds = new Rectangle(x, y, w, h);
            if (viewPointer != 0) {
                int rc = AquaVibrantSupport.setVisualEffectViewFrame(viewPointer, x, y, w, h);
                if (rc != 0) {
                    System.err.println("setVisualEffectViewFrame failed");
                }
            }
        }
    }

    private class MyComponentTracker extends ComponentTracker {
        @Override
        protected void windowChanged(Window oldWindow, Window newWindow) {
            VisualEffectView.this.windowChanged(newWindow);
        }

        @Override
        protected void visibleBoundsChanged(Window window) {
            if (window != null) {
                VisualEffectView.this.visibleBoundsChanged();
            }
        }
    }

    /**
     * Determine the visible bounds of the specified component in the window coordinate space. The bounds are normally
     * the bounds of the component. However, if the base component is within a viewport view, then the bounds are
     * constrained by the viewport.
     *
     * @param tc The component.
     * @return the visible bounds, as defined above, in the coordinate space of the window, or null if the component is
     *   not visible.
     */
    protected static Rectangle getVisibleBounds(Component tc) {
        Component c = tc;
        Rectangle bounds = c.getBounds();
        Window w = SwingUtilities.getWindowAncestor(c);

        for (;;) {
            if (!c.isVisible()) {
                return null;
            }

            if (c instanceof JRootPane) {
                break;
            }

            if (c instanceof JViewport && c != tc) {
                JViewport p = (JViewport) c;
                bounds = p.getBounds();
                return SwingUtilities.convertRectangle(p.getParent(), bounds, w);
            }

            Container parent = c.getParent();
            if (parent == null) {
                // should not happen
                return null;
            }

            c = parent;
        }

        return SwingUtilities.convertRectangle(tc.getParent(), bounds, w);
    }
}
