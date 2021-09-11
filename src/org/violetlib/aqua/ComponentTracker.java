/*
 * Copyright (c) 2015-2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.*;
import java.util.Objects;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A base class for an object tracks a component as
 * it moves, changes visibility, or is scrolled in or out of view.
 */

public abstract class ComponentTracker
{
    private final HierarchyListener hierarchyListener;
    private final HierarchyBoundsListener hierarchyBoundsListener;
    private final ComponentListener componentListener;

    private @Nullable Component tracked;    // the currently configured component being tracked
    private @Nullable Window window;        // the window containing the tracked component
    private boolean windowIsDisplayable;    // the last known displayability of the window

    /**
     * Create an object that tracks a component.
     */
    public ComponentTracker() {

        // We need to know when the component is added to a containment hierarchy, removed from a containment hierarchy,
        // or its bounds are changed.

        hierarchyListener = e -> {
            long flags = e.getChangeFlags();

            if ((flags & (HierarchyEvent.PARENT_CHANGED)) != 0) {
                Window w = SwingUtilities.getWindowAncestor(tracked);
                if (!Objects.equals(window, w)) {
                    Window oldWindow = window;
                    window = w;
                    windowIsDisplayable = w != null && w.isDisplayable();
                    windowChanged(oldWindow, window);
                } else {
                    ancestorChanged();
                }
            }

            if ((flags & (HierarchyEvent.DISPLAYABILITY_CHANGED)) != 0) {
                // Displayability change events are generated for adding and removing the tracked component.
                // These events are redundant with parent changed.
                // The only time we want to call windowChanged() with identical arguments is when the
                // window is not null and its displayability has changed.
                Window w = SwingUtilities.getWindowAncestor(tracked);
                if (w != null && w == window) {
                    boolean isDisplayable = w.isDisplayable();
                    if (isDisplayable != windowIsDisplayable) {
                        windowIsDisplayable = isDisplayable;
                        windowChanged(window, window);
                    }
                }
            }

            if ((flags & (HierarchyEvent.SHOWING_CHANGED)) != 0) {
                visibleBoundsChanged(window);
            }
        };

        hierarchyBoundsListener = new HierarchyBoundsListener() {
            public void ancestorMoved(@NotNull HierarchyEvent e) {
                visibleBoundsChanged(window);
            }

            public void ancestorResized(@NotNull HierarchyEvent e) {
                visibleBoundsChanged(window);
            }
        };

        componentListener = new ComponentListener() {
            public void componentResized(@NotNull ComponentEvent e) {
                visibleBoundsChanged(window);
            }

            public void componentMoved(@NotNull ComponentEvent e) {
                visibleBoundsChanged(window);
            }

            public void componentShown(@NotNull ComponentEvent e) {
                visibleBoundsChanged(window);
            }

            public void componentHidden(@NotNull ComponentEvent e) {
                visibleBoundsChanged(window);
            }
        };
    }

    /**
     * Specify the component to be tracked.
     * @param c The component, or null to detach this tracker from any previous component.
     */
    public void attach(@Nullable Component c) {
        if (tracked != c) {
            if (tracked != null) {
                Window w = window;
                tracked.removeHierarchyListener(hierarchyListener);
                tracked.removeHierarchyBoundsListener(hierarchyBoundsListener);
                tracked.removeComponentListener(componentListener);
                tracked = null;
                window = null;
                windowIsDisplayable = false;
                detached(w);
            }

            if (c != null) {
                tracked = c;
                tracked.addHierarchyListener(hierarchyListener);
                tracked.addHierarchyBoundsListener(hierarchyBoundsListener);
                tracked.addComponentListener(componentListener);
                window = SwingUtilities.getWindowAncestor(tracked);
                windowIsDisplayable = window != null && window.isDisplayable();
                attached(window);
            }
        }
    }

    /**
     * Called when this tracker is detached from a component.
     * @param w The window previously containing the component, or null if none.
     */
    protected void detached(@Nullable Window w) {
    }

    /**
     * Called when this tracker is attached to a component.
     * @param w The window containing the component, or null if none.
     */
    protected void attached(@Nullable Window w) {
    }

    /**
     * Called when the component is removed from a window, added to a window, or its displayability changed.
     *
     * @param oldWindow The window previously containing the component, or null if none.
     * @param newWindow The window now containing the component, or null if none. Normally oldWindow and newWindow
     * are different. However, they may be the same if the window containing the component becomes displayable.
     */
    protected void windowChanged(@Nullable Window oldWindow, @Nullable Window newWindow) {
    }

    /**
     * Called when the component is moved within a window hierarchy.
     * (Can this ever happen? Not if move is implemented as remove followed by add.)
     */
    protected void ancestorChanged() {
    }

    /**
     * Called when the component size or location in the window changes or its visibility changes.
     *
     * @param window The window containing the component, or null if none.
     */
    protected void visibleBoundsChanged(@Nullable Window window) {
    }
}
