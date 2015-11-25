/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.HierarchyBoundsListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;

/**
 * A base class for an object tracks a component as
 * it moves, changes visibility, or is scrolled in or out of view.
 */

public abstract class ComponentTracker
{
    private final HierarchyListener hierarchyListener;
    private final HierarchyBoundsListener hierarchyBoundsListener;
    private final ComponentListener componentListener;

    private Component tracked;              // the currently configured component being tracked
    private Window window;                  // the window containing the tracked component

    /**
     * Create an object that tracks a component.
     */
    public ComponentTracker() {

        // We need to know when the component is added to a containment hierarchy, removed from a containment hierarchy,
        // or its bounds are changed.

        hierarchyListener = new HierarchyListener() {
            public void hierarchyChanged(HierarchyEvent e) {
                long flags = e.getChangeFlags();

                if ((flags & (HierarchyEvent.PARENT_CHANGED)) != 0) {
                    Window w = SwingUtilities.getWindowAncestor(tracked);
                    if (!Objects.equals(window, w)) {
                        Window oldWindow = window;
                        window = w;
                        windowChanged(oldWindow, window);
                    }
                }

                if ((flags & (HierarchyEvent.DISPLAYABILITY_CHANGED)) != 0) {
                    windowChanged(window, window);
                }

                if ((flags & (HierarchyEvent.SHOWING_CHANGED)) != 0) {
                    visibleBoundsChanged(window);
                }
            }
        };

        hierarchyBoundsListener = new HierarchyBoundsListener() {
            public void ancestorMoved(HierarchyEvent e) {
                visibleBoundsChanged(window);
            }

            public void ancestorResized(HierarchyEvent e) {
                visibleBoundsChanged(window);
            }
        };

        componentListener = new ComponentListener() {
            public void componentResized(ComponentEvent e) {
                visibleBoundsChanged(window);
            }

            public void componentMoved(ComponentEvent e) {
                visibleBoundsChanged(window);
            }

            public void componentShown(ComponentEvent e) {
                visibleBoundsChanged(window);
            }

            public void componentHidden(ComponentEvent e) {
                visibleBoundsChanged(window);
            }
        };
    }

    /**
     * Specify the component to be tracked.
     * @param c The component, or null to detach this tracker from any previous component.
     */
    public void attach(JComponent c) {
        if (tracked != c) {
            if (tracked != null) {
                tracked.removeHierarchyListener(hierarchyListener);
                tracked.removeHierarchyBoundsListener(hierarchyBoundsListener);
                tracked.removeComponentListener(componentListener);
                tracked = null;
                window = null;
            }

            if (c != null) {
                tracked = c;
                tracked.addHierarchyListener(hierarchyListener);
                tracked.addHierarchyBoundsListener(hierarchyBoundsListener);
                tracked.addComponentListener(componentListener);
                window = SwingUtilities.getWindowAncestor(tracked);
            }
        }

        visibleBoundsChanged(window);
    }

    /**
     * Called when the component is removed from a window, added to a window, or its displayability changed.
     *
     * @param oldWindow The window previously containing the component, or null if none.
     * @param newWindow The window now containing the component, or null if none. Normally oldWindow and newWindow
     * are different. However, they may be the same if the window containing the component becomes displayable.
     */
    protected void windowChanged(Window oldWindow, Window newWindow) {
    }

    /**
     * Called when the component size or location in the window changes or its visibility changes.
     *
     * @param window The window containing the component, or null if none.
     */
    protected void visibleBoundsChanged(Window window) {
    }
}
