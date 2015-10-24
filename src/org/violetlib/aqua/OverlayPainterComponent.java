/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * A base class for a component that paints an overlay over a base component. The overlay tracks the base component as
 * it moves, changes visibility, or is scrolled in or out of view.
 */
public abstract class OverlayPainterComponent extends JComponent {
    protected final Insets margins;
    private final Integer layer;

    private final HierarchyListener baseHierarchyListener;
    private final HierarchyBoundsListener baseHierarchyBoundsListener;
    private final ComponentListener baseComponentListener;

    private Component base;             // the currently configured base component
    private Rectangle baseBounds;       // the bounds of the base component in our coordinate space
    private Rectangle visibleBounds;    // the bounds within our coordinate space where we may paint or null if not paintable

    /**
     * Create a component for painting an overlay over a base component.
     * @param margins The margins that determine the size of this component. The size of this component is determined
     *               by adding the margins to the bounds of the base component.
     * @param layer The layer in the layered pane to use for this component.
     */
    public OverlayPainterComponent(Insets margins, int layer) {

        this.margins = margins;
        this.layer = layer;

        // We need to know when the base component is added to a containment hierarchy, removed from a containment
        // hierarchy, or its bounds are changed.

        baseHierarchyListener = new HierarchyListener() {
            public void hierarchyChanged(HierarchyEvent e) {
                long flags = e.getChangeFlags();

                boolean isUpdated = updateHierarchy();

                if (isUpdated || (flags & HierarchyEvent.SHOWING_CHANGED) != 0) {
                    updateBounds();
                }
            }
        };

        baseHierarchyBoundsListener = new HierarchyBoundsListener() {
            public void ancestorMoved(HierarchyEvent e) {
                updateBounds();
            }

            public void ancestorResized(HierarchyEvent e) {
                updateBounds();
            }
        };

        baseComponentListener = new ComponentListener() {
            public void componentResized(ComponentEvent e) {
                updateBounds();
            }

            public void componentMoved(ComponentEvent e) {
                updateBounds();
            }

            public void componentShown(ComponentEvent e) {
                updateBounds();
            }

            public void componentHidden(ComponentEvent e) {
                updateBounds();
            }
        };

        super.setOpaque(false);
    }

    /**
     * Attach this component to the specified base component.
     * @param c The base component, or null to detach this component from any previous base component.
     */
    public void attach(JComponent c) {
        if (base != c) {
            if (base != null) {
                base.removeHierarchyListener(baseHierarchyListener);
                base.removeHierarchyBoundsListener(baseHierarchyBoundsListener);
                base.removeComponentListener(baseComponentListener);
                base = null;
            }

            if (c != null) {
                base = c;
                base.addHierarchyListener(baseHierarchyListener);
                base.addHierarchyBoundsListener(baseHierarchyBoundsListener);
                base.addComponentListener(baseComponentListener);
            }
        }

        if (base != null) {
            updateHierarchy();
        }

        updateBounds();
        repaint();
    }

    /**
     * Because the pane may be painted using transparency, it must not be made opaque.
     */
    public final void setOpaque(boolean b) {
    }

    // When looking for the component under the mouse, e.g. to select a cursor, we want to be invisible.
    @Override
    public boolean contains(int x, int y) {
        return false;
    }

    /**
     * Ensure that this component is properly located in the containment hierarchy that contains the base component.
     * Return true if and only if this component was moved to a new containment hierarchy or to a new place in the
     * current containment hierarchy.
     */
    private boolean updateHierarchy() {
        JRootPane orp = getOurRootPane();
        JRootPane nrp = getDesiredRootPane();

        if (orp == nrp) {
            return false;
        }

        if (orp != null) {
            Container p = getParent();
            p.remove(this);
        }

        if (nrp != null) {
            JLayeredPane lp = nrp.getLayeredPane();
            lp.add(this, layer);
        }

        return true;
    }

    /**
     * Update our information about the bounds of the base component in our coordinate space and the visible bounds
     * into which we are allowed to paint.
     */
    private void updateBounds() {
        JRootPane rp = getOurRootPane();
        if (rp == null || base == null) {
            baseBounds = null;
            visibleBounds = null;
            return;
        }

        JLayeredPane lp = rp.getLayeredPane();
        Point baseLoc = SwingUtilities.convertPoint(base.getParent(), base.getLocation(), lp);
        Dimension baseSize = base.getSize();

        int x = baseLoc.x - margins.left;
        int y = baseLoc.y - margins.top;
        int width = baseSize.width + margins.left + margins.right;
        int height = baseSize.height + margins.top + margins.bottom;

        if (x != getX() || y != getY() || width != getWidth() || height != getHeight()) {
            super.setBounds(x, y, width, height);
            repaint();
        }

        baseBounds = new Rectangle(margins.left, margins.top, baseSize.width, baseSize.height);
        visibleBounds = getVisibleBounds(base);
        if (visibleBounds != null) {
            visibleBounds = new Rectangle(visibleBounds.x + margins.left, visibleBounds.y + margins.top,
                    visibleBounds.width, visibleBounds.height);
        }
    }

    /**
     * Return the root pane associated with the base component.
     */
    private JRootPane getDesiredRootPane() {
        if (base == null) {
            return null;
        }

        Window w = SwingUtilities.getWindowAncestor(base);
        if (w instanceof RootPaneContainer) {
            RootPaneContainer rpc = (RootPaneContainer) w;
            return rpc.getRootPane();
        }

        return null;
    }

    /**
     * Return the root pane associated with this component.
     */
    private JRootPane getOurRootPane() {
        Window w = SwingUtilities.getWindowAncestor(this);
        if (w instanceof RootPaneContainer) {
            RootPaneContainer rpc = (RootPaneContainer) w;
            return rpc.getRootPane();
        }

        return null;
    }

    public final void paintComponent(Graphics g) {
        if (visibleBounds != null) {
            Graphics2D gg = (Graphics2D) g.create();
            // Updating the bounds may make the user clip obsolete, so we remove it.
            gg.setClip(-1000, -1000, 1000000, 1000000);
            gg.clip(visibleBounds);
            gg.translate(baseBounds.x, baseBounds.y);
            internalPaint(gg);
        }
    }

    /**
     * Paint the overlay for the current base component.
     * @param g The graphics context. The top left corner of the component is at the origin of the base component.
     */
    protected abstract void internalPaint(Graphics2D g);

    /**
     * Determine the bounds within which it is acceptable to paint an overlay for a given base component. The bounds are
     * normally the bounds of the root pane. However, if the base component is within a viewport view, then the bounds
     * are constrained by the viewport.
     *
     * @param base The base component.
     * @return the visible bounds, as defined above, in the coordinate space of the base component,
     * or null if the component is not visible.
     */
    protected static Rectangle getVisibleBounds(Component base) {
        int x = 0;
        int y = 0;

        Component c = base;
        for (;;) {
            if (!c.isVisible()) {
                return null;
            }

            if (c instanceof JRootPane) {
                Dimension size = c.getSize();
                return new Rectangle(x, y, size.width, size.height);
            }

            if (c instanceof JViewport) {
                JViewport p = (JViewport) c;
                Rectangle bounds = p.getVisibleRect();
                return new Rectangle(x + bounds.x, y + bounds.y, bounds.width, bounds.height);
            }

            Container parent = c.getParent();
            if (parent == null) {
                // should not happen
                return null;
            }

            x -= c.getX();
            y -= c.getY();
            c = parent;
        }
    }
}
