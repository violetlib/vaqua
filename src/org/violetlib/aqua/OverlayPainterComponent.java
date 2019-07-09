/*
 * Copyright (c) 2014-2019 Alan Snyder.
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
 * A base class for a component that paints an overlay over a base component. The overlay tracks the base component as
 * it moves, changes visibility, or is scrolled in or out of view.
 */
public abstract class OverlayPainterComponent extends JComponent {
    protected final @NotNull Insets margins;

    private final ComponentTracker tracker;

    private @Nullable Component base;           // the currently configured base component
    private @Nullable Rectangle baseBounds;     // the bounds of the base component in our coordinate space
    private @Nullable Rectangle visibleBounds;  // the bounds within our coordinate space where we may paint or null if not paintable
    private @Nullable Window baseWindow;        // the window last known to contain the base component

    private int layer;                  // the layer number of the layer containing the overlay painter component

    /**
     * Create a component for painting an overlay over a base component.
     * @param margins The margins that determine the size of this component. The size of this component is determined
     *                by adding the margins to the bounds of the base component.
     */
    public OverlayPainterComponent(@NotNull Insets margins) {

        this.margins = margins;
        this.layer = 1;

        // We need to know when the base component is added to a containment hierarchy, removed from a containment
        // hierarchy, reparented within a containment hierarhcy, or its bounds are changed.

        tracker = new ComponentTracker() {
            @Override
            protected void attached(@Nullable Window w) {
                if (w != null) {
                    OverlayPainterComponent.this.windowChanged(w);
                    OverlayPainterComponent.this.visibleBoundsChanged();
                }
            }

            @Override
            protected void windowChanged(@Nullable Window oldWindow, @Nullable Window newWindow) {
                OverlayPainterComponent.this.windowChanged(newWindow);
            }

            @Override
            protected void ancestorChanged() {
                OverlayPainterComponent.this.ancestorChanged();
            }

            @Override
            protected void visibleBoundsChanged(@Nullable Window window) {
                OverlayPainterComponent.this.visibleBoundsChanged();
            }
        };

        super.setOpaque(false);
        setVisible(false);
    }

    /**
     * Attach this component to the specified base component.
     * @param c The base component, or null to detach this component from any previous base component.
     */
    public void attach(@Nullable JComponent c) {
        if (base != c) {
            if (c != null) {
                base = c;
                tracker.attach(c);
                setVisible(true);
                repaint();
            } else {
                setVisible(false);
                if (base != null) {
                    tracker.attach(null);
                    base = null;
                }
            }
        }
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
     */
    private void windowChanged(@Nullable Window newWindow) {
        if (newWindow == baseWindow) {
            return;
        }

        JLayeredPane oldLayeredPane = baseWindow != null ? AquaUtils.getLayeredPane(baseWindow) : null;
        JLayeredPane newLayeredPane = newWindow != null ? AquaUtils.getLayeredPane(newWindow) : null;
        baseWindow = newWindow;

        if (oldLayeredPane != null) {
            Container p = getParent();
            if (p != null) {
                p.remove(this);
            }
        }

        if (newLayeredPane != null) {
            addToLayeredPane(newLayeredPane);
        }
    }

    private void ancestorChanged() {
        // I'm not sure this method can ever be called...
        if (baseWindow != null) {
            JLayeredPane layeredPane = AquaUtils.getLayeredPane(baseWindow);
            if (layeredPane != null) {
                addToLayeredPane(layeredPane);
            }
        }
    }

    private void addToLayeredPane(@NotNull JLayeredPane layeredPane) {
        assert base != null;
        int componentLayer = AquaUtils.getComponentLayer(base);
        int overlayLayer = componentLayer + 1;
        if (layer != overlayLayer) {
            this.layer = overlayLayer;
            layeredPane.setLayer(this, layer);
            layeredPane.add(this);
            visibleBoundsChanged();
        }
    }

    /**
     * Update our information about the bounds of the base component in our coordinate space and the visible bounds
     * into which we are allowed to paint.
     */
    private void visibleBoundsChanged() {
        JRootPane rp = baseWindow != null ? AquaUtils.getRootPane(baseWindow) : null;
        if (rp == null || base == null || !base.isVisible()) {
            baseBounds = null;
            visibleBounds = null;
            return;
        }

        Dimension baseSize = base.getSize();
        if (baseSize.width == 0 || baseSize.height == 0) {
            baseBounds = null;
            visibleBounds = null;
            return;
        }

        JLayeredPane lp = rp.getLayeredPane();
        Point baseLoc = SwingUtilities.convertPoint(base.getParent(), base.getLocation(), lp);

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

    @Override
    public final void paintComponent(@NotNull Graphics g) {
        if (visibleBounds != null) {
            assert baseBounds != null;
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
    protected abstract void internalPaint(@NotNull Graphics2D g);

    /**
     * Determine the bounds within which it is acceptable to paint an overlay for a given base component. The bounds are
     * normally the bounds of the root pane. However, if the base component is within a viewport view, then the bounds
     * are constrained by the viewport.
     *
     * @param base The base component.
     * @return the visible bounds, as defined above, in the coordinate space of the base component, or null if the
     * component is not visible.
     */
    protected static Rectangle getVisibleBounds(@NotNull Component base) {
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
