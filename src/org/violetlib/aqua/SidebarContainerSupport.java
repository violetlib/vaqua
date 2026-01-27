/*
 * Copyright (c) 2025-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.geom.RoundRectangle2D;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.*;

/**
 * Support for a top level sidebar container (list, tree, or scroll pane containing a list or tree).
 * This implementation mimics the macOS 26 style that uses a rounded rectangle border.
 */

public class SidebarContainerSupport
  implements Border, UIResource
{
    private final int radius = 36;
    private final int inset = 1;

    private final @NotNull JComponent container;
    private @Nullable Border oldBorder;

    private @Nullable AquaSplitPaneUI configuredSplitPaneUI;

    private final @NotNull HierarchyListener myHierarchyListener = new HierarchyListener() {
        @Override
        public void hierarchyChanged(@NotNull HierarchyEvent e) {
            if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0) {
                if (e.getChanged() == container) {
                    updateSplitPaneParent();
                }
            }
        }
    };

    // This code supports painting a rounded rectangle border and clipping the content within that border.

    public SidebarContainerSupport(@NotNull JComponent container)
    {
        this.container = container;
        oldBorder = container.getBorder();
        container.setBorder(this);
        updateSplitPaneParent();
        container.addHierarchyListener(myHierarchyListener);
    }

    /**
     * Specify the border to be installed when this border is uninstalled.
     */

    public void replaceUninstallBorder(@Nullable Border b)
    {
        oldBorder = b;
    }

    /**
     * Return a new graphics context to use when painting the container.
     * This method retains data used by other methods.
     *
     * @param o The original graphics context given to the UI.
     * @return a graphics context to use instead of {@code o} to paint the container.
     */
    public @NotNull Graphics2D setupContainerGraphics(@NotNull Graphics o, @NotNull AppearanceContext ac)
    {
        int w = container.getWidth();
        int h = container.getHeight();
        Shape outerShape = new RoundRectangle2D.Double(0, 0, w, h, radius, radius);

        Graphics2D g = (Graphics2D) o.create();
        g.clip(outerShape);
        AquaColors.SIDEBAR_CONTAINER_COLORS.configureForContainer();
        Color color = AquaColors.SIDEBAR_CONTAINER_COLORS.getBackground(ac);
        g.setColor(color);
        Rectangle rect = new Rectangle(0, 0, container.getWidth(), container.getHeight());
        AquaUtils.fillAntiAliased(g, rect);
        return g;
    }

    public void uninstall()
    {
        container.setBorder(oldBorder);
        container.removeHierarchyListener(myHierarchyListener);
        if (configuredSplitPaneUI != null) {
            configuredSplitPaneUI.configureAsSidebar(0);
            configuredSplitPaneUI = null;
        }
    }

    @Override
    public void paintBorder(Component c, Graphics o, int x, int y, int width, int height) {
        PaintingContext pc = PaintingContext.getDefault();
        boolean isActive = AquaFocusHandler.isActive((JComponent)c);

        if (isActive || pc.appearance.isHighContrast()) {
            float d = 0.5f;
            float r = radius;
            Shape borderPath = new RoundRectangle2D.Float(x+d, y+d, width-2*d, height-2*d, r, r);
            Graphics2D g = (Graphics2D) o;

            Color color = pc.appearance.getColor(isActive ? "sidebarBorder" : "sidebarBorder_inactive");
            g.setColor(color);
            g.setStroke(new BasicStroke(0.5f));
            AquaUtils.drawAntiAliased(g, borderPath);
        }
    }

    @Override
    public Insets getBorderInsets(Component c) {
        AquaViewStyleContainerUI ui = AquaUtils.getUI(c, AquaViewStyleContainerUI.class);
        if (ui != null) {
            // This border needs to simulate the side insets of the inset view border.
            Insets insets = ui.getInsetViewInsets();
            return new Insets(inset, inset+insets.left, inset, inset+insets.right);
        }
        return new Insets(inset, inset, inset, inset);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    public @Nullable Insetter2D getInsetter()
    {
        float d = 2;
        float r = radius + 0.5f;
        return (w, h) -> new RoundRectangle2D.Float(d, d, w-2*d, h-2*d, r, r);
    }

    private void updateSplitPaneParent()
    {
        Container parent = container.getParent();
        AquaSplitPaneUI splitPaneUI = AquaUtils.getUI(parent, AquaSplitPaneUI.class);
        int option = 0;
        if (splitPaneUI != null) {
            JSplitPane sp = splitPaneUI.getSplitPane();
            if (container == sp.getLeftComponent()) {
                option = AquaSplitPaneUI.SIDEBAR_LEFT;
            } else if (container == sp.getRightComponent()) {
                option = AquaSplitPaneUI.SIDEBAR_RIGHT;
            } else {
                Utils.logDebug("Unexpected: sidebar is not a known component of its split pane parent");
            }
        }
        if (splitPaneUI != configuredSplitPaneUI) {
            if (configuredSplitPaneUI != null) {
                configuredSplitPaneUI.configureAsSidebar(0);
                splitPaneUI = null;
            }
        }
        configuredSplitPaneUI = splitPaneUI;
        if (configuredSplitPaneUI != null) {
            configuredSplitPaneUI.configureAsSidebar(option);
        }
    }
}
