/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.Rectangle2D;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.Configuration;

/**
 * A description of a natively painted component that can be used as an icon.
 * A split toolbar item contains a virtual button.
 */
public class VirtualComponent
  implements Icon2D
{
    private final @NotNull Configuration cc;
    private final @NotNull AquaUIPainter painter;
    private final @NotNull AquaAppearance appearance;
    private final @Nullable Icon2D icon;
    private final @Nullable Insetter iconInsets;
    private final double width;
    private final double height;

    /**
     * Create a virtual component that can be painted.
     * @param g The native component configuration.
     * @param painter The native painter.
     * @param appearance The appearance.
     * @param icon The icon to include in the component (optional).
     * @param iconInsets The insets that define the icon area (optional).
     * @param width The width of the virtual component.
     * @param height The height of the virtual component.
     */
    public VirtualComponent(@NotNull Configuration g,
                            @NotNull AquaUIPainter painter,
                            @NotNull AquaAppearance appearance,
                            @Nullable Icon2D icon,
                            @Nullable Insetter iconInsets,
                            double width,
                            double height)
    {
        this.cc = g;
        this.painter = painter;
        this.appearance = appearance;
        this.icon = icon;
        this.iconInsets = iconInsets;
        this.width = width;
        this.height = height;
    }

    @Override
    public void paintIcon(@NotNull Graphics2D g, double x, double y, @Nullable Color c)
    {
        int xx = (int) Math.floor(x);
        int yy = (int) Math.floor(y);
        int ww = (int) Math.ceil(width);
        int hh = (int) Math.ceil(height);
        AquaUtils.configure(painter, appearance, ww, hh);
        org.violetlib.jnr.Painter p = painter.getPainter(cc);
        p.paint(g, xx, yy);

        if (icon != null) {
            Icon2D iconToPaint = icon;
            double iconX = x;
            double iconY = y;
            if (iconInsets != null) {
                Rectangle2D contentBounds = iconInsets.apply2D(width, height);
                iconToPaint = Icons.scale(icon, contentBounds.getWidth(), contentBounds.getHeight());
                iconX += contentBounds.getX();
                iconY += contentBounds.getY();
            }
            iconToPaint.paintIcon(g, iconX, iconY, c);
        }
    }

    @Override
    public double getIconWidth()
    {
        return width;
    }

    @Override
    public double getIconHeight()
    {
        return height;
    }

    public @NotNull Dimension size()
    {
        return new Dimension(AquaUtils.ceil(width), AquaUtils.ceil(height));
    }
}
