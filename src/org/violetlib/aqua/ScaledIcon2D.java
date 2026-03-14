/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.*;

/**
 * An icon of a specified size that paints the specified source icon.
 * If the source does not fit in the specified size, it is linearly scaled (downward) as needed.
 * If the icon (after scaling) does not match the specified size, it is centered.
 */
public class ScaledIcon2D
  implements Icon2D
{
    /**
     * Create an icon of the specified size that paints the specified source icon inside its bounds.
     * @param source The source icon.
     * @param width The width of this icon.
     * @param height The height of this icon.
     */
    public static @NotNull Icon2D scale(@NotNull Icon2D source, double width, double height)
    {
        double w = source.getIconWidth();
        double h = source.getIconHeight();
        if (w == width && h == height) {
            return source;
        }
        return new ScaledIcon2D(source, width, height);
    }

    private final @NotNull Icon2D source;
    private final double width;
    private final double height;

    private ScaledIcon2D(@NotNull Icon2D source, double width, double height)
    {
        this.source = source;
        this.width = width;
        this.height = height;
    }

    @Override
    public void paintIcon(@NotNull Graphics2D g, double x, double y, @Nullable Color c)
    {
        double iconWidth = source.getIconWidth();
        double iconHeight = source.getIconHeight();

        double scaleFactor = Math.min(1, Math.min(width / iconWidth, height / iconHeight));
        double scaledWidth = scaleFactor * iconWidth;
        double scaledHeight = scaleFactor * iconHeight;

        if (width > scaledWidth) {
            x += (width - scaledWidth) / 2;
        }
        if (height > scaledHeight) {
            y += (height - scaledHeight) / 2;
        }

        g = (Graphics2D) g.create();
        g.translate(x, y);
        if (iconWidth != width || iconHeight != height) {
            g.scale(scaleFactor, scaleFactor);
        }
        source.paintIcon(g, 0, 0, c);
        g.dispose();
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
}
