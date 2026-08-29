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
import javax.swing.*;

import org.jetbrains.annotations.*;

public class Icons
{
    /**
     * Return the width needed to display an icon in a fixed height area. The width is the width of the icon,
     * possibly scaled down so that the icon height does not exceed the fixed height.
     * @param iconSize The icon size.
     * @param fixedHeight The fixed height.
     * @return the width of the icon as possibly scaled to fit in the fixed height area.
     */
    public static int getIconWidthInFixedHeight(@NotNull Dimension iconSize, int fixedHeight) {
        int iconHeight = iconSize.height;
        int iconWidth = iconSize.width;
        float scaleFactor = getScaleFactor(iconHeight, fixedHeight);
        if (scaleFactor == 1) {
            return iconWidth;
        }
        float scaledWidth = iconWidth * scaleFactor;
        return AquaUtils.ceil(scaledWidth);
    }

    /**
     * Return the width needed to display an icon in a fixed height area. The width is the width of the icon,
     * possibly scaled down so that the icon height does not exceed the fixed height.
     * @param iconSize The icon size.
     * @param fixedHeight The fixed height.
     * @return the width of the icon as possibly scaled to fit in the fixed height area.
     */
    public static float getIconWidthInFixedHeight(@NotNull Dimension iconSize, float fixedHeight) {
        int iconHeight = iconSize.height;
        int iconWidth = iconSize.width;
        float scaleFactor = getScaleFactor(iconHeight, fixedHeight);
        if (scaleFactor == 1) {
            return iconWidth;
        }
        float scaledWidth = iconWidth * scaleFactor;
        return AquaUtils.ceil(scaledWidth);
    }

    private static float getScaleFactor(int actual, int bounds) {
        if (actual <= bounds) {
            return 1;
        }
        return bounds / (float) actual;
    }

    private static float getScaleFactor(float actual, float bounds) {
        if (actual <= bounds) {
            return 1;
        }
        return bounds / actual;
    }

    public static @NotNull Icon2D scale(@NotNull Icon2D source, double width, double height) {
        return ScaledIcon2D.scale(source, width, height);
    }

    public static @NotNull Icon processTemplateIcon(@NotNull Icon icon, @Nullable Color color) {
        if (color != null && !(icon instanceof AquaButtonIcon) && AquaImageFactory.isTemplateIcon(icon)) {
            return AquaImageFactory.getProcessedImage(icon, color);
        }
        return icon;
    }

    public static void paintIcon(@NotNull Graphics2D g,
                                 @NotNull AbstractButton b,
                                 @NotNull Icon icon,
                                 @NotNull Rectangle2D iconRect,
                                 @Nullable Color iconColor) {
        icon = processTemplateIcon(icon, iconColor);

        int iconWidth = icon.getIconWidth();
        int iconHeight = icon.getIconHeight();

        double x = iconRect.getX();
        double y = iconRect.getY();
        double w = iconRect.getWidth();
        double h = iconRect.getHeight();

        double scaleFactor = Math.min(1, Math.min(w / iconWidth, h / iconHeight));
        double scaledWidth = scaleFactor * iconWidth;
        double scaledHeight = scaleFactor * iconHeight;

        if (w > scaledWidth) {
            x += (w - scaledWidth) / 2;
        }
        if (h > scaledHeight) {
            y += (h - scaledHeight) / 2;
        }

        g = (Graphics2D) g.create();
        g.translate(x, y);
        if (iconWidth != w || iconHeight != h) {
            g.scale(scaleFactor, scaleFactor);
        }

        icon.paintIcon(b, g, 0, 0);
        g.dispose();
    }
}
