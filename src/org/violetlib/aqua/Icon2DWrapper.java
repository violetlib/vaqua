/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 *
 */

public class Icon2DWrapper
  implements Icon2D
{
    public static @NotNull Icon2D of(@NotNull Icon icon)
    {
        return new Icon2DWrapper(icon);
    }

    private final @NotNull Icon icon;

    private Icon2DWrapper(@NotNull Icon icon)
    {
        this.icon = icon;
    }

    @Override
    public void paintIcon(@NotNull Graphics2D g, double x, double y, @Nullable Color c)
    {
        Icon p = Icons.processTemplateIcon(icon, c);
        p.paintIcon(null, g, (int) Math.round(x), (int) Math.round(y));
    }

    @Override
    public double getIconWidth()
    {
        return icon.getIconWidth();
    }

    @Override
    public double getIconHeight()
    {
        return icon.getIconHeight();
    }
}
