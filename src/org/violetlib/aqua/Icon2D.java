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

public interface Icon2D
{
    static @NotNull Icon2D of(@NotNull Icon icon)
    {
        return Icon2DWrapper.of(icon);
    }

    static @Nullable Icon2D optional(@Nullable Icon icon)
    {
        return icon != null ? Icon2DWrapper.of(icon) : null;
    }

    void paintIcon(@NotNull Graphics2D g, double x, double y, @Nullable Color c);

    double getIconWidth();

    double getIconHeight();
}
