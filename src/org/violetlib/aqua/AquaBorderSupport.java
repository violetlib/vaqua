/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneDivider;

import org.jetbrains.annotations.*;

/**
 *
 */

public final class AquaBorderSupport
{
    public static <B> @Nullable B get(@NotNull JComponent component, @NotNull Class<B> c)
    {
        Border b = component.getBorder();
        return b != null ? get(b, c) : null;
    }

    public static <B> @Nullable B get(@NotNull BasicSplitPaneDivider d, @NotNull Class<B> c)
    {
        Border b = d.getBorder();
        return b != null ? get(b, c) : null;
    }

    public static <B> @Nullable B get(@NotNull Border b, @NotNull Class<B> c)
    {
        if (c.isInstance(b)) {
            return c.cast(b);
        }
        return null;
    }
}
