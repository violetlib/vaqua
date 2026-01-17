/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 *
 */

public final class PaintingContext
{
    public static @NotNull PaintingContext of(@NotNull AquaAppearance appearance)
    {
        return new PaintingContext(null, appearance, null);
    }

    static @NotNull PaintingContext create(@NotNull JComponent owner,
                                           @NotNull AquaAppearance appearance,
                                           @Nullable PaintingContext previous)
    {
        return new PaintingContext(owner, appearance, previous);
    }

    public final @Nullable JComponent owner;
    public final @NotNull AquaAppearance appearance;
    public final @Nullable PaintingContext previous;

    private PaintingContext(@Nullable JComponent owner,
                            @NotNull AquaAppearance appearance,
                            @Nullable PaintingContext previous)
    {
        this.owner = owner;
        this.appearance = appearance;
        this.previous = previous;
    }

    private static @Nullable PaintingContext current;

    public static @Nullable PaintingContext get()
    {
        return current;
    }

    public static @NotNull PaintingContext getDefault()
    {
        if (current != null) {
            return current;
        }
        return PaintingContext.of(AppearanceManager.getApplicationAppearance());
    }

    public static @NotNull PaintingContext push(@NotNull JComponent owner, @NotNull AquaAppearance appearance)
    {
        current = create(owner, appearance, current);
        return current;
    }

    public static void pop(@NotNull JComponent owner)
    {
        if (current != null && current.owner == owner) {
            current = current.previous;
        }
    }
}
