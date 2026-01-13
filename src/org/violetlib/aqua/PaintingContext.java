/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.*;

/**
 *
 */

public final class PaintingContext
{
    static @NotNull PaintingContext of(@NotNull AquaAppearance appearance)
    {
        return new PaintingContext(appearance);
    }

    public final @NotNull AquaAppearance appearance;

    private PaintingContext(@NotNull AquaAppearance appearance)
    {
        this.appearance = appearance;
    }
}
