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
 * A description of the highlight for selected items in a list or similar container. Adjacent selected items may be
 * painted using a single highlight.
 */

public class SelectionHighlightDescription {

    public static @NotNull SelectionHighlightDescription of(int top, int left, int bottom, int right, int cornerRadius)
    {
        return new SelectionHighlightDescription(top, left, bottom, right, cornerRadius);
    }

    public static @NotNull SelectionHighlightDescription of(@NotNull Insets s, int cornerRadius)
    {
        return new SelectionHighlightDescription(s.top, s.left, s.bottom, s.right, cornerRadius);
    }

    public final int top;
    public final int left;
    public final int bottom;
    public final int right;
    public final int cornerRadius;  // zero if rectangular

    private SelectionHighlightDescription(int top, int left, int bottom, int right, int cornerRadius)
    {
        this.top = top;
        this.left = left;
        this.bottom = bottom;
        this.right = right;
        this.cornerRadius = cornerRadius;
    }
}
