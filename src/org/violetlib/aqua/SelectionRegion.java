/*
 * Copyright (c) 2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.NotNull;

/**
 * A description of a full-width region corresponding to a selection.
 */

public class SelectionRegion {
    public final int y;
    public final int height;

    public SelectionRegion(int y, int height) {
        this.y = y;
        this.height = height;
    }

    public boolean matches(@NotNull SelectionRegion r) {
        return y == r.y && height == r.height;
    }
}
