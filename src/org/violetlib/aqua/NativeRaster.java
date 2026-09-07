/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A representation of an image raster obtained from native code.
 */

public final class NativeRaster
{
    /**
     * Create an object containing a rectangular array of pixels.
     * @param width The width of the array.
     * @param height The height of the array.
     * @param data A linear array containing the pixels.
     */

    public static @NotNull NativeRaster create(int width, int height, int @Nullable [] data)
    {
        if (width < 0) {
            throw new IllegalArgumentException("Invalid width: " + width);
        }
        if (height < 0) {
            throw new IllegalArgumentException("Invalid height: " + height);
        }
        if (width == 0 || height == 0) {
            return new NativeRaster(0, 0, new int[0]);
        }
        if (data == null) {
            throw new IllegalArgumentException("Data array is required");
        }
        if (data.length != width * height) {
            throw new IllegalArgumentException("Inconsistent data: " + width + " " + height + " " + data.length);
        }
        return new NativeRaster(width, height, data);
    }

    public final int width;
    public final int height;
    public final int @NotNull [] data;

    private NativeRaster(int width, int height, int @NotNull [] data)
    {
        this.width = width;
        this.height = height;
        this.data = data;
    }

    public boolean isEmpty()
    {
        return width == 0;
    }
}
