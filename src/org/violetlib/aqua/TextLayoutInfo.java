/*
 * Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

/**
 *
 */

public class TextLayoutInfo {
    public final int width;
    public final int height;
    public final int leftSideBearing;
    public final boolean isHTML;

    public TextLayoutInfo(int width, int height, int leftSideBearing, boolean isHTML) {
        this.width = width;
        this.height = height;
        this.leftSideBearing = leftSideBearing;
        this.isHTML = isHTML;
    }
}
