/*
 * Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.*;

import static javax.swing.SwingConstants.*;

/**
 * Options for supported arrangements of an icon and text in a button;
 */
public enum CompoundLabelAlignment {
    VERTICAL_TOP_TEXT(true, true),
    VERTICAL_BOTTOM_TEXT(true, false),
    HORIZONTAL_LEFT_TEXT(false, true),    // horizontal, text to the left of the icon
    HORIZONTAL_RIGHT_TEXT(false, false);  // horizontal, text to the right of the icon

    public static @NotNull CompoundLabelAlignment create(int horizontalTextPosition, int verticalTextPosition, boolean isLTR)
    {
        if (verticalTextPosition == TOP) {
            return VERTICAL_TOP_TEXT;
        }
        if (verticalTextPosition == BOTTOM) {
            return VERTICAL_BOTTOM_TEXT;
        }
        if (horizontalTextPosition == LEFT) {
            return HORIZONTAL_LEFT_TEXT;
        }
        if (horizontalTextPosition == RIGHT) {
            return HORIZONTAL_RIGHT_TEXT;
        }
        if (horizontalTextPosition == LEADING) {
            return isLTR ? HORIZONTAL_LEFT_TEXT : HORIZONTAL_RIGHT_TEXT;
        }
        if (horizontalTextPosition == TRAILING) {
            return isLTR ? HORIZONTAL_RIGHT_TEXT : HORIZONTAL_LEFT_TEXT;
        }
        return isLTR ? HORIZONTAL_LEFT_TEXT : HORIZONTAL_RIGHT_TEXT;
    }

    public static @NotNull CompoundLabelAlignment createLeadingText(boolean isLTR)
    {
        return isLTR ? HORIZONTAL_LEFT_TEXT : HORIZONTAL_RIGHT_TEXT;
    }

    public static @NotNull CompoundLabelAlignment createTrailingText(boolean isLTR)
    {
        return isLTR ? HORIZONTAL_RIGHT_TEXT : HORIZONTAL_LEFT_TEXT;
    }

    private final boolean isVertical;
    private final boolean isTopOrLeftText;

    CompoundLabelAlignment(boolean isVertical, boolean isTopOrLeftText)
    {
        this.isVertical = isVertical;
        this.isTopOrLeftText = isTopOrLeftText;
    }

    public boolean isVerticalArrangement()
    {
        return isVertical;
    }

    public boolean isTopText()
    {
        return isVertical && isTopOrLeftText;
    }

    public boolean isLeftText()
    {
        return !isVertical && isTopOrLeftText;
    }
}
