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
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;

/**
 * An immutable description of constraints on a size in the form of an optional minimum or fixed width or height.
 */
public final class SizeConstraints
{
    private final int fixedWidth;
    private final int fixedHeight;
    private final int minimumWidth;  // could be derived from the fixed width
    private final int minimumHeight;  // could be derived from the fixed height

    private static final @NotNull SizeConstraints EMPTY = new SizeConstraints(0, 0, 0, 0);

    /**
     * Return a description with no constraints.
     */
    public static @NotNull SizeConstraints none()
    {
        return EMPTY;
    }

    /**
     * Return a constraint with a fixed width.
     * @param w The fixed width.
     * @throws IllegalArgumentException if the specified width is zero or negative.
     */
    public static @NotNull SizeConstraints fixedWidth(int w)
      throws IllegalArgumentException
    {
        if (w <= 0) {
            throw new IllegalArgumentException("Invalid width: " + w);
        }
        return new SizeConstraints(w, 0, 0, 0);
    }

    /**
     * Extend a constraint to have a fixed width.
     * @param w The fixed width.
     * @throws IllegalArgumentException if the specified width is zero or negative, or if the source has a constrained
     * width.
     */
    public @NotNull SizeConstraints andFixedWidth(int w)
    {
        if (w <= 0) {
            throw new IllegalArgumentException("Invalid width: " + w);
        }
        if (minimumWidth > 0) {
            throw new IllegalArgumentException("Width is already constrained");
        }
        return new SizeConstraints(w, fixedHeight, 0, minimumHeight);
    }

    /**
     * Return a constraint with a fixed height.
     * @param h The fixed height.
     * @throws IllegalArgumentException if the specified height is zero or negative.
     */
    public static @NotNull SizeConstraints fixedHeight(int h)
      throws IllegalArgumentException
    {
        if (h <= 0) {
            throw new IllegalArgumentException("Invalid height: " + h);
        }
        return new SizeConstraints(0, h, 0, 0);
    }

    /**
     * Extend a constraint to have a fixed height.
     * @param h The fixed height.
     * @throws IllegalArgumentException if the specified height is zero or negative, or if the source has a constrained
     * height.
     */
    public @NotNull SizeConstraints andFixedHeight(int h)
    {
        if (h <= 0) {
            throw new IllegalArgumentException("Invalid height: " + h);
        }
        if (minimumHeight > 0) {
            throw new IllegalArgumentException("Height is already constrained");
        }
        return new SizeConstraints(fixedWidth, h, minimumWidth, 0);
    }

    /**
     * Return a constraint with a minimum width.
     * @param w The minimum width.
     * @throws IllegalArgumentException if the specified width is zero or negative.
     */
    public static @NotNull SizeConstraints minWidth(int w)
    {
        if (w <= 0) {
            throw new IllegalArgumentException("Invalid width: " + w);
        }
        return new SizeConstraints(0, 0, w, 0);
    }

    /**
     * Extend a constraint to have a minimum width.
     * @param w The minimum width.
     * @throws IllegalArgumentException if the specified width is zero or negative, or if the source has a constrained
     * width.
     */
    public @NotNull SizeConstraints andMinWidth(int w)
    {
        if (w <= 0) {
            throw new IllegalArgumentException("Invalid width: " + w);
        }
        if (minimumWidth > 0) {
            throw new IllegalArgumentException("Width is already constrained");
        }
        return new SizeConstraints(0, fixedHeight, w, minimumHeight);
    }

    /**
     * Return a constraint with a minimum height.
     * @param h The minimum height.
     * @throws IllegalArgumentException if the specified height is zero or negative.
     */
    public static @NotNull SizeConstraints minHeight(int h)
    {
        if (h <= 0) {
            throw new IllegalArgumentException("Invalid height: " + h);
        }
        return new SizeConstraints(0, 0, 0, h);
    }

    /**
     * Extend a constraint to have a minimum height.
     * @param h The minimum height.
     * @throws IllegalArgumentException if the specified height is zero or negative, or if the source has a constrained
     * height.
     */
    public @NotNull SizeConstraints andMinHeight(int h)
    {
        if (h <= 0) {
            throw new IllegalArgumentException("Invalid height: " + h);
        }
        if (minimumHeight > 0) {
            throw new IllegalArgumentException("Height is already constrained");
        }
        return new SizeConstraints(fixedWidth, 0, minimumWidth, h);
    }

    public @NotNull SizeConstraints apply(@NotNull Insets s)
    {
        if (isEmpty()) {
            return this;
        }
        int w = Math.max(0, minimumWidth - (s.left + s.right));
        int h = Math.max(0, minimumHeight - (s.top + s.bottom));
        int fw = fixedWidth > 0 ? w : 0;
        int fh = fixedHeight > 0 ? h : 0;
        return new SizeConstraints(fw, fh, w, h);
    }

    public @NotNull SizeConstraints apply(@NotNull Insetter s)
    {
        if (isEmpty()) {
            return this;
        }
        int w = minimumWidth;
        int h = minimumHeight;
        Rectangle r = s.apply(w, h);
        w = Math.max(0, r.width);
        h = Math.max(0, r.height);
        int fw = fixedWidth > 0 ? w : 0;
        int fh = fixedHeight > 0 ? h : 0;
        return new SizeConstraints(fw, fh, w, h);
    }

    public static @NotNull SizeConstraints of(@NotNull LayoutInfo info)
    {
        SizeConstraints c = EMPTY;
        float w = info.getFixedVisualWidth();
        if (w > 0) {
            c = c.andFixedWidth((int) w);
        } else {
            w = info.getMinimumVisualWidth();
            if (w > 0) {
                c = c.andMinWidth((int) w);
            }
        }
        float h = info.getFixedVisualHeight();
        if (h > 0) {
            c = c.andFixedHeight((int) h);
        } else {
            h = info.getMinimumVisualHeight();
            if (h > 0) {
                c = c.andMinHeight((int) h);
            }
        }
        return c;
    }

    private SizeConstraints(int fixedWidth, int fixedHeight, int minimumWidth, int minimumHeight)
      throws IllegalArgumentException
    {
        if (fixedWidth < 0) {
            throw new IllegalArgumentException("Invalid negative fixed width");
        }

        if (fixedHeight < 0) {
            throw new IllegalArgumentException("Invalid negative fixed height");
        }

        if (minimumWidth < 0) {
            throw new IllegalArgumentException("Invalid negative minimum width");
        }

        if (minimumHeight < 0) {
            throw new IllegalArgumentException("Invalid negative minimum height");
        }

        if (minimumWidth > 0 && fixedWidth > 0 && minimumWidth != fixedWidth) {
            throw new IllegalArgumentException("Incompatible fixed and minimum width");
        }

        if (minimumHeight > 0 && fixedHeight > 0 && minimumHeight != fixedHeight) {
            throw new IllegalArgumentException("Incompatible fixed and minimum height");
        }

        this.fixedWidth = fixedWidth;
        this.fixedHeight = fixedHeight;
        this.minimumWidth = minimumWidth > 0 ? minimumWidth : fixedWidth;
        this.minimumHeight = minimumHeight > 0 ? minimumHeight : fixedHeight;
    }

    /**
     * Indicate whether the constraints are empty.
     */
    public boolean isEmpty()
    {
        return minimumWidth == 0 && minimumHeight == 0;
    }

    /**
     * Return the fixed width.
     * @return the fixed width, or 0 if the width is not fixed.
     */
    public int getFixedWidth()
    {
        return fixedWidth;
    }

    /**
     * Return the fixed height.
     * @return the fixed height, or 0 if the height is not fixed.
     */
    public int getFixedHeight()
    {
        return fixedHeight;
    }

    /**
     * Return the minimum or fixed width.
     * @return the minimum or fixed width, whichever is defined, or 0 if the width is unconstrained.
     */
    public int getMinimumWidth()
    {
        return minimumWidth;
    }

    /**
     * Return the minimum or fixed height.
     * @return the minimum or fixed height, whichever is defined, or 0 if the height is unconstrained.
     */
    public int getMinimumHeight()
    {
        return minimumHeight;
    }

    @Override
    public @NotNull String toString()
    {
        if (isEmpty()) {
            return "[Unconstrained]";
        }
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        if (fixedWidth > 0) {
            sb.append("W=");
            sb.append(fixedWidth);
        } else if (minimumWidth > 0) {
            sb.append("W>=");
            sb.append(minimumWidth);
        }
        if (minimumHeight > 0) {
            if (sb.length() > 1) {
                sb.append(" ");
            }
            if (fixedHeight > 0) {
                sb.append("H=");
                sb.append(fixedHeight);
            } else {
                sb.append("H>=");
                sb.append(minimumHeight);
            }
        }
        sb.append("]");
        return sb.toString();
    }
}
