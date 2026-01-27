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
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.*;

/**
 *
 */
public final class AquaUICompoundBorder
  extends CompoundBorder
  implements UIResource
{
    public static @NotNull AquaUICompoundBorder of(@NotNull Border outside, @NotNull Border inside)
    {
        return new AquaUICompoundBorder(outside, inside);
    }

    private AquaUICompoundBorder(@NotNull Border outsideBorder, @NotNull Border insideBorder) {
        super(outsideBorder, insideBorder);
    }

    @Override
    public void paintBorder(@NotNull Component c, @NotNull Graphics g, int x, int y, int width, int height) {
        JComponent jc = (JComponent) c;
        super.paintBorder(jc, g, x, y, width, height);
        PaintingContext.pop(jc);
    }
}
