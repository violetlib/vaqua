/*
 * Copyright (c) 2025-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * A context dependent border for lists that supports side margins when the inset view style is in use.
 */
public class AquaListBorder
  extends AquaBorder
{
    @Override
    protected void paint(@NotNull JComponent c, @NotNull Graphics2D g, int x, int y, int width, int height)
    {
    }

    @Override
    public Insets getBorderInsets(Component c) {
        AquaListUI ui = AquaUtils.getUI((JComponent) c, AquaListUI.class);
        if (ui != null) {
            return ui.getInsets();
        }
        return new Insets(0, 1, 0, 1);
    }
}
