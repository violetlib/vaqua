/*
 * Copyright (c) 2020-2026 Alan Snyder.
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
 * A context dependent border for trees that supports side margins when the inset view style is in use.
 */
@SuppressWarnings("serial")
public class AquaTreeBorder
  extends AquaBorder
{
    public static final @NotNull String RENDERER_CONTAINER_KEY = "JComponent.rendererContainer";

    @Override
    protected void paint(@NotNull JComponent c, @NotNull Graphics2D g, int x, int y, int width, int height)
    {
    }

    @Override
    public Insets getBorderInsets(@NotNull Component c)
    {
        AquaTreeUI ui = getTreeUI((JComponent) c);
        if (ui != null) {
            return ui.getInsets();
        }
        return new Insets(0, 1, 0, 1);
    }

    private @Nullable AquaTreeUI getTreeUI(@NotNull JComponent jc)
    {
        AquaTreeUI ui = AquaUtils.getUI(jc, AquaTreeUI.class);
        if (ui != null) {
            return ui;
        }
        Object o = jc.getClientProperty(RENDERER_CONTAINER_KEY);
        if (o instanceof Component) {
            return AquaUtils.getUI((Component) o, AquaTreeUI.class);
        }
        return null;
    }
}
