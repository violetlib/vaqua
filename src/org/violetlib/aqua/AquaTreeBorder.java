/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.NotNull;

/**
 * A context dependent border for trees that supports side margins when the inset view style is in use.
 */
@SuppressWarnings("serial")
public class AquaTreeBorder extends AbstractBorder implements UIResource {

    public Insets getBorderInsets(@NotNull Component c, @NotNull Insets insets) {
        boolean isInset = computeIsInset((JComponent)c);
        int top = 0;
        int side = isInset ? 10 : 1;
        insets.top = insets.bottom = top;
        insets.left = insets.right = side;
        if (isInset) {
            boolean isLTR = c.getComponentOrientation().isLeftToRight();
            if (isLTR) {
                insets.right += 10;
            } else {
                insets.left += 10;
            }
        }
        return insets;
    }

    private boolean computeIsInset(@NotNull JComponent c) {
        AquaTreeUI ui = AquaUtils.getUI(c, AquaTreeUI.class);
        if (ui != null) {
            return ui.isInset();
        }
        return false;
    }
}
