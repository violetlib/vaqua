/*
 * Copyright (c) 2025 Alan Snyder.
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

import org.jetbrains.annotations.*;

/**
 * A context dependent border for lists that supports side margins when the inset view style is in use.
 */
public class AquaListBorder extends AbstractBorder implements UIResource {

    @Override
    public Insets getBorderInsets(@NotNull Component c, @NotNull Insets insets) {
        AquaListUI ui = AquaUtils.getUI((JComponent) c, AquaListUI.class);
        if (ui != null) {
            return ui.getInsets();
        }
        insets.top = 0;
        insets.bottom = 0;
        insets.left = 1;
        insets.right = 1;
        return insets;
    }
}
