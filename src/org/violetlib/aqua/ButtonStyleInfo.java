/*
 * Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * The widget and size information for a generalized button. This description allows an unstyled button to be assigned a
 * widget and a size.
 */
public class ButtonStyleInfo {
    public final @NotNull AquaUIPainter.GenericButtonWidget widget;
    public final @NotNull AquaUIPainter.Size size;

    public ButtonStyleInfo(@NotNull AquaUIPainter.GenericButtonWidget widget, @NotNull AquaUIPainter.Size size) {
        this.widget = widget;
        this.size = size;
    }
}
