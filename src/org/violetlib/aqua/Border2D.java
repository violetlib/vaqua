/*
 * Copyright (c) 2014-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.NotNull;
import org.violetlib.jnr.Insets2D;

/**
 * Interface for a border that has floating point insets.
 */
public interface Border2D {
    @NotNull Insets2D getBorderInsets2D(@NotNull Component c);
}
