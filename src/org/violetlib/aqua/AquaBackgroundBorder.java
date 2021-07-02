/*
 * Copyright (c) 2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import javax.swing.border.Border;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 *  A "border" that actually paints a background. Used by AquaTextComponentUI.
 */
public interface AquaBackgroundBorder extends Border {

    void paintBackground(@NotNull Component c, Graphics g, @Nullable Color background);
}
