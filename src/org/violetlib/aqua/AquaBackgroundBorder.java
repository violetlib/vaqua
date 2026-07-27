/*
 * Copyright (c) 2021-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 *  A "border" that actually paints a background. Used by AquaTextComponentUI.
 */
public interface AquaBackgroundBorder extends Border {

    /**
     * Paint a background for a component.
     * @param c The component whose background is being painted (might be a scroll pane).
     * @param g The graphics context.
     * @param background The background color.
     * @param borderColor The border color.
     */
    void paintBackground(@NotNull JComponent c,
                         @NotNull Graphics g,
                         @Nullable Color background,
                         @Nullable Color borderColor);
}
