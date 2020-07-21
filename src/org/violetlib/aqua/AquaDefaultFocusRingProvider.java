/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;

import org.jetbrains.annotations.NotNull;

/**
 * The default focus ring provider uses the bounds rectangle of the component.
 */
public class AquaDefaultFocusRingProvider implements FocusRingOutlineProvider {

    public static final float OUTLINE_OFFSET = 0;

    @Override
    public @NotNull Shape getFocusRingOutline(@NotNull JComponent c) {
        int width = c.getWidth();
        int height = c.getHeight();
        return new Rectangle2D.Double(OUTLINE_OFFSET, OUTLINE_OFFSET, width-2*OUTLINE_OFFSET, height-2*OUTLINE_OFFSET);
    }
}
