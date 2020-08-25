/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

/**
 * Interface for a focus ring painter.
 */
public interface FocusRingPainter {

    /**
     * Paint a focus ring using the specified color.
     */
    void paint(Graphics2D g, Color c);
}
