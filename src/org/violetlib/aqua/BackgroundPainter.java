/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * The interface for an object that paints the background of a component.
 */
public interface BackgroundPainter {
    void paintBackground(JComponent c, Graphics g, int x, int y, int width, int height);
}
