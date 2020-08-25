/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.violetlib.jnr.Insets2D;

import java.awt.*;

/**
 * Interface for a border that has floating point insets.
 */
public interface Border2D {
    Insets2D getBorderInsets2D(Component c);
}
