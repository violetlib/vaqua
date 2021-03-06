/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.plaf.ColorUIResource;

/**
 * A synthetic color that erases, then applies a tint.
 */

public class TintedEraser extends ColorUIResource {
    public TintedEraser(int intensity, int alpha) {
        super(new Color(intensity, intensity, intensity, alpha));
    }
}
