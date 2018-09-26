/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.NotNull;
import org.violetlib.jnr.aqua.AquaNativeRendering;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Provides access to the native painter.
 */
public class AquaPainting {

    public static @NotNull AquaUIPainter create() {
        return AquaNativeRendering.createPainter();
    }
}
