/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.*;

/**
 * This file is deliberately lean to avoid unnecessary computation at load time.
 */

public class InstalledColorsInstrumentation {
    static @Nullable ColorsInstrumentation installed;
}
