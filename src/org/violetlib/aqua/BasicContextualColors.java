/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.NotNull;

/**
 * Identify contextually determined foreground and background colors.
 */

public interface BasicContextualColors {

    @NotNull Color getBackground(@NotNull AppearanceContext context);

    @NotNull Color getForeground(@NotNull AppearanceContext context);
}
