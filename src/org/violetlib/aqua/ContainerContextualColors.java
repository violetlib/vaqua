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
 * Container contextual colors support row specific values. These objects are stateful. They must be configured before
 * use.
 */

public interface ContainerContextualColors extends BasicContextualColors {

    void configureForContainer();

    void configureForRow(int rowIndex, boolean isRowSelected);

    void configureForRow(boolean isRowSelected);

    boolean isStriped();

    @NotNull Color getBackground(@NotNull AppearanceContext context);

    @NotNull Color getForeground(@NotNull AppearanceContext context);

    @NotNull Color getGrid(@NotNull AppearanceContext context);
}
