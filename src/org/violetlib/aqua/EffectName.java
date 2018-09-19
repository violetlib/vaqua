/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.NotNull;

/**

 */

public class EffectName {
    private final @NotNull String name;

    public static final @NotNull EffectName EFFECT_NONE = new EffectName("none");
    public static final @NotNull EffectName EFFECT_PRESSED = new EffectName("pressed");
    public static final @NotNull EffectName EFFECT_DEEP_PRESSED = new EffectName("deepPressed");
    public static final @NotNull EffectName EFFECT_DISABLED = new EffectName("disabled");
    public static final @NotNull EffectName EFFECT_ROLLOVER = new EffectName("rollover");

    private EffectName(@NotNull String name) {
        this.name = name;
    }

    public @NotNull String getName() {
        return name;
    }

    @Override
    public @NotNull String toString() {
        return name;
    }
}
