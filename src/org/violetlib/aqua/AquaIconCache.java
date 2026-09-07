/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.lang.ref.SoftReference;
import java.util.*;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * Softly cache icons to avoid recomputation.
 */

public class AquaIconCache {

    private final Map<Object,SoftReference<Icon>> iconMap = new HashMap<>();

    /**
     * Return the icon with the specified key.
     * @param key The key.
     * @return the associated {@code icon}, or null if not available.
     */
    public @Nullable Icon get(@NotNull Object key) {
        SoftReference<Icon> r = iconMap.get(key);
        if (r != null) {
            Icon icon = r.get();
            if (icon != null) {
                return icon;
            }
            iconMap.remove(key);
        }
        return null;
    }

    /**
     * Register an icon with a key.
     * @param key The key.
     * @param icon The icon.
     */
    public void put(@NotNull Object key, @NotNull Icon icon) {
        SoftReference<Icon> r = new SoftReference<>(icon);
        iconMap.put(key, r);
    }
}
