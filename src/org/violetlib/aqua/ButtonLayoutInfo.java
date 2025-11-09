/*
 * Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.*;

/**
 * Information describing the layout of a button. When computing a preferred size, only the content dimensions
 * are meaningful.
 */

public class ButtonLayoutInfo {
    public final @Nullable Rectangle iconBounds;
    public final @Nullable Rectangle labelBounds;
    public final @NotNull Rectangle contentBounds;
    public final @Nullable String substitutedLabel;  // when the label must be clipped

    public ButtonLayoutInfo(@Nullable Rectangle iconBounds,
                            @Nullable Rectangle labelBounds,
                            @NotNull Rectangle contentBounds,
                            @Nullable String substitutedLabel) {
        this.iconBounds = iconBounds;
        this.labelBounds = labelBounds;
        this.contentBounds = contentBounds;
        this.substitutedLabel = substitutedLabel;
    }

    public ButtonLayoutInfo(@NotNull ButtonLayoutInfo source) {
        this.iconBounds = source.iconBounds;
        this.labelBounds = source.labelBounds;
        this.contentBounds = source.contentBounds;
        this.substitutedLabel = source.substitutedLabel;
    }

    public @NotNull ButtonLayoutInfo extend(@NotNull Insets s) {
        return new ButtonLayoutInfo(offset(iconBounds, s.left, s.top),
          offset(labelBounds, s.left, s.top),
          offset(contentBounds, s.left, s.top),
          substitutedLabel);
    }

    public @NotNull ButtonLayoutInfo toLeftAligned() {
        if (iconBounds != null || labelBounds != null) {
            int delta = (iconBounds != null ? iconBounds.x : labelBounds.x) - contentBounds.x;
            Rectangle nIconBounds = null;
            Rectangle nLabelBounds = null;

            if (iconBounds != null) {
                nIconBounds = new Rectangle(contentBounds.x, iconBounds.y, iconBounds.width, iconBounds.height);
            }
            if (labelBounds != null) {
                nLabelBounds = new Rectangle(labelBounds.x - delta, labelBounds.y, labelBounds.width, labelBounds.height);
            }
            return new ButtonLayoutInfo(nIconBounds, nLabelBounds, contentBounds, substitutedLabel);
        }
        return this;
    }

    public static @Nullable Rectangle offset(@Nullable Rectangle r, int x, int y) {
        if (r != null) {
            return new Rectangle(r.x + x, r.y + y, r.width, r.height);
        }
        return null;
    }
}
