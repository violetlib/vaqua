/*
 * Copyright (c) 2025-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.Rectangle2D;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insets2D;

/**
 * Information describing the layout of a toolbar item button whose outline or background is drawn around the icon.
 */

public class SplitToolbarItemLayoutInfo extends ButtonLayoutInfo {
    public final @NotNull Rectangle2D iconOutlineBounds;
    public final @NotNull Insets contentInsets;

    public SplitToolbarItemLayoutInfo(@NotNull ButtonLayoutInfo basic,
                                      @NotNull Rectangle2D iconOutlineBounds,
                                      @NotNull Insets contentInsets) {
        super(basic);
        this.iconOutlineBounds = iconOutlineBounds;
        this.contentInsets = contentInsets;
    }

    public SplitToolbarItemLayoutInfo(@NotNull Rectangle2D iconBounds,
                                      @NotNull Rectangle2D labelBounds,
                                      @NotNull Rectangle2D contentBounds,
                                      @Nullable String substitutedLabel,
                                      @NotNull Rectangle2D iconOutlineBounds,
                                      @NotNull Insets contentInsets) {
        super(iconBounds, labelBounds, contentBounds, substitutedLabel);
        this.iconOutlineBounds = iconOutlineBounds;
        this.contentInsets = contentInsets;
    }

    @Override
    public @NotNull SplitToolbarItemLayoutInfo extend(@NotNull Insets2D s) {
        return new SplitToolbarItemLayoutInfo(super.extend(s), offset(iconOutlineBounds, s.getLeft(), s.getTop()), contentInsets);
    }
}
