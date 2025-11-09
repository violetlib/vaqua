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
 * Information describing the layout of a toolbar item button whose outline or background is drawn around the icon.
 */

public class SplitToolbarItemLayoutInfo extends ButtonLayoutInfo {
    public final @NotNull Rectangle iconOutlineBounds;
    public final @NotNull Insets contentInsets;

    public SplitToolbarItemLayoutInfo(@NotNull ButtonLayoutInfo basic,
                                      @NotNull Rectangle iconOutlineBounds,
                                      @NotNull Insets contentInsets) {
        super(basic);
        this.iconOutlineBounds = iconOutlineBounds;
        this.contentInsets = contentInsets;
    }

    public SplitToolbarItemLayoutInfo(@NotNull Rectangle iconBounds,
                                      @NotNull Rectangle labelBounds,
                                      @NotNull Rectangle contentBounds,
                                      @Nullable String substitutedLabel,
                                      @NotNull Rectangle iconOutlineBounds,
                                      @NotNull Insets contentInsets) {
        super(iconBounds, labelBounds, contentBounds, substitutedLabel);
        this.iconOutlineBounds = iconOutlineBounds;
        this.contentInsets = contentInsets;
    }

    @Override
    public @NotNull SplitToolbarItemLayoutInfo extend(@NotNull Insets s) {
        return new SplitToolbarItemLayoutInfo(super.extend(s), offset(iconOutlineBounds, s.left, s.top), contentInsets);
    }
}
