/*
 * Copyright (c) 2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Objects;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A description of the location and visibility of a native view with respect to the native content view. This
 * description supports rectangular clipping that can be imposed by scroll pane viewports. It does not support arbitrary
 * clipping that might result from occlusion by components painted over the native view.
 */

public final class VisibleBounds {

    /**
     * The region of the content view where the view appears.
     */
    public final @NotNull Rectangle visibleBounds;

    /**
     * The origin and size of the full view. For a fully visible view, the frame will match the visible bounds.
     */
    public final @NotNull Rectangle frame;

    public VisibleBounds(@NotNull Rectangle visibleBounds, @NotNull Rectangle frame) {
        this.visibleBounds = visibleBounds;
        this.frame = frame;
    }

    public boolean isClipped() {
        return !visibleBounds.equals(frame);
    }

    @Override
    public boolean equals(@Nullable Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        VisibleBounds that = (VisibleBounds) o;
        return visibleBounds.equals(that.visibleBounds) && frame.equals(that.frame);
    }

    @Override
    public int hashCode() {
        return Objects.hash(visibleBounds, frame);
    }
}
