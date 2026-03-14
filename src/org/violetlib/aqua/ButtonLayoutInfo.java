/*
 * Copyright (c) 2025-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.geom.Rectangle2D;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insets2D;

/**
 * Information describing the layout of a button. When computing a preferred size, only the content dimensions
 * are meaningful.
 */

public class ButtonLayoutInfo {
    public final @Nullable Rectangle2D iconBounds;
    public final @Nullable Rectangle2D labelBounds;
    public final @NotNull Rectangle2D contentBounds;
    public final @Nullable String substitutedLabel;  // when the label must be clipped

    public ButtonLayoutInfo(@Nullable Rectangle2D iconBounds,
                            @Nullable Rectangle2D labelBounds,
                            @NotNull Rectangle2D contentBounds,
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

    public @NotNull ButtonLayoutInfo extend(@NotNull Insets2D s) {
        return new ButtonLayoutInfo(offset(iconBounds, s.getLeft(), s.getTop()),
          offset(labelBounds, s.getLeft(), s.getTop()),
          offset(contentBounds, s.getLeft(), s.getTop()),
          substitutedLabel);
    }

    public @NotNull ButtonLayoutInfo offset(float top, float left)
    {
        return new ButtonLayoutInfo(offset(iconBounds, left, top),
          offset(labelBounds, left, top),
          offset(contentBounds, left, top),
          substitutedLabel);
    }

    public @NotNull ButtonLayoutInfo toLeftAligned() {
        if (iconBounds != null || labelBounds != null) {
            double delta = (iconBounds != null ? iconBounds.getX() : labelBounds.getX()) - contentBounds.getX();
            Rectangle2D nIconBounds = null;
            Rectangle2D nLabelBounds = null;

            if (iconBounds != null) {
                nIconBounds = new Rectangle2D.Double(contentBounds.getX(), iconBounds.getY(),
                  iconBounds.getWidth(), iconBounds.getHeight());
            }
            if (labelBounds != null) {
                nLabelBounds = new Rectangle2D.Double(labelBounds.getX() - delta, labelBounds.getY(),
                  labelBounds.getWidth(), labelBounds.getHeight());
            }
            return new ButtonLayoutInfo(nIconBounds, nLabelBounds, contentBounds, substitutedLabel);
        }
        return this;
    }

    public static @Nullable Rectangle2D offset(@Nullable Rectangle2D r, double x, double y) {
        if (r != null) {
            return new Rectangle2D.Double(r.getX() + x, r.getY() + y, r.getWidth(), r.getHeight());
        }
        return null;
    }
}
