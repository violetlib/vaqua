/*
 * Copyright (c) 2014-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.GenericButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget;

/**
 * A border for a toggle button with no client specified button style. A generic button border must adapt to any size
 * text. It can use a fixed height style only if the text fits using that height. Otherwise, an alternate style must be
 * used.
 *
 * @see AquaPushButtonBorder
 */
public class AquaToggleButtonBorder extends AquaButtonBorder implements FocusRingOutlineProvider {

    @Override
    public final @NotNull GenericButtonWidget getButtonWidget(@NotNull AbstractButton b) {
        boolean isOnToolbar = AquaUtils.isOnToolbar(b);

        GenericButtonWidget preferredWidget = isOnToolbar
                ? ButtonWidget.BUTTON_TEXTURED_TOOLBAR
                : SegmentedButtonWidget.BUTTON_SEGMENTED;
        if (isProposedButtonWidgetUsable(b, preferredWidget)) {
            return preferredWidget;
        }

        if (b.getIcon() != null) {
            return isOnToolbar ? ButtonWidget.BUTTON_TOOLBAR_ITEM : ButtonWidget.BUTTON_GRADIENT;
        }

        return ButtonWidget.BUTTON_BEVEL_ROUND;
    }

    @Override
    public @Nullable GenericButtonConfiguration getConfiguration(@NotNull AbstractButton b, int width, int height) {

        LayoutConfiguration g = getLayoutConfiguration(b);

        if (g instanceof SegmentedButtonLayoutConfiguration) {
            AquaUIPainter.State state = getState(b);
            boolean isFocused = computeIsFocused(state, b);
            boolean isSelected = b.getModel().isSelected();
            AquaUIPainter.Direction d = AquaUIPainter.Direction.NONE;
            return new SegmentedButtonConfiguration((SegmentedButtonLayoutConfiguration) g, state, isSelected,
                    isFocused, d, SegmentedButtonConfiguration.DividerState.NONE, SegmentedButtonConfiguration.DividerState.NONE);
        }

        return super.getConfiguration(b, width, height);
    }
}
