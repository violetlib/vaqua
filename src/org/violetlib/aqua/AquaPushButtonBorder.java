/*
 * Copyright (c) 2015-2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;

/**
 * A border for a push button with no client specified button style. A generic button border must adapt to any size
 * text. It can use a fixed height style only if the text fits using that height. Otherwise, an alternate style must be
 * used.
 *
 * @see AquaToggleButtonBorder
 */
public class AquaPushButtonBorder extends AquaButtonBorder {

    @Override
    public @NotNull ButtonStyleInfo getButtonStyleInfo(@NotNull AbstractButton b) {
        boolean isOnToolbar = AquaUtils.isOnToolbar(b);
        if (isOnToolbar) {
            return AquaButtonSupport.getToolbarItemStyleInfo(b, painter);
        }

        ButtonWidget preferredWidget = ButtonWidget.BUTTON_PUSH;
        if (AquaButtonSupport.isButtonWidgetUsable(b, preferredWidget, painter)) {
            return AquaButtonSupport.getButtonStyleInfo(b, preferredWidget);
        }

        if (b.getIcon() != null) {
            int version = AquaPainting.getVersion();
            boolean isOld = version < 1500;
            ButtonWidget w  = isOld ? ButtonWidget.BUTTON_GRADIENT : ButtonWidget.BUTTON_BEVEL_ROUND;
            return AquaButtonSupport.getButtonStyleInfo(b, w);
        }

        return AquaButtonSupport.getButtonStyleInfo(b, ButtonWidget.BUTTON_BEVEL_ROUND);
    }
}
