/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;
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
    public @NotNull ButtonWidget getButtonWidget(@NotNull AbstractButton b) {
        boolean isOnToolbar = AquaUtils.isOnToolbar(b);

        ButtonWidget preferredWidget = isOnToolbar ? ButtonWidget.BUTTON_TEXTURED_TOOLBAR : ButtonWidget.BUTTON_PUSH;
        if (isProposedButtonWidgetUsable(b, preferredWidget)) {
            return preferredWidget;
        }

        if (b.getIcon() != null) {
            return isOnToolbar ? ButtonWidget.BUTTON_TOOLBAR_ITEM : ButtonWidget.BUTTON_GRADIENT;
        }

        return ButtonWidget.BUTTON_BEVEL_ROUND;
    }
}
