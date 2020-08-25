/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget;

import javax.swing.*;

import static org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED;
import static org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_TEXTURED_SEPARATED;

/**
 * A border for a toggle button with no client specified button style. A generic button border must adapt to any size
 * text. It can use a fixed height style only if the text fits using that height. Otherwise, an alternate style must be
 * used.
 *
 * @see AquaPushButtonBorder
 */
public class AquaToggleButtonBorder extends AquaButtonBorder implements FocusRingOutlineProvider {

    @Override
    protected final Object getButtonWidget(AbstractButton b) {
        boolean isOnToolbar = AquaButtonUI.isOnToolbar(b);

        Object preferredWidget = isOnToolbar ? ButtonWidget.BUTTON_TEXTURED : BUTTON_SEGMENTED;
        if (isProposedButtonWidgetUsable(b, preferredWidget)) {
            return preferredWidget;
        }

        if (b.getIcon() != null) {
            return isOnToolbar ? ButtonWidget.BUTTON_TOOLBAR_ITEM : ButtonWidget.BUTTON_GRADIENT;
        }

        return ButtonWidget.BUTTON_BEVEL_ROUND;
    }
}
