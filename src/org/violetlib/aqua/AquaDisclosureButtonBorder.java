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
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonState;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;

import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.OFF;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.ON;

/**
 * A border for a disclosure button.
 */
public class AquaDisclosureButtonBorder extends AquaButtonBorder implements FocusRingOutlineProvider {

    public AquaDisclosureButtonBorder() {
    }

    @Override
    public @NotNull ButtonWidget getButtonWidget(@NotNull AbstractButton b) {
        return ButtonWidget.BUTTON_DISCLOSURE;
    }

    @Override
    protected @NotNull ButtonState getButtonState(@NotNull AbstractButton b) {
        return b.getModel().isSelected() ? ON : OFF;
    }

    @Override
    public boolean allowsContent() {
        return false;
    }
}
