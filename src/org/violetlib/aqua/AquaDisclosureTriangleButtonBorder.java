/*
 * Copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;

import javax.swing.*;

import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.OFF;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.ON;

/**
 * A border for a disclosure triangle button.
 */
public class AquaDisclosureTriangleButtonBorder extends AquaButtonBorder implements FocusRingOutlineProvider {

    public AquaDisclosureTriangleButtonBorder() {
    }

    @Override
    public ButtonWidget getButtonWidget(AbstractButton b) {
        return ButtonWidget.BUTTON_DISCLOSURE_TRIANGLE;
    }

    @Override
    protected AquaUIPainter.ButtonState getButtonState(AbstractButton b) {
        return b.getModel().isSelected() ? ON : OFF;
    }

    @Override
    public boolean allowsContent() {
        return false;
    }
}
