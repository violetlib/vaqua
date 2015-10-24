/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;

/**
 * A border for a radio button.
 */
public class AquaRadioButtonBorder extends AquaLabeledButtonBorder {
    public AquaRadioButtonBorder() {
        super(ButtonWidget.BUTTON_RADIO, AquaButtonExtendedTypes.getWidgetInfo(ButtonWidget.BUTTON_RADIO));
    }
}
