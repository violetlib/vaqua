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
 * A border for a checkbox.
 */
public class AquaCheckBoxButtonBorder extends AquaLabeledButtonBorder {
    public AquaCheckBoxButtonBorder() {
        super(ButtonWidget.BUTTON_CHECK_BOX, AquaButtonExtendedTypes.getWidgetInfo(ButtonWidget.BUTTON_CHECK_BOX));
    }
}
