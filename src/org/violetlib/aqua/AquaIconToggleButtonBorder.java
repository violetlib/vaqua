/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.violetlib.jnr.aqua.AquaUIPainter.Size;

import javax.swing.*;
import java.awt.*;

/**
 * The border used for the icon button style.
 */
public class AquaIconToggleButtonBorder extends AquaToggleButtonBorder {

    @Override
    public Font getCustomDefaultFont(AbstractButton b, Size size, Font df) {
        float fontSize = AquaFonts.getControlTextSmallFont().getSize2D();
        return df.deriveFont(fontSize);
    }
}
