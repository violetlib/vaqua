/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * A popup factory for Java 9 and later.
 */

public class Aqua9PopupFactory extends AquaPopupFactory {

    @Override
    protected Popup getHeavyweightPopup(Component owner, Component contents, int x, int y) {
        // Heavy weight popups are required to support vibrant backgrounds and rounded corners.
        Popup p = super.getPopup(owner, contents, x, y, true);

        // Reusing popups is not working reliably. Not sure if there is a general timing problem or a change in
        // behavior in El Capitan. The problem is that the stale contents are briefly displayed.
        // See bug JDK-8040630.

        try {
            AquaUtils.disablePopupCache(p);
        } catch (Throwable th) {
            System.err.println("Unable to prevent popup from being reused");
        }

        return p;
    }
}
