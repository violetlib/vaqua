/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.lang.reflect.Method;
import javax.swing.*;

/**
 * A Java 8 specific popup factory.
 */

public class Aqua8PopupFactory extends AquaPopupFactory {

    @Override
    protected Popup getHeavyweightPopup(Component owner, Component contents, int x, int y) {
        // Heavy weight popups are required to support vibrant backgrounds and rounded corners.
        // If we cannot configure this factory to create heavy weight popups, we will create a new popup directly.
        try {
            Method m = PopupFactory.class.getDeclaredMethod("setPopupType", Integer.TYPE);
            m.setAccessible(true);
            m.invoke(this, 2);
        } catch (Throwable e) {
            Utils.logError("Unable to setPopupType", e);
            return new AquaPopup(owner, contents, x, y);
        }

        Popup p = getDefaultPopup(owner, contents, x, y);

        // Reusing popups is not working reliably. Not sure if there is a general timing problem or a change in
        // behavior in El Capitan. The problem is that the stale contents are briefly displayed.
        // See bug JDK-8040630.

        try {
            Method m = p.getClass().getDeclaredMethod("setCacheEnabled", Boolean.TYPE);
            m.setAccessible(true);
            m.invoke(p, false);
        } catch (Exception ex) {
            Utils.logError("Unable to prevent popup from being reused", ex);
        }

        return p;
    }
}
