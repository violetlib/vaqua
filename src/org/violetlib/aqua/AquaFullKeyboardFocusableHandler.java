/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * This handler sets the focusability of a component based on the Full Keyboard Access preference.
 */
public class AquaFullKeyboardFocusableHandler implements ChangeListener {

    public static final String OPTIONAL_FOCUSABILITY_HANDLER_KEY = "Aqua.optionalFocusabilityHandler";

    private static final String CLIENT_PROPERTY_KEY = "FullKeyboardFocusListener";

    private final JComponent c;

    public AquaFullKeyboardFocusableHandler(JComponent c) {
        this.c = c;
        update();
        OSXSystemProperties.addChangeListener(this);
    }

    public void dispose() {
        OSXSystemProperties.removeChangeListener(this);
    }

    @Override
    public void stateChanged(ChangeEvent e) {
        update();
    }

    private void update() {
        boolean isFocusable = OSXSystemProperties.isFullKeyboardAccessEnabled();

        Object o = c.getClientProperty(OPTIONAL_FOCUSABILITY_HANDLER_KEY);
        if (o instanceof OptionallyFocusableComponentHandler) {
            OptionallyFocusableComponentHandler h = (OptionallyFocusableComponentHandler) o;
            h.updateFocusability(c, isFocusable);
        } else {
            c.setFocusable(isFocusable);
        }
    }

    public static void addListener(JComponent c) {
        AquaFullKeyboardFocusableHandler h = new AquaFullKeyboardFocusableHandler(c);
        c.putClientProperty(CLIENT_PROPERTY_KEY, h);
    }

    public static void removeListener(JComponent c) {
        AquaFullKeyboardFocusableHandler h = (AquaFullKeyboardFocusableHandler) c.getClientProperty(CLIENT_PROPERTY_KEY);
        if (h != null) {
            c.putClientProperty(CLIENT_PROPERTY_KEY, null);
            h.dispose();
        }
    }
}
