/*
 * Copyright (c) 2015-2017 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

/**
 * A custom popup factory that creates heavyweight popups and reconfigures reused popups. Custom behavior is disabled
 * when this look and feel is uninstalled. Custom behavior is implemented by platform-dependent subclasses.
 */
public class AquaPopupFactory extends PopupFactory {
    protected boolean isActive;

    public boolean isActive() {
        return isActive;
    }

    public void setActive(boolean active) {
        isActive = active;
    }
}
