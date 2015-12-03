/*
 * Copyright (c) 2015 Alan Snyder.
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
 * A custom popup factory that creates heavyweight popups and reconfigures reused popups. Custom behavior is disabled
 * when this look and feel is uninstalled.
 */
public class AquaPopupFactory extends PopupFactory {
    private boolean isActive;

    @Override
    public Popup getPopup(Component owner, Component contents, int x, int y) throws IllegalArgumentException {
        if (isActive) {
            // Heavy weight popups are required to support vibrant backgrounds and rounded corners
            try {
                Method m = PopupFactory.class.getDeclaredMethod("setPopupType", Integer.TYPE);
                m.setAccessible(true);
                m.invoke(this, 2);
            } catch (Throwable e) {
                System.err.println("Unable to setPopupType: " + e);
                return new AquaPopup(owner, contents, x, y);
            }
            Popup p = super.getPopup(owner, contents, x, y);
            // If the popup is a reused popup, then we need to reconfigure it.
            Window w = SwingUtilities.getWindowAncestor(contents);
            if (w.isDisplayable() && w instanceof RootPaneContainer) {
                JRootPane rp = ((RootPaneContainer) w).getRootPane();
                AquaRootPaneUI ui = AquaUtils.getUI(rp, AquaRootPaneUI.class);
                if (ui != null) {
                    ui.configure();
                }
            }
            return p;
        }
        return super.getPopup(owner, contents, x, y);
    }

    public boolean isActive() {
        return isActive;
    }

    public void setActive(boolean active) {
        isActive = active;
    }
}
