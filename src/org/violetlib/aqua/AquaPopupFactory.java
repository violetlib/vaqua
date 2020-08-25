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

            // Reusing popups is not working reliably. Not sure if there is a general timing problem or a change in
            // behavior in El Capitan. The problem is that the stale contents are briefly displayed.
            // See bug JDK-8040630.

            try {
                Method m = p.getClass().getDeclaredMethod("setCacheEnabled", Boolean.TYPE);
                m.setAccessible(true);
                m.invoke(p, false);
            } catch (Exception ex) {
                System.err.println("Unable to prevent popup from being reused");
            }

            // If the popup is a reused popup, then we need to reconfigure it.
            // A new popup will have zero size because pack() is not called if the window is not visible (and why would
            // it be?) A reused popup will probably have the wrong size, for the same reason.
            Window w = SwingUtilities.getWindowAncestor(contents);
            if (w.isDisplayable() && w.getWidth() > 0) {
                // The popup is reused. It will have the old size.
                w.setSize(w.getPreferredSize());
                w.invalidate();
                w.validate();
                if (w instanceof RootPaneContainer) {
                    JRootPane rp = ((RootPaneContainer) w).getRootPane();
                    AquaRootPaneUI ui = AquaUtils.getUI(rp, AquaRootPaneUI.class);
                    if (ui != null) {
                        // Reconfigure the popup based on the client properties of the new content component.
                        ui.configure();
                    }
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
