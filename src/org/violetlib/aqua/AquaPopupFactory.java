/*
 * Copyright (c) 2015-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * A custom popup factory that creates heavyweight popups and reconfigures reused popups. Custom behavior is disabled
 * when this look and feel is uninstalled. Custom behavior is implemented by platform-dependent subclasses.
 */
public abstract class AquaPopupFactory extends PopupFactory {
    protected boolean isActive;

    public boolean isActive() {
        return isActive;
    }

    public void setActive(boolean active) {
        isActive = active;
    }

    @Override
    public Popup getPopup(Component owner, Component contents, int x, int y)
            throws IllegalArgumentException {
        if (isActive) {
            Popup p = getHeavyweightPopup(owner, contents, x, y);
            return configure(owner, p, contents);
        } else {
            return super.getPopup(owner, contents, x, y);
        }
    }

    protected abstract Popup getHeavyweightPopup(Component owner, Component contents, int x, int y);

    // for use by subclasses
    protected Popup getDefaultPopup(Component owner, Component contents, int x, int y)
    {
        return super.getPopup(owner, contents, x, y);
    }

    private Popup configure(Component owner, Popup p, Component contents) {
        // If the popup is a reused popup, then we need to reconfigure it.
        // A new popup will have zero size because pack() is not called if the window is not visible (and why would
        // it be?) A reused popup will probably have the wrong size, for the same reason.

        Window w = SwingUtilities.getWindowAncestor(contents);
        AquaRootPaneUI ui = null;
        JRootPane rp = AquaUtils.getRootPane(w);
        if (rp != null) {
            ui = AquaUtils.getUI(rp, AquaRootPaneUI.class);
            if (ui == null) {
                rp.updateUI();
                ui = AquaUtils.getUI(rp, AquaRootPaneUI.class);
            }
        }

        if (w.isDisplayable() && w.getWidth() > 0) {
            // The popup is reused. It will have the old size.
            w.setSize(w.getPreferredSize());
            w.invalidate();
            w.validate();
        }

        if (ui != null) {
            String appearanceName = AppearanceManager.getSpecifiedAppearanceName(owner);
            AppearanceManager.setSpecifiedAppearanceName(rp, appearanceName);
        }

        // Workaround for JDK bug when popup owner is an embedded component
        AquaUtils.nativeFixPopupWindow((Window) w.getParent(), p);

        return p;
    }
}
