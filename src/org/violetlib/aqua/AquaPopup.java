/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * A heavy weight popup.
 */
public class AquaPopup extends Popup {

    private final Window popup;

    private static final Float TRANSLUCENT = new Float(248f/255f);

    public AquaPopup(Component owner, Component contents, int x, int y, boolean isContextual) {
        super(owner, contents, x, y);
        popup = SwingUtilities.getWindowAncestor(contents);

        if (popup instanceof RootPaneContainer) {
            JRootPane popupRootPane = ((RootPaneContainer) popup).getRootPane();

            if (isContextual) {
                popup.pack();
                AquaUtils.setCornerRadius(popup, 6);
                popupRootPane.putClientProperty("Aqua.backgroundStyle", "vibrantMenu");
            } else {
                popupRootPane.putClientProperty("Window.alpha", TRANSLUCENT);
                popupRootPane.putClientProperty("Window.shadow", Boolean.TRUE);
                popupRootPane.putClientProperty("apple.awt._windowFadeDelegate", owner);
                popupRootPane.putClientProperty("apple.awt.draggableWindowBackground", Boolean.FALSE);
                popup.pack();
            }
        }
    }

    public Window getPopup() {
        return popup;
    }
}
