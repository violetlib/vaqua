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
 * The heavy weight popup used by this look and feel.
 */
public class AquaPopup extends Popup {

    private final Window popup;

    public AquaPopup(Component owner, Component contents, int x, int y, boolean isContextual) {
        super(owner, contents, x, y);
        popup = SwingUtilities.getWindowAncestor(contents);

        if (popup instanceof RootPaneContainer) {
            JRootPane popupRootPane = ((RootPaneContainer) popup).getRootPane();

            if (isContextual) {
                popup.pack();
                AquaUtils.setCornerRadius(popup, 6);
            } else {
            }
        }
    }

    public Window getPopup() {
        return popup;
    }
}
