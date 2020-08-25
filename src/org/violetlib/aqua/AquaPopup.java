package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * A heavy weight popup.
 */

public class AquaPopup extends Popup {

    private final Window popup;

    private static final Float TRANSLUCENT = new Float(248f/255f);

    public AquaPopup(Component owner, Component contents, int x, int y) {
        super(owner, contents, x, y);
        popup = SwingUtilities.getWindowAncestor(contents);

        // It seems that window shadows only work on opaque windows

        if (popup instanceof RootPaneContainer) {
            popup.pack();
            JRootPane popupRootPane = ((RootPaneContainer) popup).getRootPane();
            popupRootPane.putClientProperty("Window.alpha", TRANSLUCENT);
            popupRootPane.putClientProperty("Window.shadow", Boolean.TRUE);
            popupRootPane.putClientProperty("apple.awt._windowFadeDelegate", owner);
            popupRootPane.putClientProperty("apple.awt.draggableWindowBackground", Boolean.FALSE);
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    popupRootPane.putClientProperty("apple.awt.windowShadow.revalidateNow", Double.valueOf(Math.random()));
                }
            });
        }
    }

    public Window getPopup() {
        return popup;
    }

    @Override
    public void hide() {
        super.hide();
    }

    @Override
    public void show() {
        super.show();
    }
}
