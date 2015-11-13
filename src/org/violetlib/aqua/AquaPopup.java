/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * A heavy weight popup.
 */
public class AquaPopup extends Popup {

    private final Window popup;
    private boolean isShadowValid;

    private static final Float TRANSLUCENT = new Float(248f/255f);

    public AquaPopup(Component owner, Component contents, int x, int y, boolean isContextual) {
        super(owner, contents, x, y);
        popup = SwingUtilities.getWindowAncestor(contents);

        // It seems that window shadows only work on opaque windows

        if (popup instanceof RootPaneContainer) {
            JRootPane popupRootPane = ((RootPaneContainer) popup).getRootPane();

            if (isContextual) {
                makeClear(popup, !OSXSystemProperties.isReduceTransparency());
                popup.pack();
                AquaUtils.setTextured(popup);   // avoid painting a window background
                AquaVibrantSupport.addFullWindowVibrantView(popup, AquaVibrantSupport.LIGHT_STYLE);
                AquaUtils.setCornerRadius(popup, 6);

                OSXSystemProperties.addChangeListener(new ChangeListener() {
                    @Override
                    public void stateChanged(ChangeEvent e) {
                        makeClear(popup, !OSXSystemProperties.isReduceTransparency());
                    }
                });
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

    @Override
    public void hide() {
        super.hide();
    }

    @Override
    public void show() {
        super.show();

        if (!isShadowValid) {
            isShadowValid = true;
            if (popup instanceof RootPaneContainer) {
                JRootPane popupRootPane = ((RootPaneContainer) popup).getRootPane();
                ShadowMaker shadowMaker = new ShadowMaker(popupRootPane);
                // The shadow maker must run after the window has been painted.
                // Not sure how to do that reliably...
                // Sometimes invokeLater happens too quickly
                SwingUtilities.invokeLater(shadowMaker);
                Timer t = new Timer(50, shadowMaker);
                t.setRepeats(false);
                t.start();
            }
        }
    }

    private static class ShadowMaker implements ActionListener, Runnable {
        private final JRootPane rp;

        public ShadowMaker(JRootPane rp) {
            this.rp = rp;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            run();
        }

        @Override
        public void run() {
            rp.putClientProperty("apple.awt.windowShadow.revalidateNow", Double.valueOf(Math.random()));
        }
    }

    private void makeClear(Window w, boolean isClear) {

        Color clear = new Color(0, 0, 0, 0);
        Color bc = isClear ? clear : AquaImageFactory.getWindowBackgroundColorUIResource();

        w.setBackground(bc);

        if (w instanceof RootPaneContainer) {
            JRootPane rp = ((RootPaneContainer) w).getRootPane();
            rp.setBackground(clear);

            JLayeredPane lp = rp.getLayeredPane();
            lp.setOpaque(false);

            Container cp = rp.getContentPane();
            if (cp instanceof JComponent) {
                ((JComponent) cp).setOpaque(false);
            }
            cp.setBackground(clear);

            // set border to null ?

        }
    }
}
