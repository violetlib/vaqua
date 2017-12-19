/*
 * Copyright (c) 2015-2017 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

/**
 * Provide access to the shared NSColorPanel.
 */
public class AquaNativeColorChooser {

    private static boolean isInitialized;
    private static boolean isCreated;
    private static SharedColorChooserOwner currentOwner;

    private static SharedColorChooserOwner myOwner;

    public static boolean display(SharedColorChooserOwner owner, Color initialColor, boolean enableTranslucentColors) {
        if (!isInitialized) {
            isInitialized = true;

            if (!AquaNativeSupport.load()) {
                return false;
            }

            myOwner = new SharedColorChooserOwner() {
                @Override
                public void applyColor(Color c) {
                    if (currentOwner != null) {
                        currentOwner.applyColor(c);
                    }
                }

                @Override
                public void disconnected() {
                    if (currentOwner != null) {
                        SharedColorChooserOwner o = currentOwner;
                        currentOwner = null;
                        o.disconnected();
                    }
                }
            };
            isCreated = create(myOwner);
        }

        if (isCreated) {
            currentOwner = owner;
            float[] cs = initialColor.getRGBComponents(null);
            float alpha = enableTranslucentColors ? cs[3] : 0;
            show(cs[0], cs[1], cs[2], alpha, enableTranslucentColors);
            return true;
        }

        return false;
    }

    public static void disconnect() {
        currentOwner = null;
        hide();
    }

    private static native boolean create(SharedColorChooserOwner owner);
    private static native void show(float r, float g, float b, float a, boolean isAlpha);
    private static native void hide();
}
