/*
 * Copyright (c) 2016-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.ImageObserver;
import javax.swing.*;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.*;

/**
 * An icon for a button whose rendering may depend upon the widget, state, button state, and appearance. The
 * rendering is based on the button's default icon.
 */

public class AquaButtonIcon implements Icon, UIResource, ImageObserver {

    public interface ImageOperatorSupplier {
        @Nullable Object getCurrentImageProcessingOperator(@NotNull AbstractButton b,
                                                           boolean isTemplate,
                                                           @NotNull PaintingContext pc);
    }

    private final @NotNull AbstractButton b;
    private final @NotNull Icon basicIcon;
    private final boolean isTemplate;
    private final @NotNull ImageOperatorSupplier operatorSupplier;

    /**
     * Create a context-sensitive button icon.
     * @param b The button.
     * @param basicIcon The basic button icon.
     * @param isTemplate True if the source image is a template image and it should be treated as such.
     * @param operatorSupplier Determines the image processing operator to apply to the source image when the icon is
     *                         painted.
     */
    public AquaButtonIcon(@NotNull AbstractButton b,
                          @NotNull Icon basicIcon,
                          boolean isTemplate,
                          @NotNull ImageOperatorSupplier operatorSupplier) {
        this.b = b;
        this.basicIcon = basicIcon;
        this.isTemplate = isTemplate;
        this.operatorSupplier = operatorSupplier;
    }

    @Override
    public int getIconWidth() {
        return basicIcon.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return basicIcon.getIconHeight();
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        PaintingContext pc = PaintingContext.getDefault();
        Object operator = operatorSupplier.getCurrentImageProcessingOperator(b, isTemplate, pc);
        Icon icon = AquaImageFactory.getProcessedImage(basicIcon, operator);
        // Using the button as the image observer can fail because it aborts drawing the image if the button
        // does not recognize the image as the proper image for the button in its current state, and its ability
        // to recognize images is not flexible enough for our usage.
        icon.paintIcon(c, g, x, y);
    }

    public boolean isTemplate() {
        return isTemplate;
    }

    @Override
    public boolean imageUpdate(Image img, int infoFlags, int x, int y, int width, int height) {
        if ((infoFlags & (FRAMEBITS|ALLBITS|SOMEBITS)) != 0) {
            b.repaint();
        }

        return (infoFlags & (ALLBITS|ABORT)) == 0;
    }
}
