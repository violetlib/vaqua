/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.ButtonLayoutConfiguration;

/**
 * A border for a button with a client specified button style.
 */
public class AquaNamedButtonBorder extends AquaButtonBorder {

    protected final @NotNull ButtonWidget widget;
    private final @NotNull AquaButtonExtendedTypes.WidgetInfo info;
    private final boolean allowsContent;

    public AquaNamedButtonBorder(@NotNull ButtonWidget w, @NotNull AquaButtonExtendedTypes.WidgetInfo info) {
        this.widget = w;
        this.info = info;
        this.allowsContent = determineAllowsContent(w);
    }

    private boolean determineAllowsContent(@NotNull ButtonWidget w) {
        // Certain widgets do not allow content. For example, a help button.
        // We can figure this out, but note the assumption that the answer does not
        // depend upon the size variant.

        ButtonLayoutConfiguration g = new ButtonLayoutConfiguration(w, Size.REGULAR, AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT);
        Insetter s = painter.getLayoutInfo().getButtonLabelInsets(g);
        return s != null;
    }

    @Override
    public @NotNull ButtonWidget getButtonWidget(@NotNull AbstractButton b) {
        return widget;
    }

    @Override
    protected @NotNull AquaButtonExtendedTypes.WidgetInfo getWidgetInfo(@NotNull AbstractButton b) {
        return info;
    }

    @Override
    public boolean allowsContent() {
        return allowsContent;
    }
}
