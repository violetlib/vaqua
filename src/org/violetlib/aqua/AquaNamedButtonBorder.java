/*
 * Copyright (c) 2015-2026 Alan Snyder.
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

    protected final @NotNull ButtonWidget initialWidget;
    private final @NotNull AquaButtonExtendedTypes.WidgetInfo info;
    private final boolean allowsContent;

    public AquaNamedButtonBorder(@NotNull ButtonWidget w, @NotNull AquaButtonExtendedTypes.WidgetInfo info) {
        this.initialWidget = w;
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
    public boolean isToolbarStyle(@NotNull AbstractButton b) {
        // This method is used in the selection of the button widget, so it must use the client property.
        Object o = b.getClientProperty(AquaButtonUI.BUTTON_TYPE);
        if (o instanceof String) {
            String s = (String) o;
            return s.contains("toolbar") || s.contains("Toolbar");
        }
        return false;
    }

    @Override
    public @NotNull ButtonStyleInfo getButtonStyleInfo(@NotNull AbstractButton b) {
        return AquaButtonSupport.getButtonStyleInfo(b, initialWidget);
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
