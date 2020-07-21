/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.geom.ExpandableOutline;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonState;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;

import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.*;

/**
 * The base class for a radio button or check box button border.
 * A radio button or check box uses a natively rendered icon. The button icon is not used.
 */
public abstract class AquaLabeledButtonBorder extends AquaNamedButtonBorder {

    public AquaLabeledButtonBorder(@NotNull ButtonWidget widget, @NotNull AquaButtonExtendedTypes.WidgetInfo info) {
        super(widget, info);
    }

    @Override
    public void paintButton(@NotNull Graphics2D g,
                            @NotNull AbstractButton b,
                            @Nullable Icon icon,
                            @NotNull Rectangle viewRect) {
        GenericButtonConfiguration c = getConfiguration(b, viewRect.width, viewRect.height);
        if (c instanceof ButtonConfiguration) {
            ButtonConfiguration bg = (ButtonConfiguration) c;
            Insets insets = new Insets(0, 0, 0, 0);
            Rectangle iconRect = new Rectangle();
            Rectangle textRect = new Rectangle();
            Dimension iconSize = getIconSize((ButtonLayoutConfiguration) bg);
            String text = AquaButtonUI.layoutAndGetText(g, b, insets, viewRect, iconRect, textRect, iconSize);
            paintBackground(g, b, bg, iconRect);
            Color textColor = getForegroundColor(b, bg, false);
            AquaButtonUI.paintText(g, b, textRect, textColor, text);
        }
    }

    private @Nullable ButtonLayoutConfiguration getButtonLayoutConfiguration(@NotNull AbstractButton b) {
        LayoutConfiguration bg = getLayoutConfiguration(b);
        if (bg instanceof ButtonLayoutConfiguration) {
            return (ButtonLayoutConfiguration) bg;
        }
        return null;
    }

    @Override
    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        // The focus ring is drawn around the icon, not the entire component
        AbstractButton b = (AbstractButton) c;
        ButtonLayoutConfiguration g = getButtonLayoutConfiguration(b);
        if (g != null) {
            Insets insets = new Insets(0, 0, 0, 0);
            Rectangle viewRect = new Rectangle(0, 0, c.getWidth(), c.getHeight());
            Rectangle iconRect = new Rectangle();
            Rectangle textRect = new Rectangle();
            Dimension iconSize = getIconSize(g);
            // Assuming that the location of the icon does not depend upon the text
            AquaButtonUI.layoutAndGetText(null, b, insets, viewRect, iconRect, textRect, iconSize);
            AppearanceManager.ensureAppearance(b);
            AquaUtils.configure(painter, b, iconRect.width, iconRect.height);
            Shape s = painter.getOutline(g);
            if (s != null) {
                return ExpandableOutline.createTranslatedShape(s, iconRect.x, iconRect.y);
            }
        }
        return null;
    }

    @Override
    protected @Nullable Dimension getIconSize(@NotNull LayoutConfiguration g) {
        LayoutInfo info = painter.getLayoutInfo().getLayoutInfo(g);
        int iconWidth = (int) Math.ceil(info.getFixedVisualWidth());
        int iconHeight = (int) Math.ceil(info.getFixedVisualHeight());
        return new Dimension(iconWidth, iconHeight);
    }

    @Override
    protected @NotNull ButtonState getButtonState(@NotNull AbstractButton b) {
        return isIndeterminate(b) ? MIXED : b.getModel().isSelected() ? ON : OFF;
    }

    @Override
    protected @Nullable Insets getSpecialMarginAdjustments(@NotNull AbstractButton b) {
        return null;
    }

    static boolean isIndeterminate(@NotNull AbstractButton b) {
        return "indeterminate".equals(b.getClientProperty(AquaButtonUI.SELECTED_STATE_KEY));
    }
}
