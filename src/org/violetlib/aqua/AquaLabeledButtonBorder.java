/*
 * Copyright (c) 2015-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.*;
import org.violetlib.geom.ExpandableOutline;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonState;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.ButtonConfiguration;
import org.violetlib.jnr.aqua.ButtonLayoutConfiguration;
import org.violetlib.jnr.aqua.GenericButtonConfiguration;
import org.violetlib.jnr.aqua.LayoutConfiguration;

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
                            @NotNull Rectangle viewRect,
                            @NotNull PaintingContext pc) {
        GenericButtonConfiguration c = getConfiguration(b, pc, viewRect.width, viewRect.height);
        if (c instanceof ButtonConfiguration) {
            ButtonConfiguration bg = (ButtonConfiguration) c;
            Dimension viewSize = getVisualSize(bg);
            LayoutConfiguration lg = bg.getLayoutConfiguration();

            CompoundLabelLayoutEngine engine
              = AquaButtonSupport.createCompoundLayoutEngine(b, viewSize, lg, painter);
            if (engine == null) {
                return;
            }
            Insets2D s = getButtonContentInsets(b);
            ButtonLayoutInfo info = engine.getLayoutInfo(viewRect.width, viewRect.height, s);
            if (info.iconBounds != null) {
                paintBackground(g, b, bg, info.iconBounds, pc);
            }
            if (info.labelBounds != null) {
                String text = info.substitutedLabel != null ? info.substitutedLabel : b.getText();
                Color textColor = getForegroundColor(b, bg, pc, false);
                AquaButtonSupport.paintText(g, b, info.labelBounds, textColor, text);
            }
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
            Dimension iconSize = getVisualSize(g);
            CompoundLabelLayoutEngine engine
              = AquaButtonSupport.createCompoundLayoutEngine(b, iconSize, g, painter);
            if (engine == null) {
                return null;
            }
            Insets2D s = getButtonContentInsets(b);
            ButtonLayoutInfo info = engine.getLayoutInfo(c.getWidth(), c.getHeight(), s);
            if (info.iconBounds != null) {
                int ww = (int) Math.ceil(info.iconBounds.getWidth());
                int hh = (int) Math.ceil(info.iconBounds.getHeight());
                AquaUtils.configure(painter, null, b, ww, hh);
                Shape outline = painter.getOutline(g);
                if (outline != null) {
                    return ExpandableOutline.createTranslatedShape(outline, info.iconBounds.getX(), info.iconBounds.getY());
                }
            }
        }
        return null;
    }

    @Override
    protected @Nullable Dimension getRequiredIconSize(@NotNull LayoutConfiguration g, @Nullable Icon icon) {
        return getVisualSize(g);
    }

    private @NotNull Dimension getVisualSize(@NotNull LayoutConfiguration g) {
        LayoutInfo info = painter.getLayoutInfo().getLayoutInfo(g);
        int iconWidth = (int) Math.ceil(info.getFixedVisualWidth());
        int iconHeight = (int) Math.ceil(info.getFixedVisualHeight());
        return new Dimension(iconWidth, iconHeight);
    }

    @Override
    protected @Nullable Dimension getMaximumButtonSize(@Nullable LayoutConfiguration g) {
        return null;
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
