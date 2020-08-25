/*
 * Copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.violetlib.geom.ExpandableOutline;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonState;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.ButtonLayoutConfiguration;
import org.violetlib.jnr.aqua.LayoutConfiguration;

import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.*;

import javax.swing.*;
import java.awt.*;

/**
 * The base class for a radio button or check box button border.
 */
public abstract class AquaLabeledButtonBorder extends AquaNamedButtonBorder {

    public AquaLabeledButtonBorder(ButtonWidget widget, AquaButtonExtendedTypes.WidgetInfo info) {
        super(widget, info);
    }

    public Shape getFocusRingOutline(AbstractButton b, Rectangle iconBounds) {
        // The focus ring is drawn around the icon, not the entire component
        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g != null) {
            painter.configure(iconBounds.width, iconBounds.height);
            Shape s = painter.getOutline(g);
            if (s != null) {
                return ExpandableOutline.createTranslatedShape(s, iconBounds.x, iconBounds.y);
            }
        }

        return null;
    }

    protected AquaButtonLabeledUI.RecyclableSizingIcon createDefaultIcon(Size size) {
        ButtonLayoutConfiguration g = new ButtonLayoutConfiguration(widget, size, AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT);
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
        int width = (int) layoutInfo.getFixedVisualWidth();
        int height = (int) layoutInfo.getFixedVisualHeight();
        return new AquaButtonLabeledUI.RecyclableSizingIcon(width, height);
    }

    @Override
    protected ButtonState getButtonState(AbstractButton b) {
        return b.getModel().isSelected() ? isIndeterminate(b) ? MIXED : ON : OFF;
    }

    @Override
    protected Insets getSpecialMarginAdjustments(AbstractButton b) {
        return null;
    }

    static boolean isIndeterminate(final AbstractButton b) {
        return "indeterminate".equals(b.getClientProperty(AquaButtonUI.SELECTED_STATE_KEY));
    }
}
