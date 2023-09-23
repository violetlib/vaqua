/*
 * Copyright (c) 2015-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Generic support for buttons.
 */

public class AquaButtonSupport {

    private static final AquaButtonIcon.ImageOperatorSupplier keySupplier = new MyImageOperatorSupplier();

    /**
     * Create a special icon to use for a button. The icon rendering may be context dependent.
     */
    public static @NotNull AquaButtonIcon createIcon(@NotNull AbstractButton b) {
        boolean isTemplate = determineTemplateIconStatus(b);
        return new AquaButtonIcon(b, isTemplate, keySupplier);
    }

    /**
     * Identify the button state.
     */
    public static @NotNull AquaUIPainter.State getState(@NotNull AbstractButton b) {
        boolean isActive = AquaFocusHandler.isActive(b);

        if (!b.isEnabled()) {
            return isActive ? AquaUIPainter.State.DISABLED : AquaUIPainter.State.DISABLED_INACTIVE;
        }

        if (!isActive) {
            return AquaUIPainter.State.INACTIVE;
        }

        ButtonModel model = b.getModel();
        if (model.isArmed() && model.isPressed()) {
            return getPressedState(b);
        }

        if (b.isRolloverEnabled() && isRollover(b)) {
            return AquaUIPainter.State.ROLLOVER;
        }

        return getActiveState(b);
    }

    public static @NotNull AquaUIPainter.State getActiveState(@NotNull AbstractButton b) {
        return (b instanceof JButton) && ((JButton)b).isDefaultButton() ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE;
    }

    public static @NotNull AquaUIPainter.State getPressedState(@NotNull AbstractButton b) {
        return (b instanceof JButton) && ((JButton)b).isDefaultButton() ? AquaUIPainter.State.PRESSED_DEFAULT : AquaUIPainter.State.PRESSED;
    }

    private static boolean isRollover(@NotNull AbstractButton b)
    {
        ButtonModel model = b.getModel();
        return model.isRollover();
    }

    /**
     * Determine whether or not the button is eligible for painting the icon as a template.
     * To be eligible, the button must define an icon that is a template image, and it must not have any other
     * application provided icon.
     */
    public static boolean determineTemplateIconStatus(AbstractButton b) {
        Icon standardIcon = b.getIcon();
        if (standardIcon instanceof ImageIcon) {
            ImageIcon im = (ImageIcon) standardIcon;
            Image image = im.getImage();

            return !isApplicationDefined(b.getPressedIcon())
                    && !isApplicationDefined(getDisabledIcon(b))
                    && !isApplicationDefined(b.getSelectedIcon())
                    && !isApplicationDefined(getDisabledSelectedIcon(b))
                    && !isApplicationDefined(b.getRolloverIcon())
                    && !isApplicationDefined(b.getRolloverSelectedIcon())
                    && AquaImageFactory.isTemplateImage(image);
        }
        return false;
    }

    private static boolean isApplicationDefined(Icon ic) {
        return ic != null && !(ic instanceof UIResource);
    }

    /**
     * Return the disabled icon explicitly assigned to a button. This method is useful only for buttons that use
     * AquaButtonUI for their UI. It inhibits the automatic creation of a disabled icon by the UI when none is defined
     * on the button.
     *
     * @param b the button.
     *
     * @return the disabled icon explicitly assigned to the button.
     */
    public static Icon getDisabledIcon(AbstractButton b) {
        boolean oldValue = AquaLookAndFeel.suppressCreationOfDisabledButtonIcons;
        AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = true;
        try {
            return b.getDisabledIcon();
        } finally {
            AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = oldValue;
        }
    }

    /**
     * Return the disabled selected icon explicitly assigned to a button. This method is useful only for buttons that
     * use AquaButtonUI for their UI. It inhibits the automatic creation of a disabled selected icon by the UI when none
     * is defined on the button.
     *
     * @param b the button.
     *
     * @return the disabled selected icon explicitly assigned to the button.
     */
    public static Icon getDisabledSelectedIcon(AbstractButton b) {
        boolean oldValue = AquaLookAndFeel.suppressCreationOfDisabledButtonIcons;
        AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = true;
        try {
            return b.getDisabledSelectedIcon();
        } finally {
            AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = oldValue;
        }
    }

    public static @Nullable Dimension getPreferredButtonSize(@NotNull AbstractButton b,
                                                             int textIconGap,
                                                             @Nullable Dimension iconSize) {
        if (b.getComponentCount() > 0) {
            return null;
        }

        if (iconSize == null) {
            Icon icon = b.getIcon();
            if (icon != null) {
                iconSize = new Dimension(icon.getIconWidth(), icon.getIconHeight());
            }
        }

        String text = b.getText();
        Font font = b.getFont();
        FontMetrics fm = b.getFontMetrics(font);
        Rectangle iconR = new Rectangle();
        Rectangle textR = new Rectangle();
        Rectangle viewR = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);

        AquaUtils.layoutCompoundLabel(
                b, fm, text, iconSize,
                b.getVerticalAlignment(), b.getHorizontalAlignment(),
                b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                viewR, iconR, textR, (text == null ? 0 : textIconGap)
        );

        // The preferred size of the button is the size of the text and icon rectangles plus the insets.
        Rectangle r = iconR.union(textR);
        Insets insets = b.getInsets();
        r.width += insets.left + insets.right;
        r.height += insets.top + insets.bottom;
        return r.getSize();
    }

    /**
     * Determine the preferred content size for a button. The preferred content size does not include the button insets
     * and must be based only on the button text, icon, and the specified parameters. This code does not handle toolbar
     * wells, and it does not need to. Only used for button styles with fixed heights.
     */
    public static Dimension getPreferredContentSize(AbstractButton b, Font font, int textIconGap) {
        Icon icon = b.getIcon();
        Dimension iconSize = icon != null ? new Dimension(icon.getIconWidth(), icon.getIconHeight()) : null;
        String text = b.getText();
        FontMetrics fm = b.getFontMetrics(font);

        Rectangle iconR = new Rectangle();
        Rectangle textR = new Rectangle();
        Rectangle viewR = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);

        AquaUtils.layoutCompoundLabel(
                b, fm, text, iconSize,
                b.getVerticalAlignment(), b.getHorizontalAlignment(),
                b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                viewR, iconR, textR, (text == null ? 0 : textIconGap)
        );

        Rectangle r = iconR.union(textR);
        return r.getSize();
    }

    private static class MyImageOperatorSupplier implements AquaButtonIcon.ImageOperatorSupplier {
        @Override
        public @Nullable Object getCurrentImageProcessingOperator(@NotNull AbstractButton b, boolean isTemplate) {
            AquaUIPainter.State state = getState(b);
            if (state == AquaUIPainter.State.PRESSED) {
                AquaAppearance appearance = AppearanceManager.ensureAppearance(b);
                return appearance.isDark() ? AquaImageFactory.LIGHTEN_FOR_DISABLED : AquaImageFactory.DARKEN_FOR_PRESSED;
            }
            if (shouldUseDisabledIcon(state)) {
                AquaAppearance appearance = AppearanceManager.ensureAppearance(b);
                return appearance.isDark() ? AquaImageFactory.DARKEN_FOR_PRESSED : AquaImageFactory.LIGHTEN_FOR_DISABLED;
            }
            return null;
        }
    }

    private static boolean shouldUseDisabledIcon(@NotNull AquaUIPainter.State state)
    {
        return state == AquaUIPainter.State.DISABLED || state == AquaUIPainter.State.DISABLED_INACTIVE;
    }
}
