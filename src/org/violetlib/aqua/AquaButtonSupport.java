/*
 * Copyright (c) 2015-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.Rectangle2D;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;

import static org.violetlib.aqua.OSXSystemProperties.macOS11;
import static org.violetlib.aqua.OSXSystemProperties.macOS26;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget.*;
import static org.violetlib.jnr.aqua.AquaUIPainter.Size.EXTRA_LARGE;
import static org.violetlib.jnr.aqua.AquaUIPainter.Size.REGULAR;

/**
 * Generic support for buttons.
 */
public class AquaButtonSupport {

    private static final AquaButtonIcon.ImageOperatorSupplier keySupplier = new MyImageOperatorSupplier();

    public AquaButtonSupport() {
    }

    public static @NotNull AquaUIPainter.State getActiveState(@NotNull AbstractButton b) {
        return (b instanceof JButton) && ((JButton)b).isDefaultButton()
          ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE;
    }

    public static @NotNull AquaUIPainter.State getPressedState(@NotNull AbstractButton b) {
        return (b instanceof JButton) && ((JButton)b).isDefaultButton()
          ? AquaUIPainter.State.PRESSED_DEFAULT : AquaUIPainter.State.PRESSED;
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

    private static boolean isApplicationDefined(@Nullable Icon ic) {
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

    /**
     * Return the basic preferred size of a button, without regard to any minimums associated with the widget.
     */
    public static @Nullable Dimension getBasicPreferredButtonSize(@NotNull AbstractButton b,
                                                                  @Nullable Dimension preferredIconSize,
                                                                  @Nullable LayoutConfiguration g) {
        CompoundLabelLayoutEngine engine = createCompoundLayoutEngine(b, preferredIconSize, g);
        if (engine == null) {
            return null;
        }
        ButtonLayoutInfo info = engine.getLayoutInfo(AquaUtils.INFINITY, AquaUtils.INFINITY);
        Dimension size = AquaUtils.size(info.contentBounds);
        return AquaUtils.extend(size, b.getInsets());
    }

    public static @Nullable CompoundLabelLayoutEngine createCompoundLayoutEngine(@NotNull AbstractButton b,
                                                                                 @Nullable Dimension iconSize,
                                                                                 @Nullable LayoutConfiguration g) {
        return createCompoundLayoutEngine(b, iconSize, g, null);
    }

    public static @Nullable CompoundLabelLayoutEngine createCompoundLayoutEngine(@NotNull AbstractButton b,
                                                                                 @Nullable Dimension iconSize,
                                                                                 @Nullable LayoutConfiguration g,
                                                                                 @Nullable CompoundLabelAlignment alignment) {
        if (b.getComponentCount() > 0) {
            return null;
        }
        if (iconSize == null) {
            Icon icon = b.getIcon();
            if (icon != null) {
                iconSize = new Dimension(icon.getIconWidth(), icon.getIconHeight());
            }
        }
        Font font = b.getFont();
        FontMetrics fm = b.getFontMetrics(font);
        View v = null;
        Object h = b.getClientProperty(BasicHTML.propertyKey);
        if (h instanceof View) {
            v = (View) h;
        }
        String text = b.getText();
        if (text == null && g != null && g.getWidget() == AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM) {
            text = "";
        } else if (text != null && text.isEmpty()) {
            text = null;
        }
        if (alignment == null) {
            alignment = getCompoundLabelAlignment(b, g);
        }
        int iconTextGap = b.getIconTextGap();
        return new CompoundLabelLayoutEngine(iconSize, v, text, fm, alignment, iconTextGap);
    }

    public static @Nullable Font getDefaultFontPropertyValue(@NotNull AbstractButton b) {
        Object o = b.getClientProperty(AquaButtonUI.DEFAULT_FONT_PROPERTY);
        if (o instanceof Font) {
            return (Font) o;
        }
        return null;
    }

    /**
     * Select a button style for a button on a toolbar.
     */
    public static @NotNull ButtonStyleInfo getToolbarButtonStyleInfo(@NotNull AbstractButton b,
                                                                     @NotNull AquaUIPainter painter) {
        // The preference is to use a fixed height style that is compatible with the native toolbar height. For backward
        // compatibility, a variable height style is used on older systems when the text of a text-only button is or may
        // be too tall for the preferred fixed height style.

        String text = b.getText();
        Icon icon = b.getIcon();
        if (text != null && !text.isEmpty() && (icon != null || AquaButtonExtendedTypes.isToolbarItemStyleSpecified(b))) {
            // If the application specifies a toolbar item, then that style is used even if there is no icon.
            // The client property must be used, as this method is involved in choosing a widget.
            return createToolbarButtonStyleInfo(b, BUTTON_TOOLBAR_ITEM);
        }

        int version = AquaPainting.getVersion();
        if (version >= macOS26) {
            return createToolbarButtonStyleInfo(b, BUTTON_TOOLBAR);
        }

        if (text == null || text.isEmpty()) {
            return createToolbarButtonStyleInfo(b, BUTTON_TOOLBAR);
        }

        ButtonWidget widget = version >= macOS11 ? BUTTON_TOOLBAR : BUTTON_TEXTURED_TOOLBAR;
        ButtonWidget w = isButtonWidgetUsable(b, widget, painter) ? widget : BUTTON_BEVEL_ROUND;
        return createToolbarButtonStyleInfo(b, w);
    }

    private static @NotNull ButtonStyleInfo createToolbarButtonStyleInfo(@NotNull AbstractButton b,
                                                                         @NotNull ButtonWidget widget) {
        AquaUIPainter.Size size = getPreferredToolbarButtonSize(b, widget);
        return new ButtonStyleInfo(widget, size);
    }

    public static @NotNull AquaUIPainter.Size
    getPreferredToolbarButtonSize(@NotNull AbstractButton b, @NotNull AquaUIPainter.GenericButtonWidget widget) {
        int version = AquaPainting.getVersion();
        if (version < macOS26) {
            AquaUIPainter.Size size = AquaUtilControlSize.getOptionalUserSizeFrom(b);
            if (size != null) {
                return size;
            }
        }
        AquaUIPainter.Size size = AquaUtils.getToolbarSize(widget);
        return size != null ? size : REGULAR;
    }

    public static @NotNull ButtonStyleInfo getButtonStyleInfo(@NotNull AbstractButton b,
                                                              @NotNull AquaUIPainter.GenericButtonWidget w) {
        if (w instanceof AquaUIPainter.SegmentedButtonWidget) {
            return getSegmentedButtonStyleInfo(b, (AquaUIPainter.SegmentedButtonWidget) w);
        }
        boolean isToolbar = AquaButtonSupport.isToolbar(b);
        AquaUIPainter.Size size = AquaUtils.getSize(b, isToolbar, w);

        if (!isToolbar && w == BUTTON_TOOLBAR_ITEM) {
            String text = b.getText();
            if (text == null || text.isEmpty()) {
                w = BUTTON_TOOLBAR;
                if (AquaNativeRendering.getSystemRenderingVersion() > AquaNativeRendering.macOS26) {
                    size = EXTRA_LARGE;
                }
            }
        }
        return new ButtonStyleInfo(w, size);
    }

    private static @NotNull ButtonStyleInfo getSegmentedButtonStyleInfo(@NotNull AbstractButton b,
                                                                        @NotNull AquaUIPainter.SegmentedButtonWidget w) {
        int version = AquaNativeRendering.getSystemRenderingVersion();

        boolean isOnToolbar = AquaUtils.isOnToolbar(b);
        AquaUIPainter.Size size;
        if (isOnToolbar && w == AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED) {
            if (version >= AquaUIPainter.macOS26) {
                w = AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SLIDER_TOOLBAR;
            } else {
                w = AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_TEXTURED_TOOLBAR;
            }
            size = AquaButtonSupport.getPreferredToolbarButtonSize(b, w);
        } else if (isOnToolbar && w == AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SEPARATED) {
            if (version >= AquaUIPainter.macOS26) {
                w = AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SEPARATED;
            } else {
                w = AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_TEXTURED_SEPARATED_TOOLBAR;
            }
            size = AquaButtonSupport.getPreferredToolbarButtonSize(b, w);
        } else if (isOnToolbar) {
            size = AquaButtonSupport.getPreferredToolbarButtonSize(b, w);
        } else {
            size = AquaUtilControlSize.getUserSizeFrom(b);
        }
        return new ButtonStyleInfo(w, size);
    }

    /**
     * Determine if a proposed button widget is usable for a button based on the fixed height (if any) imposed by the
     * widget.
     */
    public static boolean isButtonWidgetUsable(@NotNull AbstractButton b,
                                               @NotNull AquaUIPainter.GenericButtonWidget widget,
                                               @NotNull AquaUIPainter painter) {

        // If the button has no text, then any widget is usable because icons are scaled as needed.
        String text = b.getText();
        if (text == null || text.isEmpty()) {
            return true;
        }
        boolean isToolbar = AquaButtonSupport.isToolbar(b);
        AquaUIPainter.Size size = AquaUtils.getSize(b, isToolbar, widget);
        LayoutConfiguration g = createLTestLayoutConfiguration(widget, size);
        size = g.getSize();  // actual size may differ
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
        int fixedHeight = (int) Math.ceil(layoutInfo.getFixedVisualHeight());
        if (fixedHeight == 0) {
            return true;
        }
        Insetter insetter = painter.getLayoutInfo().getContentInsets(g);
        if (insetter == null || !insetter.isInvertible()) {
            return false;
        }

        // We need to predict the required size of the button to see if a fixed height widget can be used. That requires
        // knowing the font that would be used if the widget were chosen. This determination cannot rely on the current
        // configuration of the button, which we may be in the process of replacing.

        Font font = AquaUtilControlSize.isOKToInstallDefaultFont(b)
          ? AquaButtonExtendedTypes.getFont(widget, size, getGenericDefaultFont(b))
          : b.getFont();

        // If the font cannot be determined, a fixed height widget is not usable.
        if (font == null) {
            return false;
        }

        FontMetrics fm = b.getFontMetrics(font);
        int requiredHeight = AquaUtils.getTextHeight(b, fm);
        Dimension d = new Dimension(1000, requiredHeight); // width does not matter
        Dimension requiredSize = insetter.expand(d);
        return requiredSize.height <= fixedHeight;
    }

    private static @NotNull LayoutConfiguration
    createLTestLayoutConfiguration(@NotNull AquaUIPainter.GenericButtonWidget widget, @NotNull AquaUIPainter.Size size) {
        if (widget instanceof ButtonWidget) {
            ButtonWidget w = (ButtonWidget) widget;
            return new ButtonLayoutConfiguration(w, size, AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT);
        }
        if (widget instanceof AquaUIPainter.SegmentedButtonWidget) {
            AquaUIPainter.SegmentedButtonWidget w = (AquaUIPainter.SegmentedButtonWidget) widget;
            return new SegmentedButtonLayoutConfiguration(w, size, AquaUIPainter.Position.ONLY);
        }
        if (widget instanceof AquaUIPainter.ComboBoxWidget) {
            AquaUIPainter.ComboBoxWidget w = (AquaUIPainter.ComboBoxWidget) widget;
            return new ComboBoxLayoutConfiguration(w, size, AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT);
        }
        if (widget instanceof AquaUIPainter.PopupButtonWidget) {
            AquaUIPainter.PopupButtonWidget w = (AquaUIPainter.PopupButtonWidget) widget;
            return new PopupButtonLayoutConfiguration(w, size, AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT);
        }
        throw new UnsupportedOperationException("Unrecognized generic button widget: " + widget);
    }

    /**
     * Return a configuration independent font for a button with no application defined font.
     */
    public static @NotNull Font getGenericDefaultFont(@NotNull AbstractButton b) {
        Font f = getDefaultFontPropertyValue(b);
        if (f == null) {
            // should not happen
            return new Font("Default", Font.PLAIN, 12);
        }
        return f;
    }

    public static void paintIconAndText(@NotNull Graphics2D g,
                                        @NotNull AbstractButton b,
                                        @Nullable GenericButtonConfiguration bg,
                                        @NotNull Insets2D insets,
                                        @Nullable Icon2D icon,
                                        @NotNull Color textColor,
                                        @Nullable Color iconColor,
                                        @NotNull Rectangle viewRect,
                                        @Nullable Dimension iconSize) {

        LayoutConfiguration lg = bg != null ? bg.getLayoutConfiguration() : null;
        CompoundLabelLayoutEngine engine = createCompoundLayoutEngine(b, iconSize, lg);
        if (engine == null) {
            return;
        }
        ButtonLayoutInfo info = engine.getLayoutInfo(viewRect.width, viewRect.height, insets);
        Rectangle2D iconBounds = info.iconBounds;
        if (iconBounds != null && icon != null) {
            icon = Icons.scale(icon, iconBounds.getWidth(), iconBounds.getHeight());
            icon.paintIcon(g, iconBounds.getX(), iconBounds.getY(), iconColor);
        }
        if (info.labelBounds != null) {
            String text = info.substitutedLabel != null ? info.substitutedLabel : b.getText();
            paintText(g, b, info.labelBounds, textColor, text);
        }
    }

    private static @NotNull CompoundLabelAlignment getCompoundLabelAlignment(@NotNull AbstractButton b,
                                                                             @Nullable LayoutConfiguration g) {
        if (g != null && g.getWidget() == AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM) {
            return CompoundLabelAlignment.VERTICAL_BOTTOM_TEXT;
        }

        int verticalTextPosition = b.getVerticalTextPosition();
        int horizontalTextPosition = b.getHorizontalTextPosition();
        boolean isLTR = b.getComponentOrientation().isLeftToRight();
        return CompoundLabelAlignment.create(horizontalTextPosition, verticalTextPosition, isLTR);
    }

    public static void paintToolbarItemBackground(@NotNull ButtonConfiguration bg,
                                                  @NotNull PaintingContext pc,
                                                  @NotNull Graphics2D g,
                                                  @NotNull Shape shape)
    {
        AquaUIPainter.State state = bg.getState();
        AquaUIPainter.ButtonState bs = bg.getButtonState();
        boolean isIcon = true;
        BasicContextualColors colors = AquaColors.TOOLBAR_COLORS;
        AppearanceContext ac = new AppearanceContext(pc.appearance, state, bs.equals(AquaUIPainter.ButtonState.ON), isIcon);
        Color c = colors.getBackground(ac);
        g.setColor(c);
        AquaUtils.fillAntiAliased(g, shape);

        if (pc.appearance.isHighContrast() && !ac.isSelected()) {
            c = colors.getForeground(ac);
            g.setColor(c);
            AquaUtils.drawAntiAliased(g, shape);
        }
    }

    /**
     * Paint the text.
     */
    public static void paintText(@NotNull Graphics2D g,
                                 @NotNull AbstractButton b,
                                 @NotNull Rectangle2D textRect,
                                 @NotNull Color textColor,
                                 @Nullable String text) {
        if (text != null && !text.isEmpty()) {
            if (textRect.getWidth() == 0) {
                textRect = new Rectangle2D.Double(textRect.getX(), textRect.getY(), 50, textRect.getHeight());
            }
            Object o = b.getClientProperty(BasicHTML.propertyKey);
            if (o instanceof View) {
                View v = (View) o;
                v.paint(g, textRect);
            } else {
                FontMetrics fm = g.getFontMetrics();
                int mnemonicIndex = AquaMnemonicHandler.isMnemonicHidden() ? -1 : b.getDisplayedMnemonicIndex();
                g.setColor(textColor);
                float x = (float) textRect.getX();
                float y = (float) textRect.getY() + fm.getAscent();
                JavaSupport.drawStringUnderlineCharAt(b, g, text, mnemonicIndex, x, y);
            }
        }
    }

    public static boolean isColorWell(@NotNull AbstractButton b) {
        AquaUIPainter.ButtonWidget widget = getButtonWidget(b);
        return widget == AquaUIPainter.ButtonWidget.BUTTON_COLOR_WELL;
    }

    private static @Nullable ButtonLayoutConfiguration getLayoutConfiguration(@NotNull AbstractButton b) {
        Object o = b.getClientProperty(AquaButtonUI.LAYOUT_CONFIGURATION_PROPERTY);
        if (o instanceof ButtonLayoutConfiguration) {
            return (ButtonLayoutConfiguration) o;
        }
        return null;
    }

    private static @Nullable AquaUIPainter.ButtonWidget getButtonWidget(@NotNull AbstractButton b) {
        ButtonLayoutConfiguration g = getLayoutConfiguration(b);
        if (g != null) {
            return g.getButtonWidget();
        }
        return null;
    }

    /**
     * Return a value that can be used to select an appropriate icon based on the current state of the specified button.
     * @param b The button.
     * @return the button icon state.
     */
    private static @NotNull AquaButtonUI.ButtonIconState getIconState(@NotNull AbstractButton b) {
        ButtonModel model = b.getModel();
        if (!model.isEnabled()) {
            if (model.isSelected()) {
                return AquaButtonUI.ButtonIconState.DISABLED_SELECTED;
            } else {
                return AquaButtonUI.ButtonIconState.DISABLED;
            }
        } else if (model.isPressed() && model.isArmed()) {
            return AquaButtonUI.ButtonIconState.PRESSED;
        } else if (b.isRolloverEnabled() && model.isRollover()) {
            if (model.isSelected()) {
                return AquaButtonUI.ButtonIconState.ROLLOVER_SELECTED;
            } else {
                return AquaButtonUI.ButtonIconState.ROLLOVER;
            }
        } else if (model.isSelected()) {
            return AquaButtonUI.ButtonIconState.SELECTED;
        } else {
            return AquaButtonUI.ButtonIconState.DEFAULT;
        }
    }

    /**
     * Return the icon explicitly defined by a button for the specified button state.
     * If the button uses AquaButtonUI, then this method will not install any default icons on the button.
     * @param b The button.
     * @param bs The button state.
     * @return the icon defined by the button for the button state, if any. This method returns null if
     * {@code bs} is DEFAULT or unrecognized.
     */
    private static @Nullable Icon getDefinedIconForState(AbstractButton b, AquaButtonUI.ButtonIconState bs) {
        switch (bs) {
            case DISABLED:              return getDisabledIcon(b);
            case DISABLED_SELECTED:     return getDisabledSelectedIcon(b);
            case PRESSED:               return getIconForPressedState(b);
            case ROLLOVER_SELECTED:     return getIconForRolloverSelectedState(b);
            case ROLLOVER:              return b.getRolloverIcon();
            case SELECTED:              return b.getSelectedIcon();
            default:                    return null;
        }
    }

    private static @Nullable Icon getIconForPressedState(@NotNull AbstractButton b) {
        Icon icon = b.getPressedIcon();
        return icon != null ? icon : b.getSelectedIcon();
    }

    private static @Nullable Icon getIconForRolloverSelectedState(@NotNull AbstractButton b) {
        Icon icon = b.getRolloverSelectedIcon();
        return icon != null ? icon : b.getSelectedIcon();
    }

    /**
     * Obtain a special icon to use for a button. The special icon rendering is based on default icon defined by the
     * button and may be context dependent.
     * @param b The button.
     * @return the special icon for the button, or null if the button does not define a default icon.
     */
    private static @Nullable AquaButtonIcon getSpecialIcon(@NotNull AbstractButton b, @NotNull PaintingContext pc) {
        Border border = b.getBorder();
        AquaButtonBorder bb = AquaBorderSupport.get(border, AquaButtonBorder.class);
        if (bb != null) {
            boolean isTemplate = determineTemplateIconStatus(b);
            AquaButtonIcon icon = bb.createSpecialIcon(b, isTemplate, pc);
            return icon;
        }
        Icon ic = b.getIcon();
        if (ic != null) {
            boolean isTemplate = determineTemplateIconStatus(b);
            return new AquaButtonIcon(b, ic, isTemplate, keySupplier);
        }
        return null;
    }

    public static void toggleButtonStateChanged(@NotNull AbstractButton b) {
        JToggleButton leftButton = SegmentedControlModel.getLeftAdjacentButton(b);
        if (leftButton != null) {
            leftButton.repaint();
        }
    }

    public static boolean isToolbar(@NotNull AbstractButton b) {
        return Boolean.TRUE.equals(b.getClientProperty(AquaButtonUI.CACHED_TOOLBAR_STATUS_PROPERTY));
    }

    public static boolean isToolbarStyle(@NotNull AbstractButton b) {
        if (isToolbar(b)) {
            return true;
        }
        AquaButtonBorder bb = AquaBorderSupport.get(b.getBorder(), AquaButtonBorder.class);
        return bb != null && bb.isToolbarStyle(b);
    }

    public static boolean isToolbarSensitive(@NotNull AbstractButton b) {
        // Checkboxes and radio buttons are not toolbar sensitive
        return b instanceof JButton
          || b instanceof JToggleButton && !(b instanceof JCheckBox) && !(b instanceof JRadioButton);
    }

    public static void disconnectColorChooser(AbstractButton b) {
        Object o = b.getClientProperty(AquaButtonUI.COLOR_CHOOSER_OWNER_PROPERTY);
        if (o instanceof SharedColorChooserOwner) {
            SharedColorChooserOwner owner = (SharedColorChooserOwner) o;
            AquaSharedColorChooser.disconnect(owner);
            b.putClientProperty(AquaButtonUI.COLOR_CHOOSER_OWNER_PROPERTY, null);
        }
    }

    public static void toggleColorChooser(AbstractButton b) {

        Object o = b.getClientProperty(AquaButtonUI.COLOR_CHOOSER_OWNER_PROPERTY);
        if (o instanceof SharedColorChooserOwner) {
            SharedColorChooserOwner owner = (SharedColorChooserOwner) o;
            AquaSharedColorChooser.disconnect(owner);
            b.setSelected(false);
            b.putClientProperty(AquaButtonUI.COLOR_CHOOSER_OWNER_PROPERTY, null);
            return;
        }

        SharedColorChooserOwner owner = new SharedColorChooserOwner() {
            @Override
            public void applyColor(Color c) {
                b.setBackground(c);
            }

            @Override
            public void disconnected() {
                b.setSelected(false);
                b.putClientProperty(AquaButtonUI.COLOR_CHOOSER_OWNER_PROPERTY, null);
            }
        };

        Color c = b.getBackground();
        boolean enableTranslucentColors = Boolean.TRUE.equals(b.getClientProperty(AquaButtonUI.ENABLE_TRANSLUCENT_COLORS_KEY));
        if (AquaSharedColorChooser.connect(owner, c, enableTranslucentColors)) {
            b.putClientProperty(AquaButtonUI.COLOR_CHOOSER_OWNER_PROPERTY, owner);
            b.setSelected(true);
        }
    }

    public static void removeCachedIcons(AbstractButton b) {

        if (b.getSelectedIcon() instanceof UIResource) {
            b.setSelectedIcon(null);
        }

        if (getDisabledIcon(b) instanceof UIResource) {
            b.setDisabledIcon(null);
        }

        if (getDisabledSelectedIcon(b) instanceof UIResource) {
            b.setDisabledSelectedIcon(null);
        }

        if (b.getPressedIcon() instanceof UIResource) {
            b.setPressedIcon(null);
        }

        if (b.getRolloverIcon() instanceof UIResource) {
            b.setRolloverIcon(null);
        }

        if (b.getRolloverSelectedIcon() instanceof UIResource) {
            b.setRolloverSelectedIcon(null);
        }
    }

    /**
     * Obtain the icon to use based on the button state.
     */
    public static @Nullable Icon getIcon(AbstractButton b, @NotNull PaintingContext pc) {
        AquaButtonUI.ButtonIconState bs = getIconState(b);
        Icon definedIcon = getDefinedIconForState(b, bs);
        if (definedIcon != null) {
            return definedIcon;
        }
        // The special icon creates state specific renderings based on the button default icon.
        return getSpecialIcon(b, pc);
    }

    private static class MyImageOperatorSupplier implements AquaButtonIcon.ImageOperatorSupplier {
        @Override
        public @Nullable Object getCurrentImageProcessingOperator(@NotNull AbstractButton b,
                                                                  boolean isTemplate,
                                                                  @NotNull PaintingContext pc) {
            AquaUIPainter.State state = getState(b);
            if (state == AquaUIPainter.State.PRESSED) {
                return pc.appearance.isDark() ? AquaImageFactory.LIGHTEN_FOR_DISABLED : AquaImageFactory.DARKEN_FOR_PRESSED;
            }
            return null;
        }
    }

    /**
     * Identify the button state.
     */
    public static @NotNull AquaUIPainter.State getState(@NotNull AbstractButton b) {
        boolean isActive = AquaFocusHandler.isActive(b);

        if (!b.isEnabled()) {
            return isActive ? AquaUIPainter.State.DISABLED : AquaUIPainter.State.DISABLED_INACTIVE;
        }

        // Starting with (at least) macOS 11, toolbar buttons exhibit rollover behavior even in an inactive window
        int version = AquaNativeRendering.getSystemRenderingVersion();
        if (version >= AquaNativeRendering.macOS11) {
            if (isToolbarStyle(b)) {
                ButtonModel model = b.getModel();
                if (model.isArmed() && model.isPressed()) {
                    return getPressedState(b);
                }
                if (isRollover(b)) {
                    return AquaUIPainter.State.ROLLOVER;
                }
            }
        }

        if (!isActive) {
            return AquaUIPainter.State.INACTIVE;
        }
        ButtonModel model = b.getModel();
        if (model.isArmed() && model.isPressed()) {
            return getPressedState(b);
        }
        if (isRollover(b)) {
            return AquaUIPainter.State.ROLLOVER;
        }
        return getActiveState(b);
    }

    private static boolean isRollover(@NotNull AbstractButton b) {
        AquaButtonBorder bb = AquaBorderSupport.get(b.getBorder(), AquaButtonBorder.class);
        if (bb != null) {
            return bb.isRollover(b);
        }
        return b.isRolloverEnabled() && b.getModel().isRollover();
    }
}
