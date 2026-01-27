/*
 * Copyright (c) 2015-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.RoundRectangle2D;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;

import static org.violetlib.aqua.OSXSystemProperties.macOS11;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget.*;
import static org.violetlib.jnr.aqua.AquaUIPainter.Size.*;

/**
 * Generic support for buttons.
 */
public class AquaButtonSupport {

    private static final AquaButtonIcon.ImageOperatorSupplier keySupplier = new MyImageOperatorSupplier();

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
        return (b instanceof JButton) && ((JButton)b).isDefaultButton()
          ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE;
    }

    public static @NotNull AquaUIPainter.State getPressedState(@NotNull AbstractButton b) {
        return (b instanceof JButton) && ((JButton)b).isDefaultButton()
          ? AquaUIPainter.State.PRESSED_DEFAULT : AquaUIPainter.State.PRESSED;
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
                                                                  @Nullable Dimension iconSize,
                                                                  @Nullable LayoutConfiguration g,
                                                                  @Nullable AquaUIPainter painter) {
        CompoundLabelLayoutEngine engine = createCompoundLayoutEngine(b, iconSize, g, painter);
        if (engine == null) {
            return null;
        }
        ButtonLayoutInfo info = engine.getLayoutInfo(Short.MAX_VALUE, Short.MAX_VALUE);
        return AquaUtils.extend(info.contentBounds.getSize(), b.getInsets());
    }

    public static @Nullable CompoundLabelLayoutEngine createCompoundLayoutEngine(@NotNull AbstractButton b,
                                                                                 @Nullable Dimension iconSize,
                                                                                 @Nullable LayoutConfiguration g,
                                                                                 @Nullable AquaUIPainter painter) {
        return createCompoundLayoutEngine(b, iconSize, g, painter, null);
    }

    public static @Nullable CompoundLabelLayoutEngine createCompoundLayoutEngine(@NotNull AbstractButton b,
                                                                                 @Nullable Dimension iconSize,
                                                                                 @Nullable LayoutConfiguration g,
                                                                                 @Nullable AquaUIPainter painter,
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
        return new CompoundLabelLayoutEngine(g, iconSize, v, text, fm, alignment, iconTextGap, painter);
    }

    public static @Nullable Font getDefaultFontPropertyValue(@NotNull AbstractButton b) {
        Object o = b.getClientProperty(AquaButtonUI.DEFAULT_FONT_PROPERTY);
        if (o instanceof Font) {
            return (Font) o;
        }
        return null;
    }

    public static @NotNull ButtonStyleInfo getToolbarItemStyleInfo(@NotNull AbstractButton b,
                                                                   @NotNull AquaUIPainter painter)
    {
        // The preference is to use a fixed height style that is compatible with the native toolbar height. For backward
        // compatibility, a variable height style is used on older systems when the text of a text-only button is or may
        // be too tall for the preferred fixed height style.

        String text = b.getText();
        Icon icon = b.getIcon();
        if (icon != null && text != null && !text.isEmpty()) {
            return createToolbarItemStyleInfo(b, BUTTON_TOOLBAR_ITEM);
        }

        int version = AquaPainting.getVersion();
        if (version >= 1600) {
            AquaUIPainter.Size specifiedSize = AquaUtilControlSize.getOptionalUserSizeFrom(b);
            if (specifiedSize == null) {
                return new ButtonStyleInfo(BUTTON_TOOLBAR, EXTRA_LARGE);
            }
            return new ButtonStyleInfo(BUTTON_TOOLBAR, specifiedSize);
        }

        if (text == null || text.isEmpty()) {
            return createToolbarItemStyleInfo(b, BUTTON_TOOLBAR);
        }

        ButtonWidget widget = BUTTON_TEXTURED_TOOLBAR;
        ButtonWidget w = isButtonWidgetUsable(b, widget, painter) ? widget : BUTTON_BEVEL_ROUND;
        return createToolbarItemStyleInfo(b, w);
    }

    private static @NotNull ButtonStyleInfo createToolbarItemStyleInfo(@NotNull AbstractButton b,
                                                                       @NotNull ButtonWidget widget) {
        AquaUIPainter.Size size = getPreferredToolbarButtonSize(b, widget);
        return new ButtonStyleInfo(widget, size);
    }

    public static @NotNull AquaUIPainter.Size
    getPreferredToolbarButtonSize(@NotNull AbstractButton b, @NotNull AquaUIPainter.GenericButtonWidget widget) {
        AquaUIPainter.Size size = AquaUtilControlSize.getOptionalUserSizeFrom(b);
        if (size != null) {
            return size;
        }
        int version = AquaPainting.getVersion();
        if (version >= macOS11) {
            // EXTRA_LARGE looks better for ordinary buttons, but LARGE is a better match for EXTRA_LARGE popup buttons
            return widget instanceof AquaUIPainter.SegmentedButtonWidget ? EXTRA_LARGE : LARGE;
        }
        return REGULAR;
    }

    public static @NotNull ButtonStyleInfo getButtonStyleInfo(@NotNull AbstractButton b,
                                                              @NotNull AquaUIPainter.GenericButtonWidget w) {
        if (w instanceof AquaUIPainter.SegmentedButtonWidget) {
            return getSegmentedButtonStyleInfo(b, (AquaUIPainter.SegmentedButtonWidget) w);
        }
        AquaUIPainter.Size size = AquaUtils.getSize(b, false, w);
        return new ButtonStyleInfo(w, size);
    }

    private static @NotNull ButtonStyleInfo getSegmentedButtonStyleInfo(@NotNull AbstractButton b,
                                                                        @NotNull AquaUIPainter.SegmentedButtonWidget w) {
        int version = AquaNativeRendering.getSystemRenderingVersion();

        boolean isOnToolbar = AquaUtils.isOnToolbar(b);
        AquaUIPainter.Size size;
        if (isOnToolbar && w == AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED) {
            if (version >= 160000) {
                w = AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SLIDER_TOOLBAR;
            } else {
                w = AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_TEXTURED_TOOLBAR;
            }
            size = AquaButtonSupport.getPreferredToolbarButtonSize(b, w);
        } else if (isOnToolbar && w == AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SEPARATED) {
            if (version >= 160000) {
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
        AquaUIPainter.Size size = AquaUtils.getSize(b, true, widget);
        LayoutConfiguration g = createLTestLayoutConfiguration(widget, size);
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
                                        @NotNull PaintingContext pc,
                                        @NotNull AbstractButton b,
                                        @Nullable GenericButtonConfiguration bg,
                                        @Nullable AquaUIPainter painter,
                                        @NotNull Insets insets,
                                        @Nullable Icon icon,
                                        @NotNull Color textColor,
                                        @Nullable Color iconColor,
                                        @NotNull Rectangle viewRect,
                                        @Nullable Dimension iconSize,
                                        boolean isSplit) {

        LayoutConfiguration lg = bg != null ? bg.getLayoutConfiguration() : null;
        CompoundLabelLayoutEngine engine = createCompoundLayoutEngine(b, iconSize, lg, painter);
        if (engine == null) {
            return;
        }
        ButtonLayoutInfo info = engine.getLayoutInfo(viewRect.width, viewRect.height, insets);
        if (icon != null && info.iconBounds != null) {
            if (isSplit) {
                assert bg != null;
                assert painter != null;
                paintSplitIconBackground(g, pc, b, bg, painter, info);
            }
            paintIcon(g, b, icon, info.iconBounds, iconColor);
        }
        if (info.labelBounds != null) {
            String text = info.substitutedLabel != null ? info.substitutedLabel : b.getText();
            paintText(g, b, info.labelBounds, textColor, text);
        }
    }

//    public static @NotNull String layoutAndGetText(@Nullable Graphics2D g,
//                                                   @NotNull AbstractButton b,
//                                                   @NotNull Insets i,
//                                                   @NotNull Rectangle viewRect,
//                                                   @NotNull Rectangle iconRect,
//                                                   @NotNull Rectangle textRect,
//                                                   @Nullable Dimension iconSize) {
//        // re-initialize the view rect to the selected insets
//        viewRect.x = i.left;
//        viewRect.y = i.top;
//        viewRect.width = b.getWidth() - (i.right + viewRect.x);
//        viewRect.height = b.getHeight() - (i.bottom + viewRect.y);
//
//        // reset the text and icon rects
//        textRect.x = textRect.y = textRect.width = textRect.height = 0;
//        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
//
//        // set up the font metrics
//        // If no graphics context is provided, the request is to determine the icon location under the
//        // assumption that the text does not matter.
//        FontMetrics fm = null;
//        if (g != null) {
//            g.setFont(b.getFont());
//            fm = g.getFontMetrics();
//        }
//
//        if (iconSize == null) {
//            Icon icon = b.getIcon();
//            if (icon != null) {
//                iconSize = new Dimension(icon.getIconWidth(), icon.getIconHeight());
//            }
//        }
//
//        return layoutCompoundLabel(b, fm, iconSize, viewRect, iconRect, textRect, b.getIconTextGap());
//    }

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

//    private static @NotNull String layoutCompoundLabel(@NotNull AbstractButton b,
//                                                       @Nullable FontMetrics fm,
//                                                       @Nullable Dimension iconSize,
//                                                       @NotNull Rectangle viewRect,
//                                                       @NotNull Rectangle iconRect,
//                                                       @NotNull Rectangle textRect,
//                                                       int iconTextGap) {
//
//        String text = b.getText();
//
//        CompoundLabelAlignment alignment = getCompoundLabelAlignment(b);
//
//        AquaUIPainter.ButtonWidget bw = getButtonWidget(b);
//        if (bw == AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM && AquaPainting.getSystemRenderingVersion() >= 1600) {
//            alignment = CompoundLabelAlignment.VERTICAL_BOTTOM_TEXT;
//            iconTextGap = 11; // includes the icon button content bottom inset of 6
//        }
//
//        return AquaUtils.layoutCompoundLabel(b, fm, text, iconSize, alignment,
//          viewRect, iconRect, textRect, text == null ? 0 : iconTextGap);
//    }

    private static void paintSplitIconBackground(@NotNull Graphics2D g,
                                                 @NotNull PaintingContext pc,
                                                 @NotNull AbstractButton b,
                                                 @NotNull GenericButtonConfiguration gg,
                                                 @NotNull AquaUIPainter painter,
                                                 @NotNull ButtonLayoutInfo info) {
        if (info instanceof SplitToolbarItemLayoutInfo) {
            SplitToolbarItemLayoutInfo si = (SplitToolbarItemLayoutInfo) info;
            if (gg instanceof ButtonConfiguration) {
                ButtonConfiguration bg = (ButtonConfiguration) gg;

                int x = si.iconOutlineBounds.x;
                int y = si.iconOutlineBounds.y;
                int width = si.iconOutlineBounds.width;
                int height = si.iconOutlineBounds.height;

                int version = AquaPainting.getVersion();
                if (version < 1600) {
                    int d = 1;
                    RoundRectangle2D shape = new RoundRectangle2D.Double(x, y - d, width - 1, height + 2 * d, 8, 8);
                    paintToolbarItemBackground(b, bg, pc, g, shape);
                } else {
                    AquaUIPainter.Size sz = bg.getSize();
                    AquaUIPainter.ButtonWidget w = AquaUIPainter.ButtonWidget.BUTTON_GLASS;
                    ButtonConfiguration ig = new ButtonConfiguration(w, sz, bg.getState(), bg.isFocused(),
                      bg.getButtonState(), bg.getLayoutDirection());
                    AquaUtils.configure(painter, pc.appearance, b, width, height);
                    org.violetlib.jnr.Painter p = painter.getPainter(ig);
                    p.paint(g, x, y);
                }
            }
        }
    }

    public static void paintToolbarItemBackground(@NotNull AbstractButton b,
                                                  @NotNull ButtonConfiguration bg,
                                                  @NotNull PaintingContext pc,
                                                  @NotNull Graphics2D g,
                                                  @NotNull Shape shape)
    {
        AquaButtonExtendedTypes.WidgetInfo wi = AquaButtonExtendedTypes.getWidgetInfo(bg.getButtonWidget());
        AquaUIPainter.State state = bg.getState();
        AquaUIPainter.ButtonState bs = bg.getButtonState();
        boolean useNonexclusive = false;
        boolean isIcon = true;
        if (bg.getState() == AquaUIPainter.State.PRESSED) {
            Color c = AquaColors.getBackground(b, pc, "controlBackground", EffectName.EFFECT_DEEP_PRESSED);
            g.setColor(c);
            AquaUtils.fillAntiAliased(g, shape);
        }
        if (bg.getState() == AquaUIPainter.State.ROLLOVER) {
            Color c = AquaColors.getBackground(b, pc, "controlBackground", EffectName.EFFECT_ROLLOVER);
            g.setColor(c);
            AquaUtils.fillAntiAliased(g, shape);
        } else if (bg.getButtonState() == AquaUIPainter.ButtonState.ON) {
            Color c = AquaColors.getBackground(b, pc, "controlBackground", EffectName.EFFECT_PRESSED);
            g.setColor(c);
            AquaUtils.fillAntiAliased(g, shape);
        }
        if (pc.appearance.isHighContrast()) {
            Color c = wi.getForeground(state, bs, pc.appearance, useNonexclusive, isIcon);
            g.setColor(c);
            AquaUtils.drawAntiAliased(g, shape);
        }
    }

    /**
     * Paint the icon.
     */
    private static void paintIcon(@NotNull Graphics2D g,
                                  @NotNull AbstractButton b,
                                  @NotNull Icon icon,
                                  @NotNull Rectangle iconRect,
                                  @Nullable Color iconColor) {
        if (iconColor != null && !(icon instanceof AquaButtonIcon) && AquaImageFactory.isTemplateIcon(icon)) {
            icon = AquaImageFactory.getProcessedImage(icon, iconColor);
        }
        Graphics2D gg = null;
        if (icon.getIconWidth() != iconRect.width || icon.getIconHeight() != iconRect.height) {
            gg = (Graphics2D) g.create();
            g = gg;
            gg.translate(iconRect.x, iconRect.y);
            gg.scale(iconRect.getWidth() / icon.getIconWidth(), iconRect.getHeight() / icon.getIconHeight());
            gg.translate(-iconRect.x, -iconRect.y);
        }
        icon.paintIcon(b, g, iconRect.x, iconRect.y);
        if (gg != null) {
            gg.dispose();
        }
    }

    /**
     * Paint the text.
     */
    public static void paintText(@NotNull Graphics2D g,
                                 @NotNull AbstractButton b,
                                 @NotNull Rectangle textRect,
                                 @NotNull Color textColor,
                                 @Nullable String text) {
        if (text != null && !text.isEmpty()) {
            if (textRect.width == 0) {
                textRect.width = 50;
            }
            Object o = b.getClientProperty(BasicHTML.propertyKey);
            if (o instanceof View) {
                View v = (View) o;
                v.paint(g, textRect);
            } else {
                FontMetrics fm = g.getFontMetrics();
                int mnemonicIndex = AquaMnemonicHandler.isMnemonicHidden() ? -1 : b.getDisplayedMnemonicIndex();
                g.setColor(textColor);
                int x = textRect.x;
                int y = textRect.y + fm.getAscent();
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

    private static @Nullable AquaUIPainter.ButtonWidget getButtonWidget(AbstractButton b) {
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
//        Object o = b.getClientProperty(AquaButtonUI.SPECIAL_ICON_PROPERTY);
//        if (o instanceof AquaButtonIcon) {
//            return (AquaButtonIcon) o;
//        }
        Border border = b.getBorder();
        AquaButtonBorder bb = AquaBorderSupport.get(border, AquaButtonBorder.class);
        if (bb != null) {
            boolean isTemplate = determineTemplateIconStatus(b);
            AquaButtonIcon icon = bb.createSpecialIcon(b, isTemplate, pc);
            //b.putClientProperty(AquaButtonUI.SPECIAL_ICON_PROPERTY, icon);
            return icon;
        }
        Icon ic = b.getIcon();
        if (ic != null) {
            boolean isTemplate = determineTemplateIconStatus(b);
            return new AquaButtonIcon(b, isTemplate, keySupplier);
        }
        return null;
    }

//    /**
//     * This method is called by AbstractButton via the LAF to obtain an icon to use when a button is disabled.
//     * @param b The button.
//     * @param source The button icon.
//     * @return the icon to use.
//     */
//    public static @NotNull Icon createDisabledIcon(AbstractButton b, ImageIcon source) {
//        AquaButtonIcon specialIcon = getSpecialIcon(b);
//        if (specialIcon != null) {
//            return specialIcon;
//        }
//        return createDefaultDisabledIcon(source);
//    }
//
//    /**
//     * This method is called by AbstractButton via the LAF to obtain an icon to use when a button is disabled and
//     * selected.
//     * @param b The button.
//     * @param source The button selected icon.
//     * @return the icon to use.
//     */
//    public static @NotNull Icon createDisabledSelectedIcon(AbstractButton b, ImageIcon source) {
//        AquaButtonIcon specialIcon = getSpecialIcon(b);
//        if (specialIcon != null) {
//            return specialIcon;
//        }
//        return createDefaultDisabledIcon(source);
//    }
//
//    private static @NotNull ImageIcon createDefaultDisabledIcon(ImageIcon source) {
//        return new ImageIconUIResource(AquaImageFactory.getProcessedImage(source.getImage(), AquaImageFactory.LIGHTEN_FOR_DISABLED));
//    }

    public static void toggleButtonStateChanged(@NotNull AbstractButton b) {
        JToggleButton leftButton = SegmentedControlModel.getLeftAdjacentButton(b);
        if (leftButton != null) {
            leftButton.repaint();
        }
    }

    public static boolean isToolbar(@NotNull AbstractButton b) {
        return Boolean.TRUE.equals(b.getClientProperty(AquaButtonUI.CACHED_TOOLBAR_STATUS_PROPERTY));
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

    /**
     * Invalidate the cached special icon if the template icon status has changed.
     */
    public static void updateTemplateIconStatus(AbstractButton b) {
        Object o = b.getClientProperty(AquaButtonUI.SPECIAL_ICON_PROPERTY);
        if (o instanceof AquaButtonIcon) {
            AquaButtonIcon icon = (AquaButtonIcon) o;
            boolean isTemplate = determineTemplateIconStatus(b);
            if (icon.isTemplate() != isTemplate) {
                // Force a new icon to be created with the new status
                removeCachedIcons(b);
            }
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

        b.putClientProperty(AquaButtonUI.SPECIAL_ICON_PROPERTY, null);
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
            if (shouldUseDisabledIcon(state)) {
                return pc.appearance.isDark() ? AquaImageFactory.DARKEN_FOR_PRESSED : AquaImageFactory.LIGHTEN_FOR_DISABLED;
            }
            return null;
        }
    }

    private static boolean shouldUseDisabledIcon(@NotNull AquaUIPainter.State state)
    {
        return state == AquaUIPainter.State.DISABLED || state == AquaUIPainter.State.DISABLED_INACTIVE;
    }
}
