/*
 * Changes Copyright (c) 2015-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2013, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import javax.swing.*;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.GenericButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

import static org.violetlib.aqua.AquaButtonUI.getDefaultFontPropertyValue;
import static org.violetlib.aqua.AquaButtonUI.isColorWell;
import static org.violetlib.aqua.OSXSystemProperties.OSVersion;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.OFF;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.ON;

/**
 * A border for a button. The border is painted by a native painter. The native painter defines the border insets,
 * which capture the visual size of the border.
 */
public abstract class AquaButtonBorder extends AquaBorder implements FocusRingOutlineProvider {

    public static final @NotNull String AQUA_EXCLUSIVE_BUTTON = "Aqua.Button.isExclusiveButton";

    public static final RecyclableSingleton<AquaPushButtonBorder> fPush = new RecyclableSingletonFromDefaultConstructor<>(AquaPushButtonBorder.class);
    static public AquaButtonBorder getPushButtonBorder() {
        return fPush.get();
    }

    private static final RecyclableSingleton<AquaToggleButtonBorder> fToggle = new RecyclableSingletonFromDefaultConstructor<>(AquaToggleButtonBorder.class);
    static public AquaButtonBorder getToggleButtonBorder() {
        return fToggle.get();
    }

    private static final RecyclableSingleton<AquaIconToggleButtonBorder> fIconToggle = new RecyclableSingletonFromDefaultConstructor<>(AquaIconToggleButtonBorder.class);
    static public AquaButtonBorder getIconToggleButtonBorder() {
        return fIconToggle.get();
    }

    static public AquaButtonBorder getToolBarPushButtonBorder() {
        return fPush.get();
    }

    static public AquaButtonBorder getToolBarToggleButtonBorder() {
        return fToggle.get();
    }

    public static final RecyclableSingleton<AquaDisclosureTriangleButtonBorder> fDisclosureTriangle = new RecyclableSingletonFromDefaultConstructor<>(AquaDisclosureTriangleButtonBorder.class);
    public static AquaButtonBorder getDisclosureTriangleButtonBorder() { return fDisclosureTriangle.get(); }

    public static final RecyclableSingleton<AquaDisclosureButtonBorder> fDisclosure = new RecyclableSingletonFromDefaultConstructor<>(AquaDisclosureButtonBorder.class);
    public static AquaButtonBorder getDisclosureButtonBorder() { return fDisclosure.get(); }

    protected static final Dimension regularToolbarSize = new Dimension(32, 32);
    protected static final Dimension smallToolbarSize = new Dimension(24, 24);

    protected final AquaButtonIcon.ImageOperatorSupplier keySupplier = new MyImageOperatorSupplier();

    protected AquaButtonBorder() {
    }

    public final void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        // These borders generally paint as backgrounds, unlike normal borders that are painted on top the component.
    }

    public void paintButton(@NotNull Graphics2D g,
                            @NotNull AbstractButton b,
                            @Nullable Icon icon,
                            @NotNull Rectangle viewRect) {
        GenericButtonConfiguration bg = getConfiguration(b, viewRect.width, viewRect.height);
        if (bg != null) {
            paintBackground(g, b, bg, viewRect);
            if (allowsContent()) {
                Dimension iconSize = getIconSize(bg);
                Color textColor = getForegroundColor(b, bg, false);
                Insets insets = getButtonContentInsets(b);
                AquaButtonUI.paintIconAndText(g, b, insets, icon, textColor, viewRect, iconSize);
            }
        }
    }

    public void paintBackground(@NotNull Graphics2D g,
                                @NotNull AbstractButton b,
                                @NotNull Configuration bg,
                                @NotNull Rectangle viewRect) {
        int x = viewRect.x;
        int y = viewRect.y;
        int width = viewRect.width;
        int height = viewRect.height;

        AppearanceManager.ensureAppearance(b);
        AquaUtils.configure(painter, b, width, height);
        org.violetlib.jnr.Painter p = painter.getPainter(bg);
        p.paint(g, x, y);

        // The following code is obsolete, as JNR now does the full rendering of a color well button.
        // It remains here just in case VAqua is used with an older version of JNR.

        if (isColorWell(b) && !"ColorWellButtonConfiguration".equals(bg.getClass().getSimpleName())) {

            Graphics2D gg = (Graphics2D) g.create();
            Shape rr;

            float t = 6;
            float s = 6;
            float x1 = x + s;
            float ww = width - 2 * s;
            float x2 = x1 + ww;
            float y1 = y + t;
            float hh = height - 2 * t;
            float y2 = y1 + hh;

            if (OSVersion >= 1300) {
                rr = new RoundRectangle2D.Float(x1, y1, ww, hh, 4, 4);
            } else {
                rr = new Rectangle2D.Float(x1, y1, ww, hh);
            }

            Color c = b.getBackground();
            if (c.getAlpha() != 255) {
                // Color is translucent. Paint a black and white background.
                gg.setColor(Color.BLACK);
                AquaUtils.fillAntiAliased(gg, rr);

                Path2D.Float path = new Path2D.Float();
                path.moveTo(x1, y2);
                path.lineTo(x2, y1);
                path.lineTo(x2, y2);
                path.closePath();

                gg.clip(rr);
                gg.setColor(Color.WHITE);
                AquaUtils.fillAntiAliased(gg, path);
                gg.setClip(null);
            }

            gg.setColor(c);
            AquaUtils.fillAntiAliased(gg, rr);

            if (OSVersion >= 1300 && c.getAlpha() != 255) {
                AquaAppearance a = AppearanceManager.ensureAppearance(b);
                gg.setColor(a.isDark() ? new Color(255, 255, 255, 52) : new Color(0, 0, 0, 52));
                AquaUtils.drawAntiAliased(gg, rr);
            }

            gg.dispose();
        }
    }

    protected @NotNull State getState(@NotNull Configuration g) {
        if (g instanceof ButtonConfiguration) {
            ButtonConfiguration bg = (ButtonConfiguration) g;
            return bg.getState();
        }

        if (g instanceof SegmentedButtonConfiguration) {
            SegmentedButtonConfiguration bg = (SegmentedButtonConfiguration) g;
            return bg.getState();
        }

        // should not happen
        return State.ACTIVE;
    }

    protected @NotNull State getState(@NotNull AbstractButton b) {
        boolean isActive = AquaFocusHandler.isActive(b);

        if (!b.isEnabled()) {
            return isActive ? State.DISABLED : State.DISABLED_INACTIVE;
        }

        if (!isActive) {
            return State.INACTIVE;
        }

        ButtonModel model = b.getModel();
        if (model.isArmed() && model.isPressed()) {
            return AquaButtonSupport.getPressedState(b);
        }

        if (b.isRolloverEnabled() && isRollover(b)) {
            return State.ROLLOVER;
        }

        return AquaButtonSupport.getActiveState(b);
    }

    protected boolean isRollover(@NotNull AbstractButton b)
    {
        ButtonModel model = b.getModel();
        return model.isRollover();
    }

    public boolean allowsContent() {
        return true;
    }

    public boolean isRolloverEnabled(AbstractButton b) {
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        return info.isRolloverEnabled();
    }

    /**
     * Optionally substitute a custom default font for a button with this border.
     * @param b The button component.
     * @param size The size variant of the button.
     * @param df The standard default font for this button.
     */
    public @NotNull Font getCustomDefaultFont(@NotNull AbstractButton b, @NotNull Size size, @NotNull Font df) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
            AquaUIPainter.ButtonWidget widget = bg.getButtonWidget();
            return AquaButtonExtendedTypes.getFont(widget, size, df);
        } else if (g instanceof SegmentedButtonLayoutConfiguration) {
            SegmentedButtonLayoutConfiguration bg = (SegmentedButtonLayoutConfiguration) g;
            AquaUIPainter.SegmentedButtonWidget widget = bg.getWidget();
            return AquaButtonExtendedTypes.getFont(widget, size, df);
        } else {
            return df;
        }
    }

    public @NotNull Color getForegroundColor(@NotNull AbstractButton b,
                                             @NotNull GenericButtonConfiguration g,
                                             boolean isIcon) {
        State state = g.getState();
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        boolean isEnabled = b.getModel().isEnabled();
        boolean useNonexclusive = shouldUseNonexclusiveStyle(b, info);
        Color existingColor = b.getForeground();
        if (existingColor == null || existingColor instanceof UIResource || !isEnabled || useNonexclusive) {
            AquaUIPainter.ButtonState bs = getButtonState(b);
            AquaAppearance appearance = AppearanceManager.ensureAppearance(b);
            // The foreground color of a default button does not change.
            // Starting with macOS 12, the foreground color of any button does not change.
            if (state == State.PRESSED_DEFAULT) {
                state = State.ACTIVE_DEFAULT;
            } else if (OSVersion >= 1200 && state == State.PRESSED) {
                state = State.ACTIVE;
            }
            return info.getForeground(state, bs, appearance, useNonexclusive, isIcon);
        } else {
            return existingColor;
        }
    }

    protected boolean shouldUseNonexclusiveStyle(@NotNull AbstractButton b,
                                                 @NotNull AquaButtonExtendedTypes.WidgetInfo info) {
        // Some segmented buttons should use a special style when selected and they are not in a button
        // group. This corresponds to the "select any" option in AppKit.

        // These buttons are textured or separated.

        return info.usesNonexclusiveSelectionStyle()
                && b.getModel().isSelected()
                && !isButtonExclusive(b);
    }

    protected boolean isButtonExclusive(@NotNull AbstractButton b) {
        ButtonModel m = b.getModel();
        if (m instanceof DefaultButtonModel) {
            DefaultButtonModel dm = (DefaultButtonModel) m;
            return dm.getGroup() != null || Boolean.TRUE.equals(b.getClientProperty(AQUA_EXCLUSIVE_BUTTON));
        }
        return true;
    }

    public int getIconTextGap(AbstractButton b) {
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        int gap = info.getIconTextGap();
        return gap > 0 ? gap : 4;
    }

    /**
     * Returns the insets of the border.
     * @param c the component for which this border insets value applies
     */
    public final Insets getBorderInsets(Component c) {
        if (!(c instanceof AbstractButton)) {
            return new Insets(0, 0, 0, 0);
        }

        AbstractButton b = (AbstractButton) c;
        Insetter s = getContentInsets(b);
        Insets adjustments = getMarginAdjustments(b);
        return AquaUtils.combineAsInsets(s, adjustments);
    }

    /**
     * Returns the insets of the border.
     * @param c the component for which this border insets value applies
     */
    public final Insets2D getBorderInsets2D(Component c) {
        if (!(c instanceof AbstractButton)) {
            return new Insets2D(0, 0, 0, 0);
        }

        AbstractButton b = (AbstractButton) c;
        Insetter s = getContentInsets(b);
        Insets adjustments = getMarginAdjustments(b);
        return AquaUtils.combineAsInsets2D(s, adjustments);
    }

    /**
     * Return the margin adjustments to use for the specified button.
     * @param b The button.
     * @return the margin adjustments, or null if none.
     */
    protected @Nullable Insets getMarginAdjustments(@NotNull AbstractButton b) {
        Insets margin = b.getMargin();
        if (margin != null && !(margin instanceof UIResource)) {
            // always use an application provided margin
            return margin;
        } else {
            // Check for a style specific margin
            // Use the UI default if there is no type specific margin
            Insets specialInsets = getSpecialMarginAdjustments(b);
            return specialInsets != null ? specialInsets : margin;
        }
    }

    protected @Nullable Insets getSpecialMarginAdjustments(@NotNull AbstractButton b) {
        int m = getMargin(b);
        int top = 0;
        int left = m;
        int bottom = 0;
        int right = m;

        // TBD: at present, there is no organized way to support size specific adjustments

        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g instanceof SegmentedButtonLayoutConfiguration) {
            SegmentedButtonLayoutConfiguration sg = (SegmentedButtonLayoutConfiguration) g;
            AquaUIPainter.SegmentedButtonWidget w = sg.getWidget();
            if (w == AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SMALL_SQUARE
                    || w == AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED
                    || w == AquaUIPainter.SegmentedButtonWidget.BUTTON_TAB) {
                if (sg.getSize() == Size.MINI) {
                    top = 1;
                }
            }
        }

        return new Insets(top, left, bottom, right);
    }

    protected int getMargin(@NotNull AbstractButton b) {
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        return info.getMargin();
    }

    protected @Nullable Insetter getContentInsets(@NotNull AbstractButton b) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        return g != null ? painter.getLayoutInfo().getContentInsets(g) : null;
    }

    public @NotNull Insets getButtonContentInsets(@NotNull AbstractButton b) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g != null) {
            Insetter s = painter.getLayoutInfo().getContentInsets(g);
            if (s != null) {
                Insets insets = s.asInsets();
                if (insets != null) {
                    return insets;
                }
            }
        }
        return b.getInsets();
    }

    /**
     * Create a special icon to use for a button. The icon rendering may be context dependent.
     */
    public @Nullable AquaButtonIcon createIcon(@NotNull AbstractButton b, boolean isTemplate) {
        return new AquaButtonIcon(b, isTemplate, keySupplier);
    }

    private class MyImageOperatorSupplier implements AquaButtonIcon.ImageOperatorSupplier {
        @Override
        public @Nullable Object getCurrentImageProcessingOperator(@NotNull AbstractButton b, boolean isTemplate) {
            GenericButtonConfiguration g = getConfiguration(b, b.getWidth(), b.getHeight());
            if (isTemplate && g != null) {
                return getForegroundColor(b, g, true);
            }
            State state = g != null ? g.getState() : getState(b);
            if (state == State.PRESSED) {
                AquaAppearance appearance = AppearanceManager.ensureAppearance(b);
                return appearance.isDark() ? AquaImageFactory.LIGHTEN_FOR_DISABLED : AquaImageFactory.DARKEN_FOR_PRESSED;
            }
            if (shouldUseDisabledIcon(g, state)) {
                AquaAppearance appearance = AppearanceManager.ensureAppearance(b);
                return appearance.isDark() ? AquaImageFactory.DARKEN_FOR_PRESSED : AquaImageFactory.LIGHTEN_FOR_DISABLED;
            }
            return null;
        }
    }

    protected boolean shouldUseDisabledIcon(@Nullable GenericButtonConfiguration g, @NotNull State state)
    {
        if (g != null) {
            return shouldUseDisabledIcon(g);
        }
        return state == State.DISABLED || state == State.DISABLED_INACTIVE;
    }

    protected boolean shouldUseDisabledIcon(@NotNull GenericButtonConfiguration g)
    {
        State st = g.getState();
        if (st == State.DISABLED || st == State.DISABLED_INACTIVE) {
            return true;

        } else if (st == State.INACTIVE) {
            if (g.isTextured()) {
                return OSXSystemProperties.OSVersion < 1015;
            }
        }
        return false;
    }

    public @NotNull Dimension getPreferredButtonSize(@NotNull AbstractButton b) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        Dimension d = getMinimumButtonSize(g);
        Dimension iconSize = null;
        if (g != null) {
            iconSize = getIconSize(g);
        }
        Dimension p = AquaButtonSupport.getPreferredButtonSize(b, b.getIconTextGap(), iconSize);
        if (p != null) {
            if (p.width > d.width) {
                d.width = p.width;
            }
            if (p.height > d.height) {
                d.height = p.height;
            }
        }
        return d;
    }

    private @NotNull Dimension getMinimumButtonSize(@Nullable LayoutConfiguration g) {
        Dimension d = new Dimension(10, 10);
        if (g != null) {
            LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
            int minimumWidth = (int) Math.ceil(layoutInfo.getMinimumVisualWidth());
            int minimumHeight = (int) Math.ceil(layoutInfo.getMinimumVisualHeight());
            if (minimumWidth > 0) {
                d.width = minimumWidth;
            }
            if (minimumHeight > 0) {
                d.height = minimumHeight;
            }
        }
        return d;
    }

    protected @Nullable Dimension getIconSize(@NotNull Configuration g) {
        if (g instanceof LayoutConfiguration) {
            return getIconSize((LayoutConfiguration) g);
        }
        return null;
    }

    protected @Nullable Dimension getIconSize(@NotNull LayoutConfiguration g) {
        if (g instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
            AquaUIPainter.ButtonWidget widget = bg.getButtonWidget();
            if (widget == AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM) {
                Size size = bg.getSize();
                switch (size) {
                    case SMALL:
                    case MINI:
                        return smallToolbarSize;
                    default:
                        return regularToolbarSize;
                }
            }
        }
        return null;
    }

    protected @NotNull AquaButtonExtendedTypes.WidgetInfo getWidgetInfo(@NotNull AbstractButton b) {
        Object widget = null;

        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g instanceof ButtonLayoutConfiguration) {
            widget = ((ButtonLayoutConfiguration) g).getButtonWidget();
        }

        if (g instanceof SegmentedButtonLayoutConfiguration) {
            widget = ((SegmentedButtonLayoutConfiguration) g).getWidget();
        }

        return AquaButtonExtendedTypes.getWidgetInfo(widget);
    }

    protected boolean shouldUseIconicWidget(@NotNull AbstractButton b) {
        return OSVersion >= 1016 && isIconOnly(b);
    }

    public static boolean isIconOnly(@NotNull AbstractButton b)
    {
        Icon ic = b.getIcon();
        if (ic == null) {
            return false;
        }
        String text = b.getText();
        if (text != null && !text.isEmpty()) {
            return false;
        }
        return true;
    }

    protected @NotNull AquaUIPainter.ButtonWidget getIconicWidget(@NotNull AquaUIPainter.ButtonWidget bw) {
        if (bw == AquaUIPainter.ButtonWidget.BUTTON_TEXTURED_TOOLBAR && VAquaRenderingAccess.BUTTON_TEXTURED_TOOLBAR_ICONS != null) {
            return VAquaRenderingAccess.BUTTON_TEXTURED_TOOLBAR_ICONS;
        }
        return bw;
    }

    /**
     * Return the configuration for painting the button. The configuration is based on the current state of the button.
     */
    public @Nullable GenericButtonConfiguration getConfiguration(@NotNull AbstractButton b, int width, int height) {

        LayoutConfiguration g = getLayoutConfiguration(b);

        if (g instanceof ButtonLayoutConfiguration) {
            AquaUIPainter.State state = getState(b);
            boolean isFocused = computeIsFocused(state, b);
            AquaUIPainter.ButtonState bs = getButtonState(b);
            ButtonConfiguration bg = new ButtonConfiguration((ButtonLayoutConfiguration) g, state, isFocused, bs);
            if (isColorWell(b)) {
                try {
                    bg = new ColorWellButtonConfiguration(bg, b.getBackground());
                } catch (Throwable ignore) {
                    // probably an old version of JNR
                }
            }
            return bg;
        }

        return null;
    }

    protected boolean computeIsFocused(@NotNull AquaUIPainter.State state, @NotNull AbstractButton b) {
        if (b.isFocusPainted() && b.hasFocus()) {
            return state != State.DISABLED
                    && state != State.INACTIVE
                    && state != State.DISABLED_INACTIVE;
        }
        return false;
    }

    /**
     * Return the layout configuration for the button to use for focus ring outline calculation.
     * A toggle button may have a different outline.
     * A toggle button is recognized by its button state, which is not part of the basic layout configuration.
     */
    public LayoutConfiguration getLayoutConfigurationForOutline(@NotNull AbstractButton b) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        if (b instanceof JToggleButton) {
            if (g instanceof ButtonLayoutConfiguration) {
                AquaUIPainter.State state = getState(b);
                boolean isFocused = computeIsFocused(state, b);
                AquaUIPainter.ButtonState bs = getButtonState(b);
                return new ButtonConfiguration((ButtonLayoutConfiguration) g, state, isFocused, bs);
            }
        }
        return g;
    }

    /**
     * Return the layout configuration for the button. They layout configuration is determined when the button is
     * configured by the button UI. The button layout is presumed to be invalid at that time.
     */
    public @Nullable LayoutConfiguration getLayoutConfiguration(@NotNull AbstractButton b) {
        return (LayoutConfiguration) b.getClientProperty(AquaButtonUI.LAYOUT_CONFIGURATION_PROPERTY);
    }

    /**
     * Determine the layout configuration for the button. They layout configuration is determined when the button is
     * configured by the button UI. The button layout is presumed to be invalid at that time.
     */
    public @Nullable LayoutConfiguration determineLayoutConfiguration(@NotNull AbstractButton b) {
        GenericButtonWidget widget = getButtonWidget(b);
        Size defaultSize = getSpecialDefaultSize(b);
        Size size = AquaUtilControlSize.getUserSizeFrom(b, defaultSize);
        if (widget instanceof AquaUIPainter.ButtonWidget) {
            AquaUIPainter.ButtonWidget bw = (AquaUIPainter.ButtonWidget) widget;
            AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(b);
            if (shouldUseIconicWidget(b)) {
                bw = getIconicWidget(bw);
            }
            return new ButtonLayoutConfiguration(bw, size, ld);
        }
        if (widget instanceof AquaUIPainter.SegmentedButtonWidget) {
            AquaUIPainter.SegmentedButtonWidget bw = (AquaUIPainter.SegmentedButtonWidget) widget;
            return new SegmentedButtonLayoutConfiguration(bw, size, AquaUIPainter.Position.ONLY);
        }
        return null;
    }

    public abstract @NotNull GenericButtonWidget getButtonWidget(@NotNull AbstractButton b);

    protected @Nullable Size getSpecialDefaultSize(@NotNull AbstractButton b) {
        if (OSVersion >= 1016) {
            boolean isToolbar = AquaUtils.isOnToolbar(b);
            return isToolbar ? AquaUIPainter.Size.LARGE : null;
        }
        return null;
    }

    @Override
    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        LayoutConfiguration g = getLayoutConfigurationForOutline((AbstractButton) c);
        if (g != null) {
            int width = c.getWidth();
            int height = c.getHeight();
            AppearanceManager.ensureAppearance(c);
            AquaUtils.configure(painter, c, width, height);
            return painter.getOutline(g);
        } else {
            return null;  // should not happen
        }
    }

    /**
     * Determine if a proposed button widget is usable for a button based on the fixed height (if any) imposed by the
     * widget.
     */
    protected boolean isProposedButtonWidgetUsable(@NotNull AbstractButton b, @NotNull Object widget) {
        LayoutConfiguration g;

        Size size = AquaUtilControlSize.getUserSizeFrom(b);

        if (widget instanceof AquaUIPainter.ButtonWidget) {
            AquaUIPainter.ButtonWidget w = (AquaUIPainter.ButtonWidget) widget;
            AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(b);
            g = new ButtonLayoutConfiguration(w, size, ld);
        } else if (widget instanceof AquaUIPainter.SegmentedButtonWidget) {
            AquaUIPainter.SegmentedButtonWidget w = (AquaUIPainter.SegmentedButtonWidget) widget;
            g = new SegmentedButtonLayoutConfiguration(w, size, AquaUIPainter.Position.MIDDLE);
        } else {
            return false;
        }

        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
        int fixedHeight = (int) Math.ceil(layoutInfo.getFixedVisualHeight());
        if (fixedHeight == 0) {
            return true;
        }

        Insetter insetter = painter.getLayoutInfo().getContentInsets(g);
        if (insetter == null || !insetter.isInvertible()) {
            return true;
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

        Dimension contentSize = AquaButtonSupport.getPreferredContentSize(b, font, b.getIconTextGap());
        Dimension requiredSize = insetter.expand(contentSize);
        return requiredSize.height <= fixedHeight;
    }

    /**
     * Return a configuration independent font for a button with no application defined font.
     */
    private static @NotNull Font getGenericDefaultFont(@NotNull AbstractButton b) {
        Font f = getDefaultFontPropertyValue(b);
        if (f == null) {
            // should not happen
            return new Font("Default", Font.PLAIN, 12);
        }
        return f;
    }

    protected @NotNull AquaUIPainter.ButtonState getButtonState(@NotNull AbstractButton b) {
        if (b instanceof JToggleButton && !(b instanceof JCheckBox) && !(b instanceof JRadioButton)) {
            return b.getModel().isSelected() ? ON : OFF;
        }

        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
            if (bg.getButtonWidget() == AquaUIPainter.ButtonWidget.BUTTON_COLOR_WELL) {
                return b.getModel().isSelected() ? ON : OFF;
            }
        }

        return AquaUIPainter.ButtonState.STATELESS;
    }
}
