/*
 * Changes Copyright (c) 2015-2026 Alan Snyder.
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

import org.jetbrains.annotations.*;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.GenericButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

import static org.violetlib.aqua.AquaButtonSupport.isColorWell;
import static org.violetlib.aqua.OSXSystemProperties.OSVersion;
import static org.violetlib.aqua.OSXSystemProperties.macOS11;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.OFF;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.ON;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM;

/**
 * A border for a button. The border is painted by a native painter. The native painter defines the border insets,
 * which capture the visual size of the border.
 */
public abstract class AquaButtonBorder extends AquaBorder implements FocusRingOutlineProvider {

    public static final @NotNull String AQUA_EXCLUSIVE_BUTTON = "Aqua.Button.isExclusiveButton";

    public static final RecyclableSingleton<AquaPushButtonBorder> fPush
      = new RecyclableSingletonFromDefaultConstructor<>(AquaPushButtonBorder.class);
    static public AquaButtonBorder getPushButtonBorder() {
        return fPush.get();
    }

    private static final RecyclableSingleton<AquaToggleButtonBorder> fToggle
      = new RecyclableSingletonFromDefaultConstructor<>(AquaToggleButtonBorder.class);
    static public AquaButtonBorder getToggleButtonBorder() {
        return fToggle.get();
    }

    private static final RecyclableSingleton<AquaIconToggleButtonBorder> fIconToggle
      = new RecyclableSingletonFromDefaultConstructor<>(AquaIconToggleButtonBorder.class);

    static public AquaButtonBorder getIconToggleButtonBorder() {
        return fIconToggle.get();
    }

    static public AquaButtonBorder getToolBarPushButtonBorder() {
        return fPush.get();
    }

    static public AquaButtonBorder getToolBarToggleButtonBorder() {
        return fToggle.get();
    }

    public static final RecyclableSingleton<AquaDisclosureTriangleButtonBorder> fDisclosureTriangle
      = new RecyclableSingletonFromDefaultConstructor<>(AquaDisclosureTriangleButtonBorder.class);
    public static AquaButtonBorder getDisclosureTriangleButtonBorder() { return fDisclosureTriangle.get(); }

    public static final RecyclableSingleton<AquaDisclosureButtonBorder> fDisclosure
      = new RecyclableSingletonFromDefaultConstructor<>(AquaDisclosureButtonBorder.class);
    public static AquaButtonBorder getDisclosureButtonBorder() { return fDisclosure.get(); }

    protected static final Dimension regularToolbarSize = new Dimension(32, 32);
    protected static final Dimension smallToolbarSize = new Dimension(24, 24);

    protected final AquaButtonIcon.ImageOperatorSupplier imageOperatorSupplier = new MyImageOperatorSupplier();

    protected AquaButtonBorder() {
    }

    @Override
    protected final void paint(JComponent c, Graphics2D g, int x, int y, int width, int height) {
        // These borders generally paint as backgrounds, unlike normal borders that are painted on top the component.
    }

    public void paintButton(@NotNull Graphics2D g,
                            @NotNull AbstractButton b,
                            @Nullable Icon icon,
                            @NotNull Rectangle viewRect,
                            @NotNull PaintingContext pc) {
        GenericButtonConfiguration bg = getConfiguration(b, pc, viewRect.width, viewRect.height);
        if (bg != null) {
            // It is easier to paint the border of a split toolbar item when the compound layout is known
            boolean isSplit = isSplitToolbarItem(b, bg);
            if (!isSplit) {
                paintBackground(g, b, bg, viewRect, pc);
            }
            if (allowsContent()) {
                Dimension iconSize = getRequiredIconSizeFromConfiguration(bg, icon);
                if (iconSize == null && icon != null) {
                    iconSize = new Dimension(icon.getIconWidth(), icon.getIconHeight());
                }
                Color textColor = getForegroundColor(b, bg, pc, false);
                Insets insets = getButtonContentInsets(b);
                Color iconColor = null;
                AquaButtonSupport.paintIconAndText(g, pc, b, bg, painter, insets, icon,
                  textColor, iconColor, viewRect, iconSize, isSplit);
            }
        }
    }

    public void paintBackground(@NotNull Graphics2D g,
                                @NotNull AbstractButton b,
                                @NotNull Configuration bg,
                                @NotNull Rectangle viewRect,
                                @NotNull PaintingContext pc) {
        int x = viewRect.x;
        int y = viewRect.y;
        int width = viewRect.width;
        int height = viewRect.height;

        int version = AquaPainting.getVersion();
        if (bg.getWidget() == BUTTON_TOOLBAR_ITEM && version < 1600) {
            RoundRectangle2D shape = new RoundRectangle2D.Double(x, y, width - 1, height - 1, 8, 8);
            AquaButtonSupport.paintToolbarItemBackground(b, (ButtonConfiguration) bg, pc, g, shape);
            return;
        }

        AquaUtils.configure(painter, pc.appearance, b, width, height);
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
                gg.setColor(pc.appearance.isDark() ? new Color(255, 255, 255, 52) : new Color(0, 0, 0, 52));
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
                                             @NotNull PaintingContext pc,
                                             boolean isIcon) {
        State state = g.getState();
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        boolean isEnabled = b.getModel().isEnabled();
        boolean useNonexclusive = shouldUseNonexclusiveStyle(b, info);
        Color existingColor = b.getForeground();
        if (existingColor == null || existingColor instanceof UIResource || !isEnabled || useNonexclusive) {
            AquaUIPainter.ButtonState bs = getButtonState(b);
            // The foreground color of a default button does not change when pressed.
            // Starting with macOS 12, the foreground color of any button does not change.
            if (state == State.PRESSED_DEFAULT) {
                state = State.ACTIVE_DEFAULT;
            } else if (OSVersion >= 1200 && state == State.PRESSED) {
                state = State.ACTIVE;
            }

            // Special case for the text color of a split button, which may be different than the icon color
            // when the icon background is the accent color.
            if (isSplitToolbarItem(b, g) && !isIcon && state == State.ACTIVE && getButtonState(b) == ON) {
                bs = OFF;
            }

            return info.getForeground(state, bs, pc.appearance, useNonexclusive, isIcon);
        } else {
            return existingColor;
        }
    }

    protected boolean shouldUseNonexclusiveStyle(@NotNull AbstractButton b,
                                                 @NotNull AquaButtonExtendedTypes.WidgetInfo info) {
        // Some segmented buttons should use a special style when the button is selected and the button is not in a
        // button group. This corresponds to the "select any" option in AppKit.

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

    /**
     * Indicate whether the border for a toolbar item is drawn around the icon only, instead of around the entire
     * content (icon and label).
     */

    protected boolean isSplitToolbarItem(@NotNull AbstractButton b, @NotNull GenericButtonConfiguration g) {
        if (g.getWidget() == BUTTON_TOOLBAR_ITEM && b.getIcon() != null) {
            int version = AquaPainting.getVersion();
            return version >= 1500;
        }
        return false;
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
     * Return the margin adjustments to use for the specified button when the application has not set the button margin.
     * The margin adjustments are added to the default button margin. The button margin is the content insets.
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
        int m = getDefaultSideMargin(b);
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
            // The fixed side margin of 9 is too large for mini, small, and regular
            switch(sg.getSize()) {
                case REGULAR:
                    left = right = 7; break;
                case SMALL:
                    left = right = 5; break;
                case MINI:
                    left = right = 3; break;
            }
        }

        return new Insets(top, left, bottom, right);
    }

    protected int getDefaultSideMargin(@NotNull AbstractButton b) {
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        return info.getDefaultSideMargin();
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
    public @Nullable AquaButtonIcon createSpecialIcon(@NotNull AbstractButton b,
                                                      boolean isTemplate,
                                                      @NotNull PaintingContext pc) {
        return new AquaButtonIcon(b, isTemplate, imageOperatorSupplier);
    }

    private class MyImageOperatorSupplier implements AquaButtonIcon.ImageOperatorSupplier {
        @Override
        public @Nullable Object getCurrentImageProcessingOperator(@NotNull AbstractButton b,
                                                                  boolean isTemplate,
                                                                  @NotNull PaintingContext pc) {
            GenericButtonConfiguration g = getConfiguration(b, pc, b.getWidth(), b.getHeight());
            if (isTemplate && g != null) {
                return getForegroundColor(b, g, pc, true);
            }
            State state = g != null ? g.getState() : getState(b);
            if (state == State.PRESSED && shouldHighlightPressedIcon(g)) {
                return pc.appearance.isDark() ? AquaImageFactory.LIGHTEN_FOR_DISABLED : AquaImageFactory.DARKEN_FOR_PRESSED;
            }
            if (shouldUseDisabledIcon(g, state)) {
                return pc.appearance.isDark() ? AquaImageFactory.DARKEN_FOR_PRESSED : AquaImageFactory.LIGHTEN_FOR_DISABLED;
            }
            return null;
        }
    }

    protected boolean shouldHighlightPressedIcon(@Nullable GenericButtonConfiguration g)
    {
        return true;
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
                return OSVersion < 1015;
            }
        }
        return false;
    }

    public @NotNull Dimension getPreferredButtonSize(@NotNull AbstractButton b) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        Dimension d = getMinimumButtonSize(g);
        Dimension iconSize = null;
        if (g != null) {
            iconSize = getRequiredIconSize(g, b.getIcon());
        }
        Dimension p = AquaButtonSupport.getBasicPreferredButtonSize(b, iconSize, g, painter);
        if (p != null) {
            if (p.width > d.width) {
                d.width = p.width;
            }
            if (p.height > d.height) {
                d.height = p.height;
            }
        }
        Dimension max = getMaximumButtonSize(g);
        if (max != null) {
            if (max.width > 0 && d.width > max.width) {
                d.width = max.width;
            }
            if (max.height > 0 && d.height > max.height) {
                d.height = max.height;
            }
        }
        return d;
    }

    protected @NotNull Dimension getMinimumButtonSize(@Nullable LayoutConfiguration g) {
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

    protected @Nullable Dimension getMaximumButtonSize(@Nullable LayoutConfiguration g) {
        if (g != null) {
            LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
            int fixedWidth = (int) Math.ceil(layoutInfo.getFixedVisualWidth());
            int fixedHeight = (int) Math.ceil(layoutInfo.getFixedVisualHeight());
            if (fixedWidth > 0 || fixedHeight > 0) {
                return new Dimension(fixedWidth, fixedHeight);
            }
        }
        return null;
    }

    protected @Nullable Dimension getRequiredIconSizeFromConfiguration(@NotNull Configuration g, @Nullable Icon icon) {
        if (icon != null && g instanceof LayoutConfiguration) {
            return getRequiredIconSize((LayoutConfiguration) g, icon);
        }
        return null;
    }

    protected @Nullable Dimension getRequiredIconSize(@NotNull LayoutConfiguration g, @Nullable Icon icon) {
        if (icon == null) {
            return null;
        }

        if (g instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
            AquaUIPainter.ButtonWidget widget = bg.getButtonWidget();
            if (widget == BUTTON_TOOLBAR_ITEM) {
                int version = AquaPainting.getVersion();
                if (version >= 1600) {
                    return smallToolbarSize;
                }
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

        int maximumHeight = getButtonIconMaximumHeight(g);
        if (maximumHeight > 0) {
            int iconHeight = icon.getIconHeight();
            if (iconHeight > maximumHeight) {
                float scaleFactor = maximumHeight / (float) iconHeight;
                int width = (int) Math.round(Math.ceil(icon.getIconWidth() * scaleFactor));
                return new Dimension(width, maximumHeight);
            }
        }

        return null;
    }

    /**
     * Return the maximum supported icon height for the given configuration.
     * @return the maximum height, or zero if the icon height is unrestricted.
     */
    private int getButtonIconMaximumHeight(@NotNull LayoutConfiguration g)
    {
        AquaUILayoutInfo layoutInfo = painter.getLayoutInfo();
        LayoutInfo info = layoutInfo.getLayoutInfo(g);
        int h = (int) info.getFixedVisualHeight();
        if (h == 0) {
            return 0;
        }
        Insetter s = layoutInfo.getContentInsets(g);
        if (s != null) {
            Rectangle bounds = s.apply(100, h);
            return bounds.height;
        }
        return h;
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
        return OSVersion >= macOS11 && isIconOnly(b);
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
        if (bw == AquaUIPainter.ButtonWidget.BUTTON_TEXTURED_TOOLBAR) {
            return AquaUIPainter.ButtonWidget.BUTTON_TEXTURED_TOOLBAR_ICONS;
        }
        return bw;
    }

    /**
     * Return the configuration for painting the button. The configuration is based on the current state of the button.
     */
    public @Nullable GenericButtonConfiguration getConfiguration(@NotNull AbstractButton b,
                                                                 @NotNull PaintingContext pc,
                                                                 int width, int height) {

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
     * Return the layout configuration for the button. The layout configuration is determined when the button is
     * configured by the button UI. The button layout is presumed to be invalid at that time.
     */
    public @Nullable LayoutConfiguration getLayoutConfiguration(@NotNull AbstractButton b) {
        return (LayoutConfiguration) b.getClientProperty(AquaButtonUI.LAYOUT_CONFIGURATION_PROPERTY);
    }

    /**
     * Determine the layout configuration for the button. The layout configuration is determined when the button is
     * configured by the button UI. The button layout is presumed to be invalid at that time.
     */
    public @Nullable LayoutConfiguration determineLayoutConfiguration(@NotNull AbstractButton b) {
        ButtonStyleInfo si = getButtonStyleInfo(b);
        GenericButtonWidget widget = si.widget;
        if (widget instanceof AquaUIPainter.ButtonWidget) {
            AquaUIPainter.ButtonWidget bw = (AquaUIPainter.ButtonWidget) widget;
            AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(b);
            if (shouldUseIconicWidget(b)) {
                bw = getIconicWidget(bw);
            }
            return new ButtonLayoutConfiguration(bw, si.size, ld);
        }
        if (widget instanceof AquaUIPainter.SegmentedButtonWidget) {
            AquaUIPainter.SegmentedButtonWidget bw = (AquaUIPainter.SegmentedButtonWidget) widget;
            return new SegmentedButtonLayoutConfiguration(bw, si.size, AquaUIPainter.Position.ONLY);
        }
        return null;
    }

    public abstract @NotNull ButtonStyleInfo getButtonStyleInfo(@NotNull AbstractButton b);

    @Override
    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        LayoutConfiguration g = getLayoutConfigurationForOutline((AbstractButton) c);
        if (g != null) {
            int width = c.getWidth();
            int height = c.getHeight();
            AquaUtils.configure(painter, null, c, width, height);
            return painter.getOutline(g);
        } else {
            return null;  // should not happen
        }
    }

    /**
     * Determine if a proposed button widget is usable for a button based on the fixed height (if any) imposed by the
     * widget.
     */
    protected boolean isProposedButtonWidgetUsable(@NotNull AbstractButton b,
                                                   boolean isOnToolbar,
                                                   @NotNull Object widget) {

        // If the button has no text, then any widget is usable because icons are scaled as needed.
        String text = b.getText();
        if (text == null || text.isEmpty()) {
            return true;
        }

        LayoutConfiguration g;

        Size size = AquaUtils.getSize(b, isOnToolbar, widget);
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
          ? AquaButtonExtendedTypes.getFont(widget, size, AquaButtonSupport.getGenericDefaultFont(b))
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
