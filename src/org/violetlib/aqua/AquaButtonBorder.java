/*
 * Changes Copyright (c) 2015-2016 Alan Snyder.
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
import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.aqua.fc.EmptyIcon;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.OFF;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState.ON;

/**
 * A border for a button. The border is painted by a native painter. The native painter defines the border insets,
 * which capture the visual size of the border.
 */
public abstract class AquaButtonBorder extends AquaBorder implements BackgroundPainter, FocusRingOutlineProvider {

    public static final RecyclableSingleton<AquaPushButtonBorder> fPush = new RecyclableSingletonFromDefaultConstructor<AquaPushButtonBorder>(AquaPushButtonBorder.class);
    static public AquaButtonBorder getPushButtonBorder() {
        return fPush.get();
    }

    private static final RecyclableSingleton<AquaToggleButtonBorder> fToggle = new RecyclableSingletonFromDefaultConstructor<AquaToggleButtonBorder>(AquaToggleButtonBorder.class);
    static public AquaButtonBorder getToggleButtonBorder() {
        return fToggle.get();
    }

    private static final RecyclableSingleton<AquaIconToggleButtonBorder> fIconToggle = new RecyclableSingletonFromDefaultConstructor<AquaIconToggleButtonBorder>(AquaIconToggleButtonBorder.class);
    static public AquaButtonBorder getIconToggleButtonBorder() {
        return fIconToggle.get();
    }

    static public AquaButtonBorder getToolBarPushButtonBorder() {
        return fPush.get();
    }

    static public AquaButtonBorder getToolBarToggleButtonBorder() {
        return fToggle.get();
    }

    public static final RecyclableSingleton<AquaDisclosureTriangleButtonBorder> fDisclosureTriangle = new RecyclableSingletonFromDefaultConstructor<AquaDisclosureTriangleButtonBorder>(AquaDisclosureTriangleButtonBorder.class);
    public static AquaButtonBorder getDisclosureTriangleButtonBorder() { return fDisclosureTriangle.get(); }

    public static final RecyclableSingleton<AquaDisclosureButtonBorder> fDisclosure = new RecyclableSingletonFromDefaultConstructor<AquaDisclosureButtonBorder>(AquaDisclosureButtonBorder.class);
    public static AquaButtonBorder getDisclosureButtonBorder() { return fDisclosure.get(); }

    protected static final Icon regularToolbarSizingIcon = new EmptyIcon(32, 32);
    protected static final Icon smallToolbarSizingIcon = new EmptyIcon(24, 24);

    protected AquaButtonBorder() {
    }

    public final void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width, final int height) {
        // These borders generally paint as backgrounds, unlike normal borders that are painted on top the component.
    }

    public final void paintBackground(final JComponent c, final Graphics g, final int x, final int y, final int width, final int height) {
        final AbstractButton b = (AbstractButton)c;
        doButtonPaint(b, g, x, y, width, height);
    }

    protected void doButtonPaint(final AbstractButton b, final Graphics g, final int x, final int y, final int width, final int height) {
        Configuration bg = getConfiguration(b, width, height);
        if (bg != null) {

            // Special case for color wells. The button background should show through the center.
            // Not all the way to the edge because the outer border is translucent when disabled.

            if (bg instanceof ButtonConfiguration) {
                ButtonConfiguration btg = (ButtonConfiguration) bg;
                if (btg.getButtonWidget() == AquaUIPainter.ButtonWidget.BUTTON_COLOR_WELL) {
                    Color c = b.getBackground();
                    g.setColor(c);
                    g.fillRect(x+4, y+4, width-8, height-8);
                }
            }

            painter.configure(width, height);
            org.violetlib.jnr.Painter p = painter.getPainter(bg);
            p.paint(g, x, y);
        }
    }

    protected State getState(final AbstractButton b) {
        if (!b.isEnabled()) {
            return AquaFocusHandler.isActive(b) ? State.DISABLED : State.DISABLED_INACTIVE;
        }

        // The default button shouldn't draw its color when the window is inactive.
        // Changed for <rdar://problem/3614421>: Aqua LAF Buttons are incorrectly drawn disabled
        // all we need to do is make sure we aren't the default button any more and that
        // we aren't active, but we still are enabled if the button is enabled.
        // if we set dimmed we would appear disabled despite being enabled and click through
        // works so this now matches the text drawing and most importantly the HIG

        if (!AquaFocusHandler.isActive(b)) {
            return State.INACTIVE;
        }

        ButtonModel model = b.getModel();
        if (model.isArmed() && model.isPressed()) {
            return State.PRESSED;
        }
        if (model.isRollover()) {
            return State.ROLLOVER;
        }
        if ((b instanceof JButton) && ((JButton)b).isDefaultButton()) {
            return State.ACTIVE_DEFAULT;
        }
        return State.ACTIVE;
    }

    public boolean allowsContent() {
        return true;
    }

    public boolean isRolloverEnabled(AbstractButton b) {
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        return info != null && info.isRolloverEnabled();
    }

    /**
     * Optionally substitute a custom default font for a button with this border.
     * @param b The button component.
     * @param size The size variant of the button.
     * @param df The standard default font for this button.
     */
    public Font getCustomDefaultFont(AbstractButton b, Size size, Font df) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
            AquaUIPainter.ButtonWidget widget = bg.getButtonWidget();
            return AquaButtonExtendedTypes.getFont(df, widget, size);
        } else if (g instanceof SegmentedButtonLayoutConfiguration) {
            SegmentedButtonLayoutConfiguration bg = (SegmentedButtonLayoutConfiguration) g;
            AquaUIPainter.SegmentedButtonWidget widget = bg.getWidget();
            return AquaButtonExtendedTypes.getFont(df, widget, size);
        } else {
            return df;
        }
    }

    public Color getTextColor(AbstractButton b, AquaButtonExtendedTypes.ColorDefaults colorDefaults) {
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        State state = getState(b);
        AquaUIPainter.ButtonState bs = getButtonState(b);
        return info.getForeground(state, bs, colorDefaults);
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
    public final Insets getBorderInsets(final Component c) {
        if (c == null || !(c instanceof AbstractButton)) {
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
    public final Insets2D getBorderInsets2D(final Component c) {
        if (c == null || !(c instanceof AbstractButton)) {
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
    protected Insets getMarginAdjustments(AbstractButton b) {
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

    protected Insets getSpecialMarginAdjustments(AbstractButton b) {
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

    protected int getMargin(AbstractButton b) {
        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);
        return info.getMargin();
    }

    protected Insetter getContentInsets(AbstractButton b) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        return g != null ? painter.getLayoutInfo().getContentInsets(g) : null;
    }

    public Insets getContentInsets(final AbstractButton b, final int w, final int h) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g != null) {
            Insetter s = painter.getLayoutInfo().getContentInsets(g);
            if (s != null) {
                return s.asInsets();
            }
        }
        return null;
    }

    public Dimension getMinimumButtonSize(AbstractButton b) {
        Dimension d = new Dimension(10, 10);
        LayoutConfiguration g = getLayoutConfiguration(b);
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

        Icon substituteIcon = getSizingIcon(b);
        Dimension preferred = AquaButtonUI.getPreferredButtonSize(b, b.getIconTextGap(), substituteIcon);
        if (preferred != null) {
            d.width = Math.max(d.width, preferred.width);
            d.height = Math.max(d.height, preferred.height);
        }

        return d;
    }

    public Dimension getPreferredButtonSize(AbstractButton b) {
        Dimension min = getMinimumButtonSize(b);

        Icon substituteIcon = getSizingIcon(b);

        Dimension d = AquaButtonUI.getPreferredButtonSize(b, b.getIconTextGap(), substituteIcon);
        if (d == null) {
            return null;
        }

        if (min != null) {
            if (min.width > d.width) {
                d.width = min.width;
            }
            if (min.height > d.height) {
                d.height = min.height;
            }
        }

        return d;
    }

    public Icon getSizingIcon(AbstractButton b) {
        LayoutConfiguration g = getLayoutConfiguration(b);
        if (g instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
            AquaUIPainter.ButtonWidget widget = bg.getButtonWidget();
            if (widget == AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM) {
                Size size = bg.getSize();
                switch (size) {
                    case SMALL:
                    case MINI:
                        return smallToolbarSizingIcon;
                    default:
                        return regularToolbarSizingIcon;
                }
            }
        }
        return null;
    }

    protected AquaButtonExtendedTypes.WidgetInfo getWidgetInfo(AbstractButton b) {
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

    /*
     * Return the configuration for painting the button. The configuration is based on the current state of the button.
     */
    public Configuration getConfiguration(AbstractButton b, int width, int height) {

        LayoutConfiguration g = getLayoutConfiguration(b);

        if (g instanceof ButtonLayoutConfiguration) {
            final AquaUIPainter.State state = getState(b);
            boolean isFocused = (state != AquaUIPainter.State.DISABLED && state != AquaUIPainter.State.INACTIVE) && b.isFocusPainted() && b.hasFocus();
            AquaUIPainter.ButtonState bs = getButtonState(b);
            return new ButtonConfiguration((ButtonLayoutConfiguration) g, state, isFocused, bs);
        }

        if (g instanceof SegmentedButtonLayoutConfiguration) {
            final AquaUIPainter.State state = getState(b);
            boolean isFocused = (state != AquaUIPainter.State.DISABLED && state != AquaUIPainter.State.INACTIVE) && b.isFocusPainted() && b.hasFocus();
            boolean isSelected = b.getModel().isSelected();
            AquaUIPainter.Direction d = AquaUIPainter.Direction.NONE;
            return new SegmentedButtonConfiguration((SegmentedButtonLayoutConfiguration) g, state, isSelected,
                            isFocused, d, SegmentedButtonConfiguration.DividerState.NONE, SegmentedButtonConfiguration.DividerState.NONE);
        }

        return null;
    }

    /*
     * Return the layout configuration for the button. They layout configuration is determined when the button is
     * configured by the button UI. The button layout is presumed to be invalid at that time.
     */
    public LayoutConfiguration getLayoutConfiguration(AbstractButton b) {
        return (LayoutConfiguration) b.getClientProperty(AquaButtonUI.LAYOUT_CONFIGURATION_PROPERTY);
    }

    /*
     * Determine the layout configuration for the button. They layout configuration is determined when the button is
     * configured by the button UI. The button layout is presumed to be invalid at that time.
     */
    public LayoutConfiguration determineLayoutConfiguration(AbstractButton b) {
        Object widget = getButtonWidget(b);
        Size size = AquaUtilControlSize.getUserSizeFrom(b);
        if (widget instanceof AquaUIPainter.ButtonWidget) {
            AquaUIPainter.ButtonWidget bw = (AquaUIPainter.ButtonWidget) widget;
            AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(b);
            return new ButtonLayoutConfiguration(bw, size, ld);
        }
        if (widget instanceof AquaUIPainter.SegmentedButtonWidget) {
            AquaUIPainter.SegmentedButtonWidget bw = (AquaUIPainter.SegmentedButtonWidget) widget;
            return new SegmentedButtonLayoutConfiguration(bw, size, AquaUIPainter.Position.ONLY);
        }
        return null;
    }

    public Object getButtonWidget(AbstractButton b) {
        return null;
    }

    @Override
    public Shape getFocusRingOutline(JComponent c) {
        LayoutConfiguration g = getLayoutConfiguration((AbstractButton) c);
        if (g != null) {
            int width = c.getWidth();
            int height = c.getHeight();
            painter.configure(width, height);
            return painter.getOutline(g);
        } else {
            return null;    // should not happen
        }
    }

    /**
     * Determine if a proposed button widget is usable for a button based on the fixed height (if any) imposed by the
     * widget.
     */
    protected boolean isProposedButtonWidgetUsable(AbstractButton b, Object widget) {
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
                ? AquaButtonExtendedTypes.getFont(AquaButtonUI.getGenericDefaultFont(b), widget, size)
                : b.getFont();
        Dimension contentSize = AquaButtonUI.getPreferredContentSize(b, font, b.getIconTextGap());
        Dimension requiredSize = insetter.expand(contentSize);
        return requiredSize.height <= fixedHeight;
    }

    protected AquaUIPainter.ButtonState getButtonState(AbstractButton b) {
        if (b instanceof JToggleButton) {
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
