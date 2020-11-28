/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.util.HashMap;
import java.util.Map;
import javax.swing.*;
import javax.swing.border.Border;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Position;
import org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget;

import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonWidget.*;
import static org.violetlib.jnr.aqua.AquaUIPainter.ComboBoxWidget.*;
import static org.violetlib.jnr.aqua.AquaUIPainter.PopupButtonWidget.*;
import static org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget.*;

/**
 * Map client properties to borders.
 */
public class AquaButtonExtendedTypes {

    /**
     * Identify the basic type of a button from its client properties and its UI.
     * @param b The button.
     * @param isToolbar True if and only if the button should be configured for use on a toolbar.
     * @return the button type name, or null if the button has not been configured with a recognized button type or
     * the button type is a segmented button type but the button has not been configured with a valid segment position.
     */
    public static @Nullable String getBasicButtonType(@NotNull AbstractButton b, boolean isToolbar) {

        Object buttonTypeProperty = b.getClientProperty(AquaButtonUI.BUTTON_TYPE);
        if (buttonTypeProperty == null) {
            if (b.getUI().getClass() == AquaButtonToggleUI.class || getValidSegmentPosition(b) != null) {
                buttonTypeProperty = "segmented";
            } else {
                return null;
            }
        }

        if (buttonTypeProperty instanceof String) {
            String buttonType = (String) buttonTypeProperty;
            if (buttonType.equals("segmented")) {
                if (isToolbar) {
                    buttonType = "segmentedTextured";
                }
            } else if (buttonType.equals("segmentedSeparated")) {
                if (isToolbar) {
                    buttonType = "segmentedTexturedSeparated";
                }
            } else if (buttonType.equals("round") && isToolbar) {
                buttonType = "roundTextured";
            }
            return buttonType;
        }

        return null;
    }

    /**
     * Identify the type of a button from its client properties, its UI, and its component orientation.
     * This method returns an extended type, which may include a segment position if the button is a segmented
     * button.
     * @param b The button.
     * @param isToolbar True if and only if the button should be configured for use on a toolbar.
     * @return the button type name, or null if the button has not been configured with a recognized button type.
     */
    public static @Nullable String getExtendedButtonType(@NotNull AbstractButton b, boolean isToolbar) {
        String basicType = getBasicButtonType(b, isToolbar);
        if (basicType != null) {
            String position = getValidSegmentPosition(b);
            if (position != null) {
                position = getRealPositionForLogicalPosition(position, b.getComponentOrientation().isLeftToRight());
                return basicType + "-" + position;
            }
        }
        return basicType;
    }

    /**
     * Identify the segment position of a button based on its client properties.
     * @param b The button.
     * @return the segment position, or null if the button has not been configured with a recognized segment position.
     */
    public static @Nullable String getValidSegmentPosition(@NotNull AbstractButton b) {
        Object segmentPositionProperty = b.getClientProperty(AquaButtonUI.SEGMENTED_BUTTON_POSITION);
        if (segmentPositionProperty instanceof String) {
            String s = (String) segmentPositionProperty;
            if (s.equals("first") || s.equals("last") || s.equals("middle") || s.equals("only")) {
                return s;
            }
        }
        return null;
    }

    /**
     * Identify a button type specifier based on the client properties of a button, its UI, and its component
     * orientation.
     * @param b The button component.
     * @return the button type specifier, or null if the button has not been configured with a recognized segment
     * position.
     */
    public static @Nullable TypeSpecifier getTypeSpecifier(@NotNull AbstractButton b, boolean isToolbar) {

        String buttonType = getExtendedButtonType(b, isToolbar);
        if (buttonType == null) {
            return null;
        }

        TypeSpecifier specifier = getSpecifierByName(b, buttonType, isToolbar);
        if (specifier != null) {
            return specifier;
        }

        if (buttonType.contains("-")) {
            String basicType = getBasicButtonType(b, isToolbar);
            assert basicType != null;
            return getSpecifierByName(b, basicType, isToolbar);
        }

        return null;
    }

    public static WidgetInfo getTabWidgetInfo(@NotNull SegmentedButtonWidget widget,
                                              AquaUIPainter.Size sz,
                                              Position pos) {
        return getWidgetDefinitions().get(widget);
    }

    protected static @NotNull String getRealPositionForLogicalPosition(@NotNull String logicalPosition,
                                                                       boolean leftToRight) {
        if (!leftToRight) {
            if ("first".equals(logicalPosition)) {
                return "last";
            }
            if ("last".equals(logicalPosition)) {
                return "first";
            }
        }
        return logicalPosition;
    }

    public static abstract class TypeSpecifier {
        final String name;

        protected TypeSpecifier(final String name) {
            this.name = name;
        }

        public abstract Border getBorder();
    }

    public static class FixedBorderTypeSpecifier extends TypeSpecifier {
        private final AquaButtonBorder border;

        public FixedBorderTypeSpecifier(String name, AquaButtonBorder border) {
            super(name);

            this.border = border;
        }

        public AquaButtonBorder getBorder() {
            return border;
        }
    }

    public static class BorderDefinedTypeSpecifier extends TypeSpecifier {
        private final ButtonWidget widget;
        private final WidgetInfo info;

        public BorderDefinedTypeSpecifier(String name, ButtonWidget widget) {
            super(name);

            this.widget = widget;
            this.info = getWidgetInfo(widget);
        }

        public AquaButtonBorder getBorder() {
            return new AquaNamedButtonBorder(widget, info);
        }
    }

    public static class SegmentedTypeSpecifier extends TypeSpecifier {
        private final @NotNull SegmentedButtonWidget widget;
        private final @NotNull WidgetInfo info;
        private final @NotNull Position position;

        /**
         * Create a specifier for a segmented button. The widget may be superseded in some cases based on the group
         * membership and the text/icon attributes of the button at the time the widget is used.
         */
        public SegmentedTypeSpecifier(@NotNull String name,
                                      @NotNull SegmentedButtonWidget widget,
                                      @NotNull Position position) {
            super(name);

            this.widget = widget;
            this.info = getWidgetInfo(widget);
            this.position = position;
        }

        public @NotNull AquaButtonBorder getBorder() {
            return new AquaSegmentedButtonBorder(widget, info, position);
        }
    }

    private static TypeSpecifier getSpecifierByName(AbstractButton b, String name, boolean isToolbar) {
        if (isToolbar) {
            String toolbarName = name + "-onToolbar";
            TypeSpecifier specifier = typeDefinitions.get().get(toolbarName);
            if (specifier != null) {
                return specifier;
            }
        }

        return typeDefinitions.get().get(name);
    }

    protected final static RecyclableSingleton<Map<String, TypeSpecifier>> typeDefinitions = new RecyclableSingleton<Map<String, TypeSpecifier>>() {
        protected Map<String, TypeSpecifier> getInstance() {
            return getAllTypes();
        }
    };

    /**
     * Return the font to use for a button based on a button widget.
     * This capability is needed for button types that determine the widget based on the button content size.
     * @param defaultFont The default font without taking the button widget into account.
     * @param widget The button widget.
     * @param size The size variant.
     * @return the font to use.
     */
    public static @NotNull Font getFont(@NotNull Object widget,
                                        @NotNull AquaUIPainter.Size size,
                                        @NotNull Font defaultFont) {
        WidgetInfo info = getWidgetInfo(widget);
        Font font = info.getFont(size);
        if (font != null) {
            return font;
        }
        if (size != AquaUIPainter.Size.REGULAR) {
            float fontSize = getFontSize(size);
            return defaultFont.deriveFont(fontSize);
        }
        return defaultFont;
    }

    public static float getFontSize(AquaUIPainter.Size size) {
        switch (size) {
            case SMALL:
                return 11;
            case MINI:
                return 9;
            case LARGE:
                return 13;
            default:
                return 13;
        }
    }

    protected final static WidgetInfo defaultButtonWidgetInfo = new WidgetInfo(AquaColors.CLEAR_CONTROL_COLORS);
    protected final static WidgetInfo defaultSegmentedButtonWidgetInfo = new WidgetInfo(AquaColors.CLEAR_CONTROL_COLORS).withSegmented();

    public static @NotNull WidgetInfo getWidgetInfo(Object widget) {
        WidgetInfo info = getWidgetDefinitions().get(widget);
        if (info != null) {
            return info;
        }
        if (widget instanceof SegmentedButtonWidget) {
            return defaultSegmentedButtonWidgetInfo;
        }
        if (widget instanceof ButtonWidget) {
            return defaultButtonWidgetInfo;
        }
        return defaultButtonWidgetInfo;
    }

    protected final static RecyclableSingleton<Map<Object, WidgetInfo>> widgetDefinitions = new RecyclableSingleton<Map<Object, WidgetInfo>>() {
        protected Map<Object, WidgetInfo> getInstance() {
            return getAllWidgets();
        }
    };

    private static @NotNull Map<Object, WidgetInfo> getWidgetDefinitions()
    {
        Map<Object, WidgetInfo> defs = widgetDefinitions.get();
        assert defs != null;
        return defs;
    }

    @FunctionalInterface
    protected interface FontFinder {
        @Nullable Font getFont(@NotNull AquaUIPainter.Size size);
    }

    public static class WidgetInfo implements Cloneable {
        private boolean isSegmented;
        private boolean isTextured;

        private @NotNull BasicContextualColors colors;
        private @Nullable BasicContextualColors nonExclusiveSelectionColors;

        private Font font;
        private FontFinder fontFinder;
        private boolean isRolloverEnabled;
        private int iconTextGap;
        private int margin;
        private int bottomMenuGap;

        WidgetInfo(@NotNull BasicContextualColors colors) {
            this.colors = colors;
        }

        @NotNull WidgetInfo copy() {
            try {
                return (WidgetInfo) clone();
            } catch (CloneNotSupportedException ex) {
                // should not happen
                throw new RuntimeException("Unable to clone WidgetInfo");
            }
        }

        @NotNull WidgetInfo withSegmented() {
            this.isSegmented = true;
            return this;
        }

        @NotNull WidgetInfo withTextured() {
            this.isTextured = true;
            return this;
        }

        @NotNull WidgetInfo withColors(@NotNull BasicContextualColors colors) {
            this.colors = colors;
            return this;
        }

        @NotNull WidgetInfo withFont(@NotNull Font f) {
            this.font = f;
            return this;
        }

        @NotNull WidgetInfo withFontFinder(@NotNull FontFinder ff) {
            this.fontFinder = ff;
            return this;
        }

        @NotNull WidgetInfo withIconTextGap(int gap) {
            this.iconTextGap = gap;
            return this;
        }

        @NotNull WidgetInfo withMargin(int margin) {
            this.margin = margin;
            return this;
        }

        @NotNull WidgetInfo withNonexclusiveSelectionColors(@NotNull BasicContextualColors cs) {
            this.nonExclusiveSelectionColors = cs;
            return this;
        }

        @NotNull WidgetInfo withRolloverEnabled() {
            this.isRolloverEnabled = true;
            return this;
        }

        @NotNull WidgetInfo withBottomMenuGap(int gap) {
            this.bottomMenuGap = gap;
            return this;
        }

        public int getIconTextGap() {
            return iconTextGap;
        }

        public int getMargin() {
            return margin;
        }

        public @Nullable Font getFont(@NotNull AquaUIPainter.Size size) {
            if (fontFinder != null) {
                return fontFinder.getFont(size);
            }

            if (font != null) {
                if (size != AquaUIPainter.Size.REGULAR) {
                    float fontSize = getFontSize(size);
                    return font.deriveFont(fontSize);
                }
            }

            return font;
        }

        public @NotNull BasicContextualColors getColors() {
            return colors;
        }

        public @NotNull Color getForeground(AquaUIPainter.State state,
                                            AquaUIPainter.ButtonState bs,
                                            AquaAppearance appearance,
                                            boolean useNonexclusiveStyle,
                                            boolean isIcon) {

            BasicContextualColors colors;

            // Special case for a segmented button that is not in a button group.
            if (useNonexclusiveStyle && nonExclusiveSelectionColors != null) {
                colors = nonExclusiveSelectionColors;
            } else {
                colors = this.colors;
            }

            boolean isSelected = bs == AquaUIPainter.ButtonState.ON;
            AppearanceContext context = new AppearanceContext(appearance, state, isSelected, isIcon);
            return colors.getForeground(context);
        }

        public boolean isSegmented() {
            return isSegmented;
        }

        public boolean isTextured() {
            return isTextured;
        }

        public boolean isRolloverEnabled() {
            return isRolloverEnabled;
        }

        public boolean usesNonexclusiveSelectionStyle() {
            return nonExclusiveSelectionColors != null && isSegmented;
        }

        public int getBottomMenuGap() {
            return bottomMenuGap;
        }
    }

    protected static Map<Object, WidgetInfo> getAllWidgets() {
        Map<Object, WidgetInfo> result = new HashMap<>();

        result.put(BUTTON_CHECK_BOX, new WidgetInfo(AquaColors.LABELLED_BUTTON_COLORS));
        result.put(BUTTON_RADIO, new WidgetInfo(AquaColors.LABELLED_BUTTON_COLORS));

        result.put(BUTTON_PUSH, new WidgetInfo(AquaColors.PUSH_BUTTON_COLORS)
                .withMargin(5)
        );

        WidgetInfo segmentedRounded = new WidgetInfo(AquaColors.SEGMENTED_BUTTON_COLORS)
                .withSegmented()
                .withNonexclusiveSelectionColors(AquaColors.SEGMENTED_NONEXCLUSIVE_BUTTON_COLORS)
                .withMargin(9)
                ;

        WidgetInfo segmentedSeparated = segmentedRounded.copy()
                .withNonexclusiveSelectionColors(AquaColors.SEGMENTED_SEPARATED_NONEXCLUSIVE_BUTTON_COLORS)
                .withColors(AquaColors.SEGMENTED_SEPARATED_BUTTON_COLORS);

        result.put(BUTTON_SEGMENTED, segmentedRounded);
        result.put(BUTTON_TAB, segmentedRounded);
        if (VAquaRenderingAccess.SLIDER_WIDGET != null) {
            result.put(VAquaRenderingAccess.SLIDER_WIDGET, segmentedRounded);
        }
        if (VAquaRenderingAccess.SLIDER_TOOLBAR_WIDGET != null) {
            result.put(VAquaRenderingAccess.SLIDER_TOOLBAR_WIDGET , segmentedRounded);
        }
        if (VAquaRenderingAccess.TEXTURED_SEPARATED_TOOLBAR_ICONS_WIDGET != null) {
            result.put(VAquaRenderingAccess.TEXTURED_SEPARATED_TOOLBAR_ICONS_WIDGET, segmentedRounded);
        }
        result.put(BUTTON_SEGMENTED_SEPARATED, segmentedSeparated);

        WidgetInfo gradient = new WidgetInfo(AquaColors.GRADIENT_BUTTON_COLORS);
        result.put(BUTTON_GRADIENT, gradient.copy().withMargin(2));
        result.put(BUTTON_BEVEL, gradient.copy().withMargin(4));  // this is the "Square" style in IB

        result.put(BUTTON_ROUNDED_RECT, new WidgetInfo(AquaColors.ROUNDED_RECT_BUTTON_COLORS)
                .withMargin(4)
                .withFontFinder((sz) -> UIManager.getFont("Button.font").deriveFont(fontSize(sz, 12, 12, 11, 9))));

        result.put(BUTTON_BEVEL_ROUND, new WidgetInfo(AquaColors.BEVEL_BUTTON_COLORS)
                .withMargin(6));

        // Round buttons are like gradient buttons, but white looks better on the blue background
        result.put(BUTTON_ROUND, new WidgetInfo(AquaColors.ROUND_BUTTON_COLORS));

        result.put(BUTTON_TOOLBAR_ITEM, new WidgetInfo(AquaColors.TOOLBAR_ITEM_COLORS)
                .withIconTextGap(2));

        result.put(BUTTON_INLINE, new WidgetInfo(AquaColors.INLINE_BUTTON_COLORS)
                .withFontFinder((sz) -> UIManager.getFont("Button.inline.font"))
        );

        result.put(BUTTON_RECESSED, new WidgetInfo(AquaColors.RECESSED_BUTTON_COLORS)
                .withRolloverEnabled()
                .withFontFinder((sz) -> {
                    Font f = UIManager.getFont("Button.recessed.font");
                    if (sz == AquaUIPainter.Size.MINI) {
                        f = f.deriveFont(Font.PLAIN);
                    }
                    float size = sz == AquaUIPainter.Size.MINI ? 9 : 12;
                    return f.deriveFont(size);
                })
        );

        // Textured toggle push buttons and segmented buttons are not identical, but they are getting closer and
        // probably should be identical.

        WidgetInfo textured = new WidgetInfo(AquaColors.TEXTURED_COLORS)
                .withTextured()
                .withNonexclusiveSelectionColors(AquaColors.TEXTURED_NONEXCLUSIVE_COLORS)
                ;
        WidgetInfo texturedToolbar = new WidgetInfo(AquaColors.TEXTURED_TOOLBAR_COLORS)
                .withTextured()
                .withNonexclusiveSelectionColors(AquaColors.TEXTURED_TOOLBAR_NONEXCLUSIVE_COLORS)
                ;

        if (OSXSystemProperties.OSVersion >= 1016) {
            texturedToolbar = texturedToolbar.withRolloverEnabled();
        }

        result.put(BUTTON_TEXTURED, textured);
        result.put(BUTTON_TEXTURED_TOOLBAR, texturedToolbar);
        result.put(BUTTON_ROUND_TEXTURED, textured);
        result.put(BUTTON_ROUND_TEXTURED_TOOLBAR, texturedToolbar);

        WidgetInfo segmentedTextured = textured.copy().withSegmented().withColors(AquaColors.TEXTURED_SEGMENTED_BUTTON_COLORS).withMargin(9);
        WidgetInfo segmentedTexturedToolbar = segmentedTextured.copy()
                .withColors(AquaColors.TEXTURED_SEGMENTED_TOOLBAR_BUTTON_COLORS)
                .withNonexclusiveSelectionColors(AquaColors.TEXTURED_TOOLBAR_NONEXCLUSIVE_COLORS);

        result.put(BUTTON_SEGMENTED_TEXTURED, segmentedTextured);
        result.put(BUTTON_SEGMENTED_TEXTURED_SEPARATED, segmentedTextured);
        result.put(BUTTON_SEGMENTED_TEXTURED_TOOLBAR, segmentedTexturedToolbar);
        if (VAquaRenderingAccess.TEXTURED_TOOLBAR_ICONS_WIDGET != null) {
            result.put(VAquaRenderingAccess.TEXTURED_TOOLBAR_ICONS_WIDGET, segmentedTexturedToolbar);
        }
        result.put(BUTTON_SEGMENTED_TEXTURED_SEPARATED_TOOLBAR, segmentedTexturedToolbar);
        if (VAquaRenderingAccess.TEXTURED_SEPARATED_TOOLBAR_ICONS_WIDGET != null) {
            result.put(VAquaRenderingAccess.TEXTURED_SEPARATED_TOOLBAR_ICONS_WIDGET, segmentedTexturedToolbar);
        }

        WidgetInfo segmentedGradient = gradient.copy().withSegmented()
                .withColors(AquaColors.GRADIENT_SEGMENTED_BUTTON_COLORS).withMargin(9);
        result.put(BUTTON_SEGMENTED_INSET, segmentedGradient);  // segmented rounded rect
        result.put(BUTTON_SEGMENTED_SCURVE, segmentedGradient);
        result.put(BUTTON_SEGMENTED_SMALL_SQUARE, segmentedGradient);

        WidgetInfo pushPopUp = new WidgetInfo(AquaColors.POP_UP_DOWN_BUTTON_COLORS)
                .withMargin(5)
                ;

        result.put(BUTTON_POP_DOWN, pushPopUp);
        result.put(BUTTON_POP_UP, pushPopUp);

        result.put(BUTTON_POP_DOWN_BEVEL, result.get(BUTTON_BEVEL));
        result.put(BUTTON_POP_UP_BEVEL, result.get(BUTTON_BEVEL));

        result.put(BUTTON_POP_DOWN_ROUND_RECT, result.get(BUTTON_ROUNDED_RECT));
        result.put(BUTTON_POP_UP_ROUND_RECT, result.get(BUTTON_ROUNDED_RECT));

        result.put(BUTTON_POP_DOWN_RECESSED, result.get(BUTTON_RECESSED));
        result.put(BUTTON_POP_UP_RECESSED, result.get(BUTTON_RECESSED));

        result.put(BUTTON_POP_DOWN_TEXTURED, result.get(BUTTON_TEXTURED));
        result.put(BUTTON_POP_UP_TEXTURED, result.get(BUTTON_TEXTURED));

        result.put(BUTTON_POP_DOWN_TEXTURED_TOOLBAR, result.get(BUTTON_TEXTURED_TOOLBAR));
        result.put(BUTTON_POP_UP_TEXTURED_TOOLBAR, result.get(BUTTON_TEXTURED_TOOLBAR));

        result.put(BUTTON_POP_DOWN_GRADIENT, result.get(BUTTON_GRADIENT));
        result.put(BUTTON_POP_UP_GRADIENT, result.get(BUTTON_GRADIENT));

        // combo box widget info currently used only for bottom menu gap
        {
            WidgetInfo comboBox = new WidgetInfo(AquaColors.CONTROL_COLORS);
            result.put(BUTTON_COMBO_BOX, comboBox.copy().withBottomMenuGap(5));
            result.put(BUTTON_COMBO_BOX_CELL, comboBox.copy().withBottomMenuGap(6));
            result.put(BUTTON_COMBO_BOX_TEXTURED, comboBox.copy().withBottomMenuGap(6));
            result.put(BUTTON_COMBO_BOX_TEXTURED_TOOLBAR, comboBox.copy().withBottomMenuGap(5));
        }

        return result;
    }

    protected static Map<String, TypeSpecifier> getAllTypes() {
        Map<String, TypeSpecifier> specifiersByName = new HashMap<String, TypeSpecifier>();

        TypeSpecifier[] specifiers = {
                new FixedBorderTypeSpecifier("toolbar", AquaButtonBorder.getToolBarToggleButtonBorder()),
                new FixedBorderTypeSpecifier("icon", AquaButtonBorder.getIconToggleButtonBorder()),
                new FixedBorderTypeSpecifier("text", (AquaButtonBorder) UIManager.getBorder("Button.border")),
                new FixedBorderTypeSpecifier("toggle", AquaButtonBorder.getToggleButtonBorder()),
                new FixedBorderTypeSpecifier("disclosureTriangle", AquaButtonBorder.getDisclosureTriangleButtonBorder()),
                new FixedBorderTypeSpecifier("disclosure", AquaButtonBorder.getDisclosureButtonBorder()),

                new BorderDefinedTypeSpecifier("checkbox", BUTTON_CHECK_BOX),
                new BorderDefinedTypeSpecifier("radio", BUTTON_RADIO),

                new BorderDefinedTypeSpecifier("square", BUTTON_BEVEL),
                new BorderDefinedTypeSpecifier("gradient", BUTTON_GRADIENT),
                new BorderDefinedTypeSpecifier("bevel", BUTTON_BEVEL_ROUND),

                new BorderDefinedTypeSpecifier("textured", BUTTON_TEXTURED),
                new BorderDefinedTypeSpecifier("textured-onToolbar", BUTTON_TEXTURED_TOOLBAR),
                new BorderDefinedTypeSpecifier("roundRect", BUTTON_ROUNDED_RECT),
                new BorderDefinedTypeSpecifier("recessed", BUTTON_RECESSED),
                new BorderDefinedTypeSpecifier("inline", BUTTON_INLINE),
                new BorderDefinedTypeSpecifier("well", BUTTON_TOOLBAR_ITEM),  // old name from Aqua LAF
                new BorderDefinedTypeSpecifier("toolbarItem", BUTTON_TOOLBAR_ITEM),
                new BorderDefinedTypeSpecifier("help", BUTTON_HELP),
                new BorderDefinedTypeSpecifier("round", BUTTON_ROUND),
                new BorderDefinedTypeSpecifier("round-onToolbar", OSXSystemProperties.OSVersion >= 1011 ? BUTTON_ROUND_TEXTURED_TOOLBAR : BUTTON_ROUND),
                new BorderDefinedTypeSpecifier("texturedRound", BUTTON_ROUND_INSET),  // TBD: this is not correct, but the button type is undocumented
                new BorderDefinedTypeSpecifier("roundTextured", BUTTON_ROUND_TEXTURED),
                new BorderDefinedTypeSpecifier("roundTextured-onToolbar", OSXSystemProperties.OSVersion >= 1011 ? BUTTON_ROUND_TEXTURED_TOOLBAR : BUTTON_ROUND_TEXTURED),
                new BorderDefinedTypeSpecifier("roundInset", BUTTON_ROUND_INSET),
                new BorderDefinedTypeSpecifier("colorWell", BUTTON_COLOR_WELL),

                new SegmentedTypeSpecifier("segmented-first", BUTTON_SEGMENTED, Position.FIRST),
                new SegmentedTypeSpecifier("segmented-middle", BUTTON_SEGMENTED, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmented-last", BUTTON_SEGMENTED, Position.LAST),
                new SegmentedTypeSpecifier("segmented-only", BUTTON_SEGMENTED, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedSeparated-first", BUTTON_SEGMENTED_SEPARATED, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedSeparated-middle", BUTTON_SEGMENTED_SEPARATED, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedSeparated-last", BUTTON_SEGMENTED_SEPARATED, Position.LAST),
                new SegmentedTypeSpecifier("segmentedSeparated-only", BUTTON_SEGMENTED_SEPARATED, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedRoundRect-first", BUTTON_SEGMENTED_INSET, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedRoundRect-middle", BUTTON_SEGMENTED_INSET, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedRoundRect-last", BUTTON_SEGMENTED_INSET, Position.LAST),
                new SegmentedTypeSpecifier("segmentedRoundRect-only", BUTTON_SEGMENTED_INSET, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedTexturedRounded-first", BUTTON_SEGMENTED_SCURVE, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedTexturedRounded-middle", BUTTON_SEGMENTED_SCURVE, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedTexturedRounded-last", BUTTON_SEGMENTED_SCURVE, Position.LAST),
                new SegmentedTypeSpecifier("segmentedTexturedRounded-only", BUTTON_SEGMENTED_SCURVE, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedTextured-first", BUTTON_SEGMENTED_TEXTURED, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedTextured-middle", BUTTON_SEGMENTED_TEXTURED, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedTextured-last", BUTTON_SEGMENTED_TEXTURED, Position.LAST),
                new SegmentedTypeSpecifier("segmentedTextured-only", BUTTON_SEGMENTED_TEXTURED, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedTextured-first-onToolbar", BUTTON_SEGMENTED_TEXTURED_TOOLBAR, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedTextured-middle-onToolbar", BUTTON_SEGMENTED_TEXTURED_TOOLBAR, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedTextured-last-onToolbar", BUTTON_SEGMENTED_TEXTURED_TOOLBAR, Position.LAST),
                new SegmentedTypeSpecifier("segmentedTextured-only-onToolbar", BUTTON_SEGMENTED_TEXTURED_TOOLBAR, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedCapsule-first", BUTTON_SEGMENTED_TOOLBAR, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedCapsule-middle", BUTTON_SEGMENTED_TOOLBAR, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedCapsule-last", BUTTON_SEGMENTED_TOOLBAR, Position.LAST),
                new SegmentedTypeSpecifier("segmentedCapsule-only", BUTTON_SEGMENTED_TOOLBAR, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedGradient-first", BUTTON_SEGMENTED_SMALL_SQUARE, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedGradient-middle", BUTTON_SEGMENTED_SMALL_SQUARE, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedGradient-last", BUTTON_SEGMENTED_SMALL_SQUARE, Position.LAST),
                new SegmentedTypeSpecifier("segmentedGradient-only", BUTTON_SEGMENTED_SMALL_SQUARE, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedTexturedSeparated-first", BUTTON_SEGMENTED_TEXTURED_SEPARATED, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedTexturedSeparated-middle", BUTTON_SEGMENTED_TEXTURED_SEPARATED, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedTexturedSeparated-last", BUTTON_SEGMENTED_TEXTURED_SEPARATED, Position.LAST),
                new SegmentedTypeSpecifier("segmentedTexturedSeparated-only", BUTTON_SEGMENTED_TEXTURED_SEPARATED, Position.ONLY),

                new SegmentedTypeSpecifier("segmentedTexturedSeparated-first-onToolbar", BUTTON_SEGMENTED_TEXTURED_SEPARATED_TOOLBAR, Position.FIRST),
                new SegmentedTypeSpecifier("segmentedTexturedSeparated-middle-onToolbar", BUTTON_SEGMENTED_TEXTURED_SEPARATED_TOOLBAR, Position.MIDDLE),
                new SegmentedTypeSpecifier("segmentedTexturedSeparated-last-onToolbar", BUTTON_SEGMENTED_TEXTURED_SEPARATED_TOOLBAR, Position.LAST),
                new SegmentedTypeSpecifier("segmentedTexturedSeparated-only-onToolbar", BUTTON_SEGMENTED_TEXTURED_SEPARATED_TOOLBAR, Position.ONLY),
        };

        for (TypeSpecifier specifier : specifiers) {
            specifiersByName.put(specifier.name, specifier);
        }

        return specifiersByName;
    }

    private static float fontSize(@NotNull AquaUIPainter.Size sz, float large, float regular, float small, float mini) {
        if (sz == AquaUIPainter.Size.LARGE) {
            return large;
        }
        if (sz == AquaUIPainter.Size.SMALL) {
            return small;
        }
        if (sz == AquaUIPainter.Size.MINI) {
            return mini;
        }
        return regular;
    }
}
