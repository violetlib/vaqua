/*
 * Copyright (c) 2018-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.text.JTextComponent;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static org.violetlib.aqua.OSXSystemProperties.OSVersion;

/**

 */

public class AquaColors {

    public static final String COMPONENT_COLORS_KEY = "Aqua.componentColors";

    private static final ContextualColor CELL_FOREGROUND_COLORS = createCellForegroundColors();

    public static final BasicContextualColors TEXT_COLORS = createBasicTextColors();

    public static final BasicContextualColors LABELLED_BUTTON_COLORS = createLabeledButtonColors();

    /** For the default style of buttons **/
    public static final BasicContextualColors PUSH_BUTTON_COLORS
            = createButtonColors("PushButton", "pushButtonText");

    /** For the textured style of buttons, not in a tool bar **/
    public static final BasicContextualColors TEXTURED_COLORS
            = createButtonColors("TexturedButton", "texturedText");

    /** For the textured style of buttons, in a tool bar **/
    public static final BasicContextualColors TEXTURED_TOOLBAR_COLORS
            = createButtonColors("TexturedToolbarButton", "texturedToolbarText");

    /** For the gradient style of buttons **/
    public static final BasicContextualColors GRADIENT_BUTTON_COLORS
            = createButtonColors("GradientButton", "gradientText");

    /** For the rounded rect style of buttons **/
    public static final BasicContextualColors ROUNDED_RECT_BUTTON_COLORS
            = createButtonColors("RoundedRectButton", "roundedRectText");

    /** For the bevel style of buttons **/
    public static final BasicContextualColors BEVEL_BUTTON_COLORS
            = createButtonColors("BevelButton", "bevelText");

    /** For the round style of buttons **/
    public static final BasicContextualColors ROUND_BUTTON_COLORS
            = createButtonColors("RoundButton", "roundText");

    /** For the inline style of buttons **/
    public static final BasicContextualColors INLINE_BUTTON_COLORS
            = createButtonColors("InlineButton", "inlineButtonText");

    /** For the recessed style of buttons **/
    public static final BasicContextualColors RECESSED_BUTTON_COLORS
            = createButtonColors("RecessedButton", "recessedText");

    /** For the toolbar item style of buttons **/
    public static final BasicContextualColors TOOLBAR_ITEM_COLORS
            = createButtonColors("ToolbarItem", "toolbarItemText");

    /** Colors for pop up and pull down buttons **/
    public static final BasicContextualColors POP_UP_DOWN_BUTTON_COLORS
            = createButtonColors("PopUpDown", "pushPopText");

    /** For the default style of segmented buttons, with select one behavior and not in a tool bar **/
    public static final BasicContextualColors SEGMENTED_BUTTON_COLORS
            = createButtonColors("SegmentedButton", "segmentedText");

    /** For the buttons in a tabbed pane **/
    public static final BasicContextualColors TAB_COLORS
            = createButtonColors("TabButton", "tabText");

    /** For the default style of segmented buttons, with select any behavior **/
    public static final BasicContextualColors SEGMENTED_NONEXCLUSIVE_BUTTON_COLORS
            = createButtonColors("NonexclusiveSegmentedButton", "nonexclusiveText");

    /** For the separated style of segmented buttons, with select one behavior **/
    public static final BasicContextualColors SEGMENTED_SEPARATED_BUTTON_COLORS
            = createButtonColors("SegmentedSeparatedButton", "segmentedSeparatedText");

    /** For the separated style of segmented buttons, with select any behavior **/
    public static final BasicContextualColors SEGMENTED_SEPARATED_NONEXCLUSIVE_BUTTON_COLORS
            = createButtonColors("NonexclusiveSegmentedSeparatedButton", "nonexclusiveSeparatedText");

    /** For the textured style of segmented buttons, with select one behavior and not in a tool bar **/
    public static final BasicContextualColors TEXTURED_SEGMENTED_BUTTON_COLORS
            = createButtonColors("TexturedSegmentedButton", "texturedSegmentedText");

    /** For the textured style of segmented buttons, with select one behavior and in a tool bar **/
    public static final BasicContextualColors TEXTURED_SEGMENTED_TOOLBAR_BUTTON_COLORS
            = createButtonColors("TexturedSegmentedToolbarButton", "texturedSegmentedToolbarText");

    /** For the textured style of segmented buttons, with select any behavior and not in a tool bar **/
    public static final BasicContextualColors TEXTURED_NONEXCLUSIVE_COLORS
            = createButtonColors("NonexclusiveTextured", "nonexclusiveTexturedText");

    /** For the textured style of segmented buttons, with select any behavior and in a tool bar **/
    public static final BasicContextualColors TEXTURED_TOOLBAR_NONEXCLUSIVE_COLORS
            = createButtonColors("NonexclusiveTexturedToolbar", "nonexclusiveTexturedToolbarText");

    /** For the gradient style of segmented buttons **/
    public static final BasicContextualColors GRADIENT_SEGMENTED_BUTTON_COLORS
            = createButtonColors("GradientSegmentedButton", "gradientSegmentedText");

    public static final BasicContextualColors CELL_TEXT_COLORS = createCellTextColors();
    public static final BasicContextualColors CONTROL_COLORS = createBasicControlColors();
    public static final BasicContextualColors CLEAR_CONTROL_COLORS = createBasicClearControlColors();
    public static final BasicContextualColors TOOL_TIP_COLORS = createToolTipColors();
    private static final BasicContextualColors MENU_COLORS = createMenuColors();
    private static final BasicContextualColors LEGACY_MENU_COLORS = createLegacyMenuColors();
    private static final BasicContextualColors LEGACY_COMBO_BOX_MENU_COLORS = createLegacyComboBoxMenuColors();
    public static final BasicContextualColors SEPARATOR_COLORS = createSeparatorColors();
    public static final BasicContextualColors TABLE_HEADER_COLORS = createTableHeaderColors();
    public static final ContainerContextualColors CONTAINER_COLORS = createContainerColors();
    public static final ContainerContextualColors STRIPED_CONTAINER_COLORS = createStripedContainerColors();
    public static final ContainerContextualColors SIDEBAR_CONTAINER_COLORS = createSidebarContainerColors();

    public static final Color CLEAR = new ColorUIResource(new Color(0, 0, 0, 0));

    /**
     * This specific color object may have a special interpretation, to be replaced by the magic eraser effect.
     */
    public static final Color MAGIC_ERASER = new ColorUIResource(new Color(0, 0, 0, 0));

    private static boolean COLORS_DEBUG = false;
    private static @Nullable BasicContextualColors COLORS_DEBUG_CHOICE = TEXTURED_SEGMENTED_BUTTON_COLORS;
    private static boolean currentColorsDebugFlag;

    public static void setupDebugging(@Nullable BasicContextualColors colors) {
        currentColorsDebugFlag = COLORS_DEBUG && (COLORS_DEBUG_CHOICE == null || COLORS_DEBUG_CHOICE == colors);
    }

    public static void clearDebugging() {
        currentColorsDebugFlag = false;
    }

    public static boolean isDebugging() {
        return currentColorsDebugFlag;
    }

    public static @NotNull String toString(@Nullable Color c) {
        if (c == null) {
            return "<null>";
        }
        String s = c.getClass().getName() + "[" + c.getRed() + "," + c.getGreen() + "," + c.getBlue();
        int alpha = c.getAlpha();
        if (alpha != 255) {
            s = s + "," + alpha;
        }
        s = s + "]";
        return s;
    }

    public static @NotNull BasicContextualColors getMenuColors() {
        return OSVersion < 1014 ? LEGACY_MENU_COLORS : MENU_COLORS;
    }

    public static @NotNull BasicContextualColors getComboBoxMenuColors() {
        return OSVersion < 1014 ? LEGACY_COMBO_BOX_MENU_COLORS : MENU_COLORS;
    }

    public static @NotNull Color getForeground(@NotNull JComponent c, @NotNull String colorName) {
        Color color = c.getForeground();
        return getDefaultColor(c, color, colorName);
    }

    public static @NotNull Color getForeground(@NotNull JComponent c, @NotNull String colorName, @NotNull EffectName effect) {
        Color color = c.getForeground();
        return getDefaultColor(c, color, colorName, effect);
    }

    public static @NotNull Color getBackground(@NotNull Component c, @NotNull String colorName) {
        Color color = c.getBackground();
        return getDefaultColor(c, color, colorName);
    }

    public static @NotNull Color getBackground(@NotNull Component c, @NotNull String colorName, @NotNull EffectName effect) {
        Color color = c.getBackground();
        return getDefaultColor(c, color, colorName, effect);
    }

    /**
     * Identify a color that should take priority over a component default color.
     * A color is considered a priority if it is explicitly specified by an application or by a
     * client component or UI.
     * @param c The color (optional).
     * @return true if and only if {@code c} is not null and should take priority over a default color.
     */

    public static boolean isPriority(@Nullable Color c) {
        if (c == null) {
            return false;
        }
        if (c instanceof ColorUIResource) {
            return false;
        }
        if (c instanceof SystemColor) {
            return false;
        }
        return true;
    }

    /**
     * Identify the system color to use for a component if the application has not specified a color.
     * @param c The component.
     * @param color The color obtained from the component.
     * @param colorName The name of system color.
     * @return {@code color} if it is application specified or from a renderer or there is no system color with the
     *  specified name; otherwise, the system color with the specified name according to the effective appearance
     *  of {@code c}.
     */
    public static @NotNull Color getDefaultColor(@NotNull Component c, @NotNull Color color, @NotNull String colorName) {
        if (isPriority(color)) {
            return color;
        }

        AquaAppearance appearance = AppearanceManager.ensureAppearance(c);
        Color appearanceColor = appearance.getColor(colorName);
        if (appearanceColor != null) {
            return appearanceColor;
        }

        return color;
    }

    /**
     * Identify the system color to use for a component if the application has not specified a color.
     * @param c The component.
     * @param color The color obtained from the component.
     * @param colorName The name of system color.
     * @return {@code color} if it is application specified or from a renderer or there is no system color with the
     *  specified name; otherwise, the system color with the specified name according to the effective appearance
     *  of {@code c}.
     */
    public static @NotNull Color getDefaultColor(@NotNull Component c,
                                                 @NotNull Color color,
                                                 @NotNull String colorName,
                                                 @NotNull EffectName effect) {
        if (isPriority(color)) {
            return color;
        }

        AquaAppearance appearance = AppearanceManager.ensureAppearance(c);
        Color appearanceColor = appearance.getColorForOptionalEffect(colorName, effect);
        if (appearanceColor != null) {
            return appearanceColor;
        }

        return color;
    }

    /**
     * Identify a system color based on the effective appearance of a component.
     * @param c The component.
     * @param colorName The name of system color.
     * @return The system color with the specified name in the effective appearance of {@code c}.
     * @throws UnsupportedOperationException if the specified color is not defined.
     */
    public static @NotNull Color getSystemColor(@NotNull JComponent c, @NotNull String colorName) {
        AquaAppearance appearance = AppearanceManager.ensureAppearance(c);
        Color appearanceColor = appearance.getColor(colorName);
        if (appearanceColor != null) {
            return appearanceColor;
        }
        throw new UnsupportedOperationException("System color " + colorName + " is not defined in " + appearance.getName());
    }

    /**
     * Identify a system color based on the effective appearance of a component.
     * @param c The component.
     * @param colorName The name of system color.
     * @return The system color with the specified name in the effective appearance of {@code c}.
     * @throws UnsupportedOperationException if the specified color is not defined.
     */
    public static @NotNull Color getSystemColor(@NotNull JComponent c,
                                                @NotNull String colorName,
                                                @NotNull EffectName effect) {
        AquaAppearance appearance = AppearanceManager.ensureAppearance(c);
        Color appearanceColor = appearance.getColorForOptionalEffect(colorName, effect);
        if (appearanceColor != null) {
            return appearanceColor;
        }
        throw new UnsupportedOperationException("System color " + colorName + " is not defined in " + appearance.getName());
    }

    private static @NotNull BasicContextualColors createBasicTextColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("BasicTextColors.background", "textBackground");
        background.setAllNames();

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("BasicTextColors.foreground", "text");
        foreground.setAllNames();
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull ContextualColor createCellForegroundColors() {
        AquaContextualColorImpl foreground = new AquaContextualColorImpl("CellTextColors.foreground", "cell");
        foreground.setAllNames();
        return foreground;
    }

    private static @NotNull BasicContextualColors createCellTextColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("CellTextColors.background", "cellBackground");
        background.setAllNames();
        return new BasicContextualColorsImpl(background, CELL_FOREGROUND_COLORS);
    }

    private static @NotNull BasicContextualColors
    createButtonColors(@NotNull String name, @NotNull String basicColorName) {

        String backgroundName = name + ".background";
        AquaContextualColorImpl background = new AquaContextualColorImpl(backgroundName, "clear");
        background.setSelectedName("selectedTextBackground");
        background.setInactiveSelectedName("selectedTextBackground_inactive");

        String foregroundName = name + ".foreground";
        AquaContextualColorImpl foreground = new AquaContextualColorImpl(foregroundName, basicColorName);
        foreground.setAllNames();
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColors createBasicControlColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("BasicControlColors.background", "controlBackground");
        background.setSelectedName("controlAccent");  // probably never used
        background.setInactiveSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("BasicControlColors.foreground", "controlText");
        foreground.setSelectedName("alternateSelectedControlText");
        foreground.setInactiveSelectedName("unemphasizedSelectedControlText");
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColors createLabeledButtonColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("LabeledButtonColors.background", "clear");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("LabeledButtonColors.foreground", "controlText");
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColors createBasicClearControlColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("BasicClearControlColors.background", "clear");
        background.setSelectedName("selectedContentBackground");  // used by combo box editors

        background.setInactiveSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("BasicClearControlColors.foreground", "controlText");
        foreground.setSelectedName("alternateSelectedControlText");
        foreground.setInactiveSelectedName("unemphasizedSelectedControlText");
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColors createToolTipColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("ToolTipColors.background", "controlBackground");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("ToolTipColors.foreground", "controlText");
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColors createMenuColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("MenuColors.background", "clear");
        background.setSelectedName("selectedContentBackground");
        background.setInactiveSelectedName("selectedContentBackground_disabled");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("MenuColors.foreground", "controlText");
        foreground.setSelectedName("selectedMenuItemText");

        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColors createLegacyMenuColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("LegacyMenuColors.background", "menuBackground");
        background.setSelectedName("menuSelectedBackground");
        background.setInactiveSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("LegacyMenuColors.foreground", "menuForeground");
        foreground.setSelectedName("selectedMenuItemText");
        foreground.setInactiveSelectedName("unemphasizedSelectedControlText");

        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColors createLegacyComboBoxMenuColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("LegacyComboBoxMenuColors.background", "comboBoxMenuBackground");
        background.setSelectedName("menuSelectedBackground");
        background.setInactiveSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("LegacyComboBoxMenuColors.foreground", "menuForeground");
        foreground.setSelectedName("selectedMenuItemText");
        foreground.setInactiveSelectedName("unemphasizedSelectedControlText");

        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColorsImpl createSeparatorColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("SeparatorColors.background", "clear");
        AquaContextualColorImpl foreground = new AquaContextualColorImpl("SeparatorColors.foreground", "separator");
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull BasicContextualColorsImpl createTableHeaderColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("TableHeaderColors.background", "tableHeaderBackground");
        AquaContextualColorImpl foreground = new AquaContextualColorImpl("TableHeaderColors.foreground", "tableHeader");
        foreground.setSelectedName("selectedTableHeader");
        return new BasicContextualColorsImpl(background, foreground);
    }

    private static @NotNull ContainerContextualColors createContainerColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("ContainerColors.background", "controlBackground");
        background.setActiveDefaultSelectedName("selectedContentBackground");
        background.setSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl grid = new AquaContextualColorImpl("ContainerColors.grid", "separator");

        return new UniformContainerContextualColors(background, CELL_FOREGROUND_COLORS, grid);
    }

    private static @NotNull ContainerContextualColors createSidebarContainerColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("SidebarContainerColors.background", "controlBackground");
        background.setActiveDefaultSelectedName("selectedContentBackground");
        background.setSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl foreground = new AquaContextualColorImpl("SidebarContainerColors.foreground", "controlText");
        if (OSVersion < 1014) {
            foreground.setInactiveName("controlText");
        }

        AquaContextualColorImpl grid = new AquaContextualColorImpl("SidebarContainerColors.grid", "separator");

        return new UniformContainerContextualColors(background, foreground, grid);
    }

    private static @NotNull ContainerContextualColors createStripedContainerColors() {
        AquaContextualColorImpl background = new AquaContextualColorImpl("StripedContainerColors.background", "controlBackground");
        background.setActiveDefaultSelectedName("selectedContentBackground");
        background.setSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl evenRowBackground;
        AquaContextualColorImpl oddRowBackground;

        /* Before macOS 11 striped tables row are grey first; since macOS 11 striped tables rows are white first */
        if (OSXSystemProperties.OSVersion >= 1016) {
            evenRowBackground = new AquaContextualColorImpl("StripedContainerColors.background 0", "alternatingContentBackground_0");
            oddRowBackground = new AquaContextualColorImpl("StripedContainerColors.background 1", "alternatingContentBackground_1");
        } else {
            evenRowBackground = new AquaContextualColorImpl("StripedContainerColors.background 1", "alternatingContentBackground_1");
            oddRowBackground = new AquaContextualColorImpl("StripedContainerColors.background 0", "alternatingContentBackground_0");
        }

        evenRowBackground.setActiveDefaultSelectedName("selectedContentBackground");
        evenRowBackground.setSelectedName("unemphasizedSelectedTextBackground");
        oddRowBackground.setActiveDefaultSelectedName("selectedContentBackground");
        oddRowBackground.setSelectedName("unemphasizedSelectedTextBackground");

        AquaContextualColorImpl grid = new AquaContextualColorImpl("StripedContainerColors.grid", "separator");

        return new StripedContainerContextualColors(background, evenRowBackground, oddRowBackground, CELL_FOREGROUND_COLORS, grid);
    }

    @SuppressWarnings("unchecked")
    public static @NotNull <T extends BasicContextualColors> T getColors(@NotNull Class<T> clazz, @NotNull Component c, @NotNull T defaultColors) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            Object o = jc.getClientProperty(COMPONENT_COLORS_KEY);
            if (o != null && clazz.isAssignableFrom(o.getClass())) {
                return (T) o;
            }
        }
        return defaultColors;
    }

    public static void installColors(@NotNull JComponent c,
                                     @NotNull AppearanceContext context,
                                     @NotNull BasicContextualColors colors) {
        if (c instanceof JTextComponent) {
            JTextComponent tc = (JTextComponent) c;
            installTextColors(tc, context, colors);
        } else if (c instanceof JTable) {
            JTable table = (JTable) c;
            boolean isStriped = getTableStriped(table);
            boolean isInset = getTableInset(table);
            installTableColors(table, context, colors, isStriped, isInset);
        } else if (c instanceof JList) {
            JList list = (JList) c;
            boolean isStriped = getListStriped(list);
            boolean isInset = getListInset(list);
            installListColors(list, context, colors, isStriped, isInset);
        } else if (c instanceof JTree) {
            JTree tree = (JTree) c;
            boolean isStriped = getTreeStriped(tree);
            boolean isInset = getTreeInset(tree);
            installTreeColors(tree, context, colors, isStriped, isInset);
        } else {
            installBasicColors(c, context, colors);
        }
    }

    private static boolean getListStriped(@NotNull JList list) {
        AquaListUI ui = AquaUtils.getUI(list, AquaListUI.class);
        return ui != null && ui.isStriped();
    }

    private static boolean getTableStriped(@NotNull JTable table) {
        AquaTableUI ui = AquaUtils.getUI(table, AquaTableUI.class);
        return ui != null && ui.isStriped();
    }

    private static boolean getTreeStriped(@NotNull JTree tree) {
        AquaTreeUI ui = AquaUtils.getUI(tree, AquaTreeUI.class);
        return ui != null && ui.isStriped();
    }

    private static boolean getListInset(@NotNull JList list) {
        AquaListUI ui = AquaUtils.getUI(list, AquaListUI.class);
        return ui != null && ui.isInset();
    }

    private static boolean getTableInset(@NotNull JTable table) {
        AquaTableUI ui = AquaUtils.getUI(table, AquaTableUI.class);
        return ui != null && ui.isInset();
    }

    private static boolean getTreeInset(@NotNull JTree tree) {
        AquaTreeUI ui = AquaUtils.getUI(tree, AquaTreeUI.class);
        return ui != null && ui.isInset();
    }

    private static void installBasicColors(@NotNull Component c,
                                           @NotNull AppearanceContext context,
                                           @NotNull BasicContextualColors colors) {
        Color bg = c.getBackground();
        if (!isPriority(bg)) {
            c.setBackground(colors.getBackground(context));
        }

        Color fg = c.getForeground();
        if (!isPriority(fg)) {
            c.setForeground(colors.getForeground(context));
        }
    }

    private static void installTextColors(@NotNull JTextComponent editor,
                                          @NotNull AppearanceContext context,
                                          @NotNull BasicContextualColors colors) {

        AppearanceContext selectedContext = context.withSelected(true);
        AppearanceContext disabledContext = context.withState(AquaUIPainter.State.DISABLED);

        Color bg = editor.getBackground();
        if (!isPriority(bg)) {
            editor.setBackground(colors.getBackground(context));
        }

        Color fg = editor.getForeground();
        if (!isPriority(fg)) {
            editor.setForeground(colors.getForeground(context));
        }

        Color cc = editor.getCaretColor();
        if (!isPriority(cc)) {
            editor.setCaretColor(colors.getForeground(context));
        }

        Color s = editor.getSelectionColor();
        if (!isPriority(s)) {
            editor.setSelectionColor(colors.getBackground(selectedContext));
        }

        Color sfg = editor.getSelectedTextColor();
        if (!isPriority(sfg)) {
            editor.setSelectedTextColor(colors.getForeground(selectedContext));
        }

        Color dfg = editor.getDisabledTextColor();
        if (!isPriority(dfg)) {
            editor.setDisabledTextColor(colors.getForeground(disabledContext));
        }
    }

    private static void installTableColors(@NotNull JTable c,
                                           @NotNull AppearanceContext context,
                                           @NotNull BasicContextualColors colors,
                                           boolean isStriped,
                                           boolean isInset) {

        // A striped table must have a clear background, so that a well-behaved table cell renderer will paint a clear
        // background and preserve the stripes. The reason is that table cell renderers are supposed to obtain their
        // colors from the table. There is no generic interface that the table UI can use to configure a renderer.

        if (isStriped) {
            c.setBackground(AquaColors.CLEAR);
        } else if (!AquaColors.isPriority(c.getBackground())) {
            c.setBackground(colors.getBackground(context));
        }

        if (!AquaColors.isPriority(c.getForeground())) {
            c.setForeground(colors.getForeground(context));
        }

        if (colors instanceof ContainerContextualColors) {
            ContainerContextualColors cc = (ContainerContextualColors) colors;
            if (!AquaColors.isPriority(c.getGridColor())) {
                c.setGridColor(cc.getGrid(context));
            }
        }

        AppearanceContext selectedContext = context.withSelected(true);

        if (isInset && c.getRowSelectionAllowed() && !c.getColumnSelectionAllowed()) {
            c.setSelectionBackground(AquaColors.CLEAR);
        } else if (!AquaColors.isPriority(c.getSelectionBackground())) {
            c.setSelectionBackground(colors.getBackground(selectedContext));
        }

        if (!AquaColors.isPriority(c.getSelectionForeground())) {
            c.setSelectionForeground(colors.getForeground(selectedContext));
        }
    }

    private static void installListColors(@NotNull JList c,
                                          @NotNull AppearanceContext context,
                                          @NotNull BasicContextualColors colors,
                                          boolean isStriped,
                                          boolean isInset) {

        // A striped list must have a clear background, so that a well-behaved list cell renderer will paint a clear
        // background and preserve the stripes. The reason is that list cell renderers are supposed to obtain their
        // colors from the list. There is no generic interface that the list UI can use to configure a renderer.

        if (isStriped) {
            c.setBackground(AquaColors.CLEAR);
        } else if (!AquaColors.isPriority(c.getBackground())) {
            c.setBackground(colors.getBackground(context));
        }

        if (!AquaColors.isPriority(c.getForeground())) {
            c.setForeground(colors.getForeground(context));
        }

        AppearanceContext selectedContext = context.withSelected(true);

        if (isInset) {
            c.setSelectionBackground(AquaColors.CLEAR);
        } else if (!AquaColors.isPriority(c.getSelectionBackground())) {
            c.setSelectionBackground(colors.getBackground(selectedContext));
        }

        if (!AquaColors.isPriority(c.getSelectionForeground())) {
            c.setSelectionForeground(colors.getForeground(selectedContext));
        }
    }

    private static void installTreeColors(@NotNull JTree c,
                                          @NotNull AppearanceContext context,
                                          @NotNull BasicContextualColors colors,
                                          boolean isStriped,
                                          boolean isInset) {

        // A striped tree must have a clear background, so that a well-behaved tree cell renderer will paint a clear
        // background and preserve the stripes. The reason is that tree cell renderers are supposed to obtain their
        // colors from the tree. There is no generic interface that the tree UI can use to configure a renderer.

        if (isStriped) {
            c.setBackground(AquaColors.CLEAR);
        } else if (!AquaColors.isPriority(c.getBackground())) {
            c.setBackground(colors.getBackground(context));
        }

        if (!AquaColors.isPriority(c.getForeground())) {
            c.setForeground(colors.getForeground(context));
        }

        Color existingSelectionForeground = getColorProperty(c, AquaTreeUI.SELECTION_FOREGROUND_KEY);
        if (!AquaColors.isPriority(existingSelectionForeground)) {
            AppearanceContext selectedContext = context.withSelected(true);
            c.putClientProperty(AquaTreeUI.SELECTION_FOREGROUND_KEY, colors.getForeground(selectedContext));
        }
    }

    private static @Nullable Color getColorProperty(@NotNull JComponent c, @NotNull String property)
    {
        Object o = c.getClientProperty(property);
        return o instanceof Color ? (Color) o : null;
    }

    public static @NotNull Color getOrdinaryColor(@NotNull Color c) {
        if (c.getClass() == Color.class) {
            return c;
        }
        return new Color(c.getRed(), c.getGreen(), c.getBlue(), c.getAlpha());
    }

    public static @Nullable Color getCellEditorBackground(@NotNull JComponent c) {
        return new Color(0, 0, 0, 150);
    }

    public static @NotNull String createSelectedColorName(@NotNull String name) {
        return "selected" + AquaUtils.capitalize(name);
    }
}
