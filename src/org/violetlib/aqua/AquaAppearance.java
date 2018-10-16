/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.swing.plaf.ColorUIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaNativeRendering;
import org.violetlib.vappearances.VAppearance;

import static org.violetlib.aqua.OSXSystemProperties.OSVersion;

/**
 * An object representing a specific appearance, including the current accent and highlight colors.
 */

public class AquaAppearance implements VAppearance {
    private final @NotNull VAppearance appearance;
    private final @NotNull Map<String,Color> colors;

    private static final @NotNull Colors defaultColors = createDefaultColors();
    private static final @NotNull Colors lightColors = createLightColors();
    private static final @NotNull Colors darkColors = createDarkColors();
    private static final @NotNull Colors highContrastLightColors = createHighContrastLightColors();
    private static final @NotNull Colors highContrastDarkColors = createHighContrastDarkColors();

    private static final int NO_INACTIVE = 1 << 0;   // colors do not change when inactive

    public AquaAppearance(@NotNull VAppearance appearance) {
        this.appearance = appearance;

        Colors colors = new Colors();
        colors.add(defaultColors);
        colors.addAll(appearance.getColors());

        Map<String,Color> myNativeColors = AquaNativeRendering.createPainter().getColors(appearance);
        colors.addAll(myNativeColors);

        if (appearance.isDark()) {
            colors.add(darkColors);
            if (appearance.isHighContrast()) {
                colors.add(highContrastDarkColors);
            }
        } else {
            colors.add(lightColors);
            if (appearance.isHighContrast()) {
                colors.add(highContrastLightColors);
            }
        }
        installFixups(colors, appearance);
        this.colors = colors.getColors();
    }

    @Override
    public @NotNull String getName() {
        return appearance.getName();
    }

    @Override
    public boolean isDark() {
        return appearance.isDark();
    }

    @Override
    public boolean isHighContrast() {
        return appearance.isHighContrast();
    }

    @Override
    public @NotNull Map<String,Color> getColors() {
        return appearance.getColors();
    }

    /**
     * Return the color with the specified name.
     * @param colorName The color name.
     * @return the color, as a ColorUIResource, or null if the color name not defined in this appearance.
     */

    public @Nullable Color getColor(@NotNull String colorName) {
        Color color = colors.get(colorName);
        if (AquaColors.isDebugging()) {
            System.err.println("  Color " + colorName + ": " + AquaColors.toString(color));
        }
        return color;
    }

    /**
     * Return a color modified for a specified effect. If no color is defined for that effect, the basic color is
     * returned.
     * @param colorName The color name.
     * @param effectName The effect name.
     * @return the color as defined by the color name and effect, or the basic color if there is no variable color
     * defined for the specified effect, or null if the color name is note defined in this appearance.
     */

    public @Nullable Color getColorForEffect(@NotNull String colorName, @NotNull EffectName effectName) {
        if (effectName == EffectName.EFFECT_NONE) {
            return colors.get(colorName);
        }

        String extendedName = colorName + "_" + effectName;
        Color c = colors.get(extendedName);
        return c;
    }

    /**
     * Return a color modified for a specified effect. If no color is defined for that effect, the basic color is
     * returned.
     * @param colorName The color name.
     * @param effectName The effect name.
     * @return the color as defined by the color name and effect, or the basic color if there is no variable color
     * defined for the specified effect, or null if the color name is note defined in this appearance.
     */

    public @Nullable Color getColorForOptionalEffect(@NotNull String colorName, @NotNull EffectName effectName) {
        if (effectName == EffectName.EFFECT_NONE) {
            return colors.get(colorName);
        }

        String extendedName = colorName + "_" + effectName;
        Color c = colors.get(extendedName);
        return c != null ? c : colors.get(colorName);
    }

    public boolean isBasedOn(@NotNull VAppearance va) {
        return va == appearance;
    }

    private static @NotNull Colors createDefaultColors() {
        Colors colors = new Colors();
        colors.add("clear", 0, 0);

        // in 10.14, the text selection background is not displayed when the window is inactive
        if (OSVersion < 1014) {
            colors.add("selectedTextBackground_inactive", "unemphasizedSelectedTextBackground");
            colors.add("selectedCellBackground_inactive", "unemphasizedSelectedTextBackground");
        } else {
            colors.add("selectedTextBackground_inactive", "clear");
            colors.add("selectedCellBackground_inactive", "clear");
        }

        // default definitions

        colors.add("texturedWindowBackground", "windowBackground");
        colors.add("texturedWindowBackground_disabled", "windowBackground_disabled");

        colors.add("topWindowMarginBackground", "windowBackground");
        colors.add("topWindowMarginBackground_disabled", "windowBackground_disabled");
        colors.add("bottomWindowMarginBackground", "windowBackground");
        colors.add("bottomWindowMarginBackground_disabled", "windowBackground_disabled");

        colors.add("topTexturedWindowMarginBackground", "texturedWindowBackground");
        colors.add("topTexturedWindowMarginBackground_disabled", "texturedWindowBackground_disabled");
        colors.add("bottomTexturedWindowMarginBackground", "texturedWindowBackground");
        colors.add("bottomTexturedWindowMarginBackground_disabled", "texturedWindowBackground_disabled");

        colors.add("topWindowDivider", "separator");
        colors.add("topWindowDivider_disabled", "separator_disabled");
        colors.add("bottomWindowDivider", "separator");
        colors.add("bottomWindowDivider_disabled", "separator_disabled");

        colors.add("topTexturedWindowDivider", "separator");
        colors.add("topTexturedWindowDivider_disabled", "separator_disabled");
        colors.add("bottomTexturedWindowDivider", "separator");
        colors.add("bottomTexturedWindowDivider_disabled", "separator_disabled");

        colors.add("text_inactive", "text");
        colors.add("selectedText_inactive", "unemphasizedSelectedText");

        colors.add("cellBackground", "textBackground");
        colors.add("selectedCellBackground", "selectedTextBackground");

        colors.addAll("cell", "text");
        colors.add("selectedCell_focused", "alternateSelectedControlText");
        colors.add("selectedCell", "unemphasizedSelectedText");
        colors.add("selectedCell_inactive", "unemphasizedSelectedText");

        colors.addAll("texturedToolbarText", "texturedText");
        colors.addAll("tabText", "segmentedText");
        colors.addAll("nonexclusiveTexturedText", "texturedText");
        colors.addAll("nonexclusiveText", "segmentedText");
        colors.addAll("segmentedSeparatedText", "segmentedText");
        colors.addAll("nonexclusiveSeparatedText", "segmentedSeparatedText");
        colors.addAll("nonexclusiveTexturedToolbarText", "nonexclusiveTexturedText");
        colors.addAll("texturedSegmentedText", "segmentedText");
        colors.addAll("texturedSegmentedToolbarText", "texturedSegmentedText");
        colors.addAll("gradientSegmentedText", "gradientText");
        colors.addAll("pushPopText", "pushButtonText");

        // rounded rect push and toggle buttons do not change when inactive
        colors.addAll("roundedRectText", "gradientText", NO_INACTIVE);

        // bevel push and toggle buttons do not change when inactive
        colors.addAll("bevelText", "gradientText", NO_INACTIVE);

        // push buttons mostly do not change when inactive
        colors.defineNoInactive("pushButtonText");

        // default style segmented buttons mostly do not change when inactive
        // they do not have pressed behavior
        colors.addAll("segmentedText", "pushButtonText", NO_INACTIVE);
        colors.add("segmentedText_pressed", "segmentedText");
        colors.add("selectedSegmentedText_pressed", "selectedSegmentedText");

        // textured segmented buttons do not have pressed behavior (unlike push and toggle buttons)
        colors.add("texturedSegmentedText_pressed", "texturedSegmentedText");
        colors.add("selectedTexturedSegmentedText_pressed", "selectedTexturedSegmentedText");
        colors.add("nonexclusiveTexturedText_pressed", "nonexclusiveTexturedText");
        colors.add("selectedNonexclusiveTexturedText_pressed", "selectedNonexclusiveTexturedText");
        colors.add("texturedSegmentedToolbarText_pressed", "texturedSegmentedToolbarText");
        colors.add("selectedTexturedSegmentedToolbarText_pressed", "selectedTexturedSegmentedToolbarText");
        colors.add("nonexclusiveTexturedToolbarText_pressed", "nonexclusiveTexturedToolbarText");
        colors.add("selectedNonexclusiveTexturedToolbarText_pressed", "selectedNonexclusiveTexturedToolbarText");

        // gradient push and toggle buttons do not change when inactive (although that may change in 10.14 light mode)
        colors.defineNoInactive("gradientText");

        colors.add("selectedGradientText_disabled", "gradientText_disabled");

        return colors;
    }

    private static @NotNull Colors createLightColors() {
        Colors colors = new Colors();
        colors.add("texturedWindowBackground", 212);
        colors.add("texturedWindowBackground_disabled", 246);
        colors.add("capsLockIcon", 0, 100);

        if (OSVersion < 1014) {
            colors.add("text_disabled", 0, 89); // was 192
            colors.add("textBackground_disabled", 255, 89);
            colors.add("windowBackground", 236);
            colors.add("separator_disabled", 0, 8);
            colors.add("controlText_disabled", 0, 77); // was 128
            colors.add("alternateSelectedControlText_disabled", 255, 89); // was 128
            colors.add("selectedMenuItemText", 255);
        }

        colors.add("windowBackground_disabled", "windowBackground");

        // disabled system colors
        if (OSVersion < 1014) {
            colors.add("systemBlue_disabled", 0, 122, 255, 89);
            colors.add("systemGray_disabled", 143, 143, 148, 89);
            colors.add("systemGreen_disabled", 41, 204, 64, 89);
            colors.add("systemOrange_disabled", 255, 148, 0, 89);
            colors.add("systemPurple_disabled", 89, 87, 214, 89);
            colors.add("systemRed_disabled", 255, 59, 48, 89);
            colors.add("systemYellow_disabled", 255, 204, 0, 89);
        }

        // file tag colors
        colors.add("tagBlue", 52, 148, 255);
        colors.add("tagGray", 164, 164, 168);
        colors.add("tagGreen", 91, 215, 105);
        colors.add("tagOrange", 255, 170, 71);
        colors.add("tagPurple", 190, 118, 229);
        colors.add("tagRed", 255, 99, 92);
        colors.add("tagYellow", 254, 214, 75);

        // support for the file chooser
        if (OSVersion == 1013) {
            colors.add("openOptionsArea", 205);
            colors.add("saveOptionsArea", 229);
        } else if (OSVersion >= 1014) {
            colors.add("openOptionsArea", 212);
            colors.add("saveOptionsArea", 236);
        }

        colors.add("selectedBrowserExpandArrow", 110);
        colors.add("selectedBrowserExpandArrow_focused", 255);
        colors.add("browserExpandArrow", 127);
        colors.add("scrollPaneGrabber", 163);

        // support for scroll panes
        colors.add("scrollPaneBorder", 197);

        // colors related to textured buttons and text fields used as textured combo box renderers
        colors.add("texturedText", 0, 170);
        colors.add("texturedText_focused", 0);
        colors.add("texturedText_disabled", 0, 64);
        colors.add("texturedText_inactive", 178);
        colors.add("texturedText_pressed", 34);
        colors.add("texturedText_inactive_disabled", 211);

        colors.add("selectedTexturedText", 255);
        colors.add("selectedTexturedText_disabled", 255, 155);
        colors.add("selectedTexturedText_pressed", "selectedTexturedText");
        colors.add("selectedTexturedText_inactive", 164);
        colors.add("selectedTexturedText_inactive_disabled", 185);

        // colors related to recessed buttons
        colors.add("recessedText", 0, 160);
        colors.add("recessedText_disabled", 0, 64);
        colors.add("recessedText_inactive", 0, 64);
        colors.add("recessedText_pressed", 255);
        colors.add("recessedText_rollover", 255);
        colors.add("recessedText_inactive_disabled", 0, 32);

        colors.add("selectedRecessedText", 255);
        colors.add("selectedRecessedText_disabled", 255, 160);
        colors.add("selectedRecessedText_pressed", 255);
        colors.add("selectedRecessedText_rollover", 255);
        colors.add("selectedRecessedText_inactive", 0, 64);
        colors.add("selectedRecessedText_inactive_disabled", 0, 32);

        // colors related to push buttons
        colors.add("pushButtonText", 34);
        colors.add("pushButtonText_disabled", 0, 64);
        colors.add("pushButtonText_pressed", 255, 224);
        colors.add("pushButtonText_focused", 250);

        colors.add("selectedPushButtonText", "alternateSelectedControlText");

        // colors related to pop up and pop down push buttons
        colors.add("pushPopText_pressed", 0, 224);

        // colors related to segmented buttons
        colors.add("selectedSegmentedText", 255);
        colors.add("selectedSegmentedText_disabled", 172);
        colors.add("selectedSegmentedText_inactive", 34);

        // colors related to textured segmented buttons
        if (OSVersion < 1011) {
            colors.add("selectedNonexclusiveTexturedText", 0, 122, 255);
            colors.add("selectedNonexclusiveTexturedText_disabled", 0, 122, 255,120);
        } else if (OSVersion < 1014) {
            colors.add("selectedNonexclusiveTexturedText", 37, 125, 252);
            colors.add("selectedNonexclusiveTexturedText_disabled", 37, 125, 252, 120);
        } else {
            colors.add("selectedNonexclusiveTexturedText", "controlAccent");
            colors.add("selectedNonexclusiveTexturedText_disabled", "controlAccent_disabled");
            colors.add("selectedNonexclusiveTexturedText_inactive", "segmentedText_disabled");
        }

        colors.add("texturedSegmentedText", 0, 150);
        colors.add("texturedSegmentedText_disabled", 0, 64);
        colors.add("texturedSegmentedText_inactive", 0, 64);
        colors.add("texturedSegmentedText_inactive_disabled", 0, 32);
        colors.add("selectedTexturedSegmentedText_disabled", 255, 144);
        colors.add("selectedTexturedSegmentedText_inactive", 0, 64);
        colors.add("selectedTexturedSegmentedText_inactive_disabled", 0, 32);

        // colors related to gradient buttons
        if (OSVersion < 1014) {
            colors.add("gradientText", 0, 216);
        } else {
            colors.add("gradientText", "controlText");
        }
        colors.add("gradientText_disabled", 0, 64);

        colors.add("selectedGradientText", 0, 216);

        // colors related to round buttons
        colors.add("roundText", 0, 220);
        colors.add("selectedRoundText", 255);
        colors.add("roundText_disabled", 0, 64);
        colors.add("selectedRoundText_disabled", 0, 64);

        // colors related to toolbar items
        colors.add("toolbarItemText", 89);
        colors.add("toolbarItemText_disabled", 150);
        colors.add("toolbarItemText_inactive", 172);
        colors.add("toolbarItemText_inactive_disabled", 172);
        colors.add("toolbarItemText_focused", 250);
        colors.add("selectedToolbarItemText", 81);
        colors.add("selectedToolbarItemText_disabled", 172);
        colors.add("selectedToolbarItemText_inactive", 153);
        colors.add("selectedToolbarItemText_inactive_disabled", 198);

        // colors related to inline buttons
        if (OSVersion < 1014) {
            colors.add("inlineButtonText", 255);
        } else {
            colors.add("inlineButtonText", 255, 224);
        }
        colors.add("inlineButtonText_inactive", "inlineButtonText");
        colors.add("inlineButtonText_disabled", 0, 64);
        colors.add("inlineButtonText_pressed", 255, 224);

        // colors related to sidebars
        colors.add("categoryText", 30, 220);
        colors.add("expandControl", 30, 220);
        colors.add("sidebarBorder", 220);
        colors.add("sidebarIcon", 30, 220);

        // colors related to (non-sidebar) trees
        colors.add("treeIcon", 140);

        // colors related to tables (especially Finder list view)

        colors.add("tableHeader", 37);
        colors.add("tableHeader_disabled", 37);
        colors.add("selectedTableHeader", 37);
        colors.add("tableHeaderBackground", 240);
        colors.add("tableHeaderBackground_disabled", 246);
        colors.add("tableHeaderSeparator", 192);
        colors.add("tableHeaderSeparator_disabled", 197);

        colors.add("browserColumnSeparator", 128);

        // colors related to window content borders

        if (OSVersion < 1014) {
            colors.addColorGradient("topTexturedWindowMarginBackground", 255, 194, 120);
            colors.addAlphaGradient("bottomTexturedWindowMarginBackground", 255, 104, 28);
            colors.addAlphaGradient("topWindowMarginBackground", 0, 4, 28);
            colors.addAlphaGradient("bottomWindowMarginBackground", 0, 8, 22);
        } else {
            colors.addColorGradient("topTexturedWindowMarginBackground", 255, 180, 80);
            colors.addAlphaGradient("bottomTexturedWindowMarginBackground", 255, 144, 8);
            colors.addAlphaGradient("topWindowMarginBackground", 0, 8, 36);
            colors.addAlphaGradient("bottomWindowMarginBackground", 0, 0, 24);
        }

        colors.add("topWindowMarginBackground_disabled", 246);
        colors.add("bottomWindowMarginBackground_disabled", 246);
        colors.add("topTexturedWindowBackground_disabled", 246);
        colors.add("bottomTexturedWindowBackground_disabled", 246);

        if (OSVersion < 1014) {
            colors.add("topWindowDivider", 0, 36);
            colors.add("topWindowDivider_disabled", 0, 28);
            colors.add("bottomWindowDivider", 0, 36);
            colors.add("bottomWindowDivider_disabled", 0, 38);
        } else {
            colors.add("topWindowDivider", 0, 32);
            colors.add("topWindowDivider_disabled", 0, 28);
            colors.add("bottomWindowDivider", 0, 28);
            colors.add("bottomWindowDivider_disabled", 0, 28);
        }

        colors.add("topTexturedWindowDivider", 0, 32);  // was 42
        colors.add("topTexturedWindowDivider_disabled", 0, 28);
        colors.add("bottomTexturedWindowDivider", 0, 56);
        colors.add("bottomTexturedWindowDivider_disabled", 0, 28);

        return colors;
    }

    private static @NotNull Colors createHighContrastLightColors() {
        Colors colors = new Colors();

        if (OSVersion < 1014) {
            colors.add("windowBackground", 246);
            colors.add("windowBackground_disabled", 246);
            colors.add("texturedWindowBackground", 207);
        } else {
            colors.add("windowBackground", 236);
            colors.add("windowBackground_disabled", 236);
            colors.add("texturedWindowBackground", 212);
        }

        if (OSVersion < 1014) {
            colors.addAlphaGradient("topWindowMarginBackground", 0, 16, 40);
            colors.addAlphaGradient("bottomWindowMarginBackground", 0, 22, 36);
        } else {
            colors.addAlphaGradient("topWindowMarginBackground", 0, 8, 34);
            colors.addAlphaGradient("bottomWindowMarginBackground", 0, 0, 26);
            colors.addColorGradient("topTexturedWindowMarginBackground", 255, 200, 104);
            colors.addAlphaGradient("bottomTexturedWindowMarginBackground", 255, 144, 8);
        }

        {
            int c = 0;
            int a = OSVersion < 1014 ? 64 : 74;
            colors.add("topWindowDivider", c, a);
            colors.add("bottomWindowDivider", c, a);
            colors.add("topTexturedWindowDivider", c, a);
            colors.add("bottomTexturedWindowDivider", c, a);

            colors.add("topWindowDivider_disabled", c, a);
            colors.add("bottomWindowDivider_disabled", c, a);
            colors.add("topTexturedWindowDivider_disabled", c, a);
            colors.add("bottomTexturedWindowDivider_disabled", c, a);
        }

        // file tag colors
        colors.add("tagBlue", 68, 171, 229);
        colors.add("tagGray", 164, 164, 167);
        colors.add("tagGreen", 72, 188, 79);
        colors.add("tagOrange", 234, 147, 66);
        colors.add("tagPurple", 160, 106, 188);
        colors.add("tagRed", 255, 99, 92);  // unchanged
        colors.add("tagYellow", 248, 192, 73);

        // support for the file chooser
        colors.add("scrollPaneGrabber", 87);

        // support for scroll panes
        colors.add("scrollPaneBorder", 0, 128);

        return colors;
    }

    // Fixups are alterations that depend on existing definitions being present.

    private static void installFixups(@NotNull Colors colors, @NotNull VAppearance appearance) {

        if (OSVersion < 1014) {
            // Some appearance based colors prior to 10.14 are not represented as system colors.
            // These colors are not used in 10.14 or later.
            Color c = colors.get("controlAccent");
            boolean isGraphite = c != null && (c.getBlue() - c.getRed()) < 30;
            if (isGraphite) {
                colors.add("menuSelectedBackground", 162, 162, 168);
                colors.add("menuBackground", 240);
            } else {
                colors.add("menuSelectedBackground", 54, 148, 253);
                colors.add("menuBackground", 0, 0);
            }
            colors.add("menuForeground", 61);
        }

        if (appearance.isDark()) {
            // A workaround for an apparently bogus color:
            {
                Color c = colors.get("alternatingContentBackground_1_disabled");
                if (c != null && c.getRed() == 255 && c.getAlpha() == 128) {
                    colors.add("alternatingContentBackground_1_disabled", 128, 13);
                }
            }
        }
    }

    private static @NotNull Colors createDarkColors() {

        // colors related to the unified title and toolbar window style (dark mode)

        Colors colors = new Colors();

        colors.add("capsLockIcon", 255, 96);

        colors.add("windowBackground_disabled", 45);
        colors.add("texturedWindowBackground", 42);
        colors.add("texturedWindowBackground_disabled", 45);

        // file tag colors (dark mode)
        colors.add("tagBlue", 63, 157, 255);
        colors.add("tagGray", 172, 172, 176);
        colors.add("tagGreen", 97, 223, 113);
        colors.add("tagOrange", 255, 178, 75);
        colors.add("tagPurple", 204, 124, 245);
        colors.add("tagRed", 255, 107, 99);
        colors.add("tagYellow", 255, 222, 78);

        colors.add("cellBackground", "clear");
        colors.add("selectedCellBackground_inactive", "selectedTextBackground_inactive");

        colors.add("cell", "controlText");
        colors.add("selectedCell", "controlText");
        colors.add("selectedCell_focused", "alternateSelectedControlText");
        colors.add("selectedCell_inactive", "controlText");

        // support for the file chooser (dark mode)
        colors.add("openOptionsArea", 42);
        colors.add("openOptionsArea_disabled", 45);
        colors.remove("saveOptionsArea");

        colors.add("selectedBrowserExpandArrow", 172);
        colors.add("selectedBrowserExpandArrow_focused", 255);
        colors.add("browserExpandArrow", 154);
        colors.add("scrollPaneGrabber", 255, 72);

        // support for scroll panes (dark mode)
        colors.add("scrollPaneBorder", 155, 128);

        // colors related to textured buttons and text fields used as textured combo box renderers (dark mode)
        colors.add("texturedText", "controlText");
        colors.add("texturedText_focused", 0, 192);
        colors.add("texturedText_disabled", "disabledControlText");
        colors.add("texturedText_inactive", "disabledControlText");
        colors.add("texturedText_inactive_disabled", 0, 96);
        colors.add("selectedTexturedText", 0, 192);
        colors.add("selectedTexturedText_disabled", 0, 64);
        colors.add("selectedTexturedText_inactive", 0, 128);
        colors.add("selectedTexturedText_inactive_disabled", 0, 64);

        colors.add("texturedToolbarText", 255); // should be translucent
        colors.add("texturedToolbarText_inactive", 255, 64);
        colors.add("texturedToolbarText_disabled", "disabledControlText");
        colors.add("texturedToolbarText_inactive_disabled", 255, 32);
        colors.add("selectedTexturedToolbarText", 0, 128);
        colors.add("selectedTexturedToolbarText_inactive", "selectedTexturedText_inactive");
        colors.add("selectedTexturedToolbarText_disabled", 0, 32);
        colors.add("selectedTexturedToolbarText_inactive_disabled", "selectedTexturedText_inactive_disabled");

        // colors related to textured segmented buttons (dark mode)
        // default synonyms are used for others

        colors.add("texturedSegmentedText_inactive", 255, 64);
        colors.add("texturedSegmentedText_inactive_disabled", 255, 32);

        colors.add("selectedTexturedSegmentedText", 0, 176);
        colors.add("selectedTexturedSegmentedText_disabled", 0, 64);
        colors.add("selectedTexturedSegmentedText_inactive", 0, 96);
        colors.add("selectedTexturedSegmentedText_inactive_disabled", 0, 64);

        colors.add("nonexclusiveTexturedText", 0, 224);
        colors.add("selectedNonexclusiveTexturedText", "controlAccent_rollover");
        colors.add("selectedNonexclusiveTexturedText_disabled", "controlAccent_disabled");
        colors.add("selectedNonexclusiveTexturedText_inactive", 0, 96);
        colors.add("selectedNonexclusiveTexturedText_inactive_disabled", 0, 48);

        colors.add("texturedSegmentedToolbarText", 255);
        colors.add("texturedSegmentedToolbarText_inactive", 255, 80);
        colors.add("texturedSegmentedToolbarText_inactive_disabled", 255, 32);

        colors.add("selectedTexturedSegmentedToolbarText", 0, 160);
        colors.add("selectedTexturedSegmentedToolbarText_disabled", 0, 32);
        colors.add("selectedTexturedSegmentedToolbarText_inactive", 0, 128);
        colors.add("selectedTexturedSegmentedToolbarText_inactive_disabled", 0, 64);

        colors.add("nonexclusiveTexturedToolbarText_inactive", 255, 80);
        colors.add("selectedNonexclusiveTexturedToolbarText_inactive", 255, 80);

        // colors related to recessed buttons (dark mode)
        colors.add("recessedText", "controlText");
        colors.add("recessedText_disabled", 255, 50);
        colors.add("recessedText_inactive", "recessedText_disabled");
        colors.add("recessedText_pressed", "controlText_pressed");
        colors.add("recessedText_rollover", "controlText_rollover");
        colors.add("selectedRecessedText", 0, 192);
        colors.add("selectedRecessedText_disabled", 0, 64);
        colors.add("selectedRecessedText_pressed", "controlText_pressed");
        colors.add("selectedRecessedText_rollover", "controlText_rollover");
        colors.add("selectedRecessedText_inactive", 0, 192);

        // colors related to push buttons (dark mode)
        colors.add("pushButtonText", "controlText");
        colors.add("pushButtonText_disabled", "disabledControlText");
        colors.add("pushButtonText_pressed", "controlText_pressed");
        colors.add("pushButtonText_focused", "controlText");
        colors.add("selectedPushButtonText", "controlText");

        // colors related to pop up and pop down buttons (dark mode)
        colors.add("pushPopText_pressed", "controlText_pressed");

        // colors related to segmented buttons (dark mode)
        colors.add("segmentedText", "controlText");
        colors.add("segmentedText_disabled", "disabledControlText");
        colors.add("segmentedText_focused", "controlText");

        colors.add("selectedSegmentedText", "controlText");
        colors.add("selectedSegmentedText_disabled", 0, 64);
        colors.add("selectedSegmentedText_inactive", 0, 192);

        colors.add("nonexclusiveText_disabled", "disabledControlText");
        colors.add("nonexclusiveText_inactive", "controlText");

        // colors related to gradient buttons (dark mode)
        colors.add("gradientText", "controlText");
        colors.add("gradientText_disabled", "disabledControlText");

        colors.add("selectedGradientText", 0, 180);
        colors.add("selectedGradientText_inactive", 0, 192);
        colors.add("selectedGradientText_disabled", 0, 64);

        colors.add("selectedGradientSegmentedText", "controlText");
        colors.add("selectedGradientSegmentedText_inactive", 0, 192);
        colors.add("selectedGradientSegmentedText_disabled", "disabledControlText");
        colors.add("selectedGradientSegmentedText_inactive_disabled", 0,64);

        // colors related to rounded rect buttons (dark mode)

        colors.add("selectedRoundedRectText", "roundedRectText");
        colors.add("selectedRoundedRectText_inactive", "roundedRectText_inactive");
        colors.add("selectedRoundedRectText_disabled", "roundedRectText_disabled");
        colors.add("selectedRoundedRectText_inactive_disabled", "roundedRectText_inactive_disabled");

        // colors related to bevel buttons (dark mode)

        colors.addAll("bevelText", "roundedRectText", NO_INACTIVE);

        // colors related to round buttons (dark mode)
        colors.add("roundText", "controlText");
        colors.add("selectedRoundText", 0, 224);
        colors.add("roundText_disabled", "disabledControlText");
        colors.add("selectedRoundText_disabled", 0, 64);

        // colors related to toolbar items (dark mode)
        colors.add("toolbarItemText", "controlText");
        colors.add("toolbarItemText_disabled", "disabledControlText");
        colors.add("toolbarItemText_inactive", "disabledControlText");
        colors.add("toolbarItemText_inactive_disabled", "disabledControlText");
        colors.add("toolbarItemText_focused", "alternateSelectedControlText");
        colors.add("selectedToolbarItemText", "alternateSelectedControlText");
        colors.add("selectedToolbarItemText_disabled", "alternateSelectedControlText_disabled");
        colors.add("selectedToolbarItemText_inactive", "alternateSelectedControlText_disabled");
        colors.add("selectedToolbarItemText_inactive_disabled", "alternateSelectedControlText_disabled");

        // colors related to inline buttons (dark mode)
        colors.add("inlineButtonText", 50); // native appears to use a blending operation
        colors.add("inlineButtonText_disabled", 50);
        colors.add("inlineButtonText_inactive", 50);

        // colors related to sidebars (dark mode)
        colors.add("categoryText", 255, 160);
        colors.add("expandControl", 255, 160);
        colors.add("sidebarBorder", 0);
        colors.add("sidebarIcon", 255, 160);

        // colors related to (non-sidebar) trees (dark mode)
        colors.add("treeIcon", 160);

        // colors related to tables (especially Finder list view) (dark mode)

        colors.add("tableHeader", 230);
        colors.add("tableHeader_disabled", 223);
        colors.add("selectedTableHeader", 230);
        colors.add("tableHeaderBackground", 46);
        colors.add("tableHeaderBackground_disabled", 48);
        colors.add("tableHeaderSeparator", 70);
        colors.add("tableHeaderSeparator_disabled", 83);

        colors.add("browserColumnSeparator", 60);   // my choice, 0 in current beta is invisible

        // colors related to window content borders (dark mode)

        colors.addMagicAlphaGradient("topTexturedWindowMarginBackground", 255, 24, 8);
        colors.add("topTexturedWindowMarginBackground_disabled", new AquaColors.TintedEraser(0, 32));
        colors.addColorGradient("bottomTexturedWindowMarginBackground", 60, 38, 128);
        colors.addAlphaGradient("topWindowMarginBackground", 255, 24, 8);
        colors.add("topWindowMarginBackground_disabled", new AquaColors.TintedEraser(32, 64));
        // using translucent colors will reveal a vibrant background, which is incorrect for the bottom margin
        colors.addColorGradient("bottomWindowMarginBackground", 50, 40, 255);

        colors.add("topWindowDivider_disabled", 55);    // flat divider over vibrant background

        return colors;
    }

    private static @NotNull Colors createHighContrastDarkColors() {
        Colors colors = new Colors();

        colors.add("windowBackground", 50);
        colors.add("texturedWindowBackground", 53);

        // file tag colors (dark mode)
        colors.add("tagBlue", 103, 176, 255);
        colors.add("tagGray", 172, 172, 176);
        colors.add("tagGreen", 91, 215, 105);   // same as ordinary light mode? mistake?
        colors.add("tagOrange", 255, 170, 71);
        colors.add("tagPurple", 226, 165, 254);
        colors.add("tagRed", 255, 135, 129);
        colors.add("tagYellow", 254, 214, 75);    // same as ordinary light mode? mistake?

        // support for the file chooser (dark mode)
        colors.add("scrollPaneGrabber", 255, 196);

        // support for scroll panes (dark mode)
        colors.add("scrollPaneBorder", 153, 128);

        colors.addColorGradient("bottomTexturedWindowMarginBackground", 80, 48, 128);
        // using translucent colors will reveal a vibrant background, which is incorrect for the bottom margin
        colors.addColorGradient("bottomWindowMarginBackground", 66, 53, 255);

        {
            int c = 255;
            int a = 104;
            colors.add("topWindowDivider", c, a);
            colors.add("bottomWindowDivider", c, a);
            colors.add("topTexturedWindowDivider", c, a);
            colors.add("bottomTexturedWindowDivider", c, a);

            colors.add("topWindowDivider_disabled", c, a);
            colors.add("bottomWindowDivider_disabled", c, a);
            colors.add("topTexturedWindowDivider_disabled", c, a);
            colors.add("bottomTexturedWindowDivider_disabled", c, a);
        }

        return colors;
    }

    private static class Colors {
        private final @NotNull Map<String,Color> colors = new HashMap<>();
        private final @NotNull Map<String,String> synonyms = new HashMap<>();

        /**
         * This method is for final consumption only.
         */
        public @NotNull Map<String,Color> getColors() {
            applySynonyms(synonyms);
            return Collections.unmodifiableMap(colors);
        }

        private void applySynonyms(@NotNull Map<String,String> synonyms) {
            // The goal is to support (short) chains of synonyms without risk of infinite loop

            Map<String,Color> results = new HashMap<>();

            for (String name : synonyms.keySet()) {
                Color c = getIndirect(name, 5);
                if (c != null) {
                    results.put(name, c);
                } else if (false) {
                    String s = getIndirectPath(name, 5);
                    System.err.println("Color " + name + " has no indirect definition: " + s);
                }
            }

            for (String name : results.keySet()) {
                Color c = results.get(name);
                colors.put(name, c);
            }
        }

        private @Nullable Color getIndirect(@NotNull String name, int limit) {
            if (limit < 0) {
                return null;
            }
            String nextName = synonyms.get(name);
            if (nextName != null) {
                return getIndirect(nextName, limit-1);
            }

            return colors.get(name);
        }

        private @NotNull String getIndirectPath(@NotNull String name, int limit) {
            if (limit < 0) {
                return "...";
            }
            String nextName = synonyms.get(name);
            if (nextName != null) {
                return name + " " + getIndirectPath(nextName, limit-1);
            }
            return name;
        }

        public @Nullable Color get(@NotNull String name) {
            return colors.get(name);
        }

        private void internalAdd(@NotNull String name, @NotNull Color color) {
            synonyms.remove(name);
            if (!(color instanceof ColorUIResource)) {
                color = new ColorUIResource(color);
            }
            colors.put(name, color);
        }

        public void add(@NotNull String name, int color) {
            Color c = new ColorUIResource(color, color, color);
            internalAdd(name, c);
        }

        public void add(@NotNull String name, int red, int green, int blue) {
            Color c = new ColorUIResource(red, green, blue);
            internalAdd(name, c);
        }

        public void add(@NotNull String name, int red, int green, int blue, int alpha) {
            Color c = new ColorUIResource(new Color(red, green, blue, alpha));
            internalAdd(name, c);
        }

        public void add(@NotNull String name, int intensity, int alpha) {
            Color c = new ColorUIResource(new Color(intensity, intensity, intensity, alpha));
            internalAdd(name, c);
        }

        public void addColorGradient(@NotNull String name, int start, int finish, int alpha) {
            Color startColor = new Color(start, start, start, alpha);
            Color finishColor = new Color(finish, finish, finish, alpha);
            Color gradientColor = new AquaColors.GradientColor(startColor, finishColor);
            internalAdd(name, gradientColor);
        }

        public void addMagicColorGradient(@NotNull String name, int start, int finish, int alpha) {
            Color startColor = new Color(start, start, start, alpha);
            Color finishColor = new Color(finish, finish, finish, alpha);
            Color gradientColor = new AquaColors.GradientColor(startColor, finishColor, true);
            internalAdd(name, gradientColor);
        }

        public void addAlphaGradient(@NotNull String name, int intensity, int startAlpha, int finishAlpha) {
            Color startColor = new Color(intensity, intensity, intensity, startAlpha);
            Color finishColor = new Color(intensity, intensity, intensity, finishAlpha);
            Color gradientColor = new AquaColors.GradientColor(startColor, finishColor);
            internalAdd(name, gradientColor);
        }

        public void addMagicAlphaGradient(@NotNull String name, int intensity, int startAlpha, int finishAlpha) {
            Color startColor = new Color(intensity, intensity, intensity, startAlpha);
            Color finishColor = new Color(intensity, intensity, intensity, finishAlpha);
            Color gradientColor = new AquaColors.GradientColor(startColor, finishColor, true);
            internalAdd(name, gradientColor);
        }

        public void add(@NotNull String name, @NotNull Color color) {
            internalAdd(name, color);
        }

        public void addAll(@NotNull Map<String,Color> cs) {
            for (String name : cs.keySet()) {
                Color c = cs.get(name);
                internalAdd(name, c);
            }
        }

        private void internalAdd(@NotNull String name, @NotNull String synonym) {
            colors.remove(name);
            synonyms.put(name, synonym);
        }

        public void add(@NotNull String name, @NotNull String synonym) {
            internalAdd(name, synonym);
        }

        public void addAll(@NotNull String root, @NotNull String synonymRoot) {
            add(root, synonymRoot);
            for (String suffix : AquaColors.getAllColorSuffixes()) {
                addDerived(root, synonymRoot, suffix);
            }
            String selectedRoot = AquaColors.createSelectedColorName(root);
            String selectedSynonymRoot = AquaColors.createSelectedColorName(synonymRoot);
            add(selectedRoot, selectedSynonymRoot);
            for (String suffix : AquaColors.getAllColorSuffixes()) {
                addDerived(selectedRoot, selectedSynonymRoot, suffix);
            }
        }

        private void addDerived(@NotNull String root, @NotNull String synonymRoot, @NotNull String suffix)
        {
            add(root + suffix, synonymRoot + suffix);
        }

        public void addAll(@NotNull String root, @NotNull String synonymRoot, int option) {
            add(root, synonymRoot);
            for (String suffix : AquaColors.getAllColorSuffixes()) {
                addDerived(root, synonymRoot, suffix, option);
            }
            String selectedRoot = AquaColors.createSelectedColorName(root);
            String selectedSynonymRoot = AquaColors.createSelectedColorName(synonymRoot);
            add(selectedRoot, selectedSynonymRoot);
            for (String suffix : AquaColors.getAllColorSuffixes()) {
                addDerived(selectedRoot, selectedSynonymRoot, suffix, option);
            }
        }

        private void addDerived(@NotNull String root, @NotNull String synonymRoot, @NotNull String suffix, int option)
        {
            if ((option & NO_INACTIVE) != 0) {
                // define inactive variants in terms of the non-inactive variant
                String nonInactiveSuffix = AquaColors.withoutInactive(suffix);
                if (nonInactiveSuffix != null) {
                    add(root + suffix, root + nonInactiveSuffix);
                    return;
                }
            }

            add(root + suffix, synonymRoot + suffix);
        }

        public void defineNoInactive(@NotNull String root)
        {
           // define inactive variants in terms of the non-inactive variant
           for (String suffix : AquaColors.getAllColorSuffixes()) {
                String nonInactiveSuffix = AquaColors.withoutInactive(suffix);
                if (nonInactiveSuffix != null) {
                    add(root + suffix, root + nonInactiveSuffix);
                }
            }

            String selectedRoot = AquaColors.createSelectedColorName(root);
            for (String suffix : AquaColors.getAllColorSuffixes()) {
                String nonInactiveSuffix = AquaColors.withoutInactive(suffix);
                if (nonInactiveSuffix != null) {
                    add(selectedRoot + suffix, selectedRoot + nonInactiveSuffix);
                }
            }
        }

        public void remove(@NotNull String name) {
            colors.remove(name);
        }

        public void add(@NotNull Colors cs) {
            Map<String,Color> colorsToAdd = cs.colors;
            for (String name : colorsToAdd.keySet()) {
                Color c = colorsToAdd.get(name);
                internalAdd(name, c);
            }
            Map<String,String> synonymsToAdd = cs.synonyms;
            for (String name : synonymsToAdd.keySet()) {
                String ref = synonymsToAdd.get(name);
                internalAdd(name, ref);
            }
        }
    }

    @Override
    public @NotNull String toString() {
        return super.toString() + "[" + appearance.getName() + "]";
    }
}
