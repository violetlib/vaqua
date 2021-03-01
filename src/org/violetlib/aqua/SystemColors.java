/*
 * Copyright (c) 2018-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A set of color definitions that can be combined to form a complete set of colors for an appearance.
 * Color names are defined in terms of specific colors or as synonyms.
 */

public class SystemColors {
    public final @NotNull BasicColors defaultColors;
    public final @NotNull BasicColors lightColors;
    public final @NotNull BasicColors darkColors;
    public final @NotNull BasicColors highContrastLightColors;
    public final @NotNull BasicColors highContrastDarkColors;

    private final @Nullable ColorsInstrumentation instrumentation;
    private final @NotNull Logger log;

    public SystemColors(int OSVersion, @Nullable ColorsInstrumentation instrumentation, @NotNull Logger log) {
        this.instrumentation = instrumentation;
        this.log = log;

        defaultColors = createDefaultColors(OSVersion);
        lightColors = createLightColors(OSVersion);
        darkColors = createDarkColors(OSVersion);
        highContrastLightColors = createHighContrastLightColors(OSVersion);
        highContrastDarkColors = createHighContrastDarkColors(OSVersion);
    }

    private @NotNull BasicColors createDefaultColors(int OSVersion) {
        BasicColorsBuilder colors = new BasicColorsBuilder("Basic", instrumentation, log);
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
        colors.add("textBackground_inactive", "textBackground");
        colors.add("searchFieldPrompt", "text_disabled");

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
        colors.addAll("roundedRectText", "gradientText", BasicColorsBuilder.NO_INACTIVE);

        // bevel push and toggle buttons do not change when inactive
        colors.addAll("bevelText", "gradientText", BasicColorsBuilder.NO_INACTIVE);

        // push buttons mostly do not change when inactive
        colors.defineNoInactive("pushButtonText");

        // default style segmented buttons mostly do not change when inactive
        // they do not have pressed behavior
        colors.addAll("segmentedText", "pushButtonText", BasicColorsBuilder.NO_INACTIVE);
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

        return colors.get();
    }

    private @NotNull BasicColors createLightColors(int OSVersion) {
        BasicColorsBuilder colors = new BasicColorsBuilder("Light", instrumentation, log);
        colors.add("texturedWindowBackground", 212);
        colors.add("texturedWindowBackground_disabled", 246);
        colors.add("capsLockIcon", 0, 100);
        colors.add("searchFieldPrompt", 0, 63);

        if (OSVersion < 1014) {
            colors.add("text_disabled", 0, 89); // was 192
            colors.add("textBackground_disabled", 255, 89);
            colors.add("windowBackground", 236);
            colors.add("separator_disabled", 0, 8);
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
        colors.add("tagBlue_disabled", 155, 203, 255);
        colors.add("tagGray_disabled", 210, 210, 212);
        colors.add("tagGreen_disabled", 175, 236, 181);
        colors.add("tagOrange_disabled", 255, 213, 165);
        colors.add("tagPurple_disabled", 223, 187, 243);
        colors.add("tagRed_disabled", 255, 177, 175);
        colors.add("tagYellow_disabled", 254, 235, 168);

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

        if (OSVersion < 1016) {
            colors.add("selectedTexturedText", 255);
            colors.add("selectedTexturedText_disabled", 255, 155);
            colors.add("selectedTexturedText_pressed", "selectedTexturedText");
            colors.add("selectedTexturedText_inactive", 164);
            colors.add("selectedTexturedText_inactive_disabled", 185);
        }

        // colors related to recessed buttons
        colors.add("recessedText", 0, 160);
        colors.add("recessedText_disabled", 0, 64);
        colors.add("recessedText_inactive", 0, 64);
        colors.add("recessedText_pressed", 255);
        colors.add("recessedText_rollover", 255);
        colors.add("recessedText_inactive_disabled", 0, 32);

        if (OSVersion >= 1016) {
            colors.add("recessedText", 0, 140);
            colors.add("selectedRecessedText", 0, 206);
            colors.add("selectedRecessedText_disabled", 0, 64);
            colors.add("selectedRecessedText_pressed", 0, 140);
            colors.add("selectedRecessedText_rollover", 0, 140);
            colors.add("selectedRecessedText_inactive", 0, 64);
            colors.add("selectedRecessedText_inactive_disabled", 0, 32);
        }

        // colors related to push buttons
        colors.add("pushButtonText", 34);
        colors.add("pushButtonText_disabled", 0, 64);
        colors.add("pushButtonText_pressed", 255, 224);
        colors.add("pushButtonText_focused", 250);

        colors.add("selectedPushButtonText", "alternateSelectedControlText");

        // colors related to pop up and pop down push buttons
        colors.add("pushPopText_pressed", 0, 224);

        // colors related to segmented buttons

        if (OSVersion >= 1016) {
            // Tab = Exclusive Rounded, but no longer the same as Exclusive Separated
            colors.addAll("segmentedText", "controlText");
            colors.add("segmentedText_inactive", 0, 240);
            colors.add("segmentedText_disabled_inactive", 0, 78);
            colors.add("selectedSegmentedText_disabled", 0, 78);
            colors.add("selectedSegmentedText_disabled_inactive", 0, 78);
            colors.add("selectedSegmentedText_inactive", 34);
            colors.add("selectedSegmentedSeparatedText", 255);
            colors.add("selectedSegmentedSeparatedText_disabled", 255, 140);
            colors.add("selectedSegmentedSeparatedText_inactive", 34);
            // changes to selected colors
            colors.add("selectedGradientSegmentedText", 255);
            colors.add("selectedGradientSegmentedText_disabled", 255, 128);
            colors.add("selectedNonexclusiveText", 255);
            colors.add("selectedNonexclusiveText_disabled", 255, 140);
        } else {
            colors.add("selectedSegmentedText", 255);
            colors.add("selectedSegmentedText_disabled", 172);
            colors.add("selectedSegmentedText_inactive", 34);
            if (OSVersion == 1014 || OSVersion == 1015) {
                colors.add("selectedGradientSegmentedText", 255);
                colors.add("selectedGradientSegmentedText_disabled", 255, 144);
            }
        }

        // colors related to textured segmented buttons
        if (OSVersion < 1011) {
            colors.add("selectedNonexclusiveTexturedText", 0, 122, 255);
            colors.add("selectedNonexclusiveTexturedText_disabled", 0, 122, 255, 120);
        } else if (OSVersion < 1014) {
            colors.add("selectedNonexclusiveTexturedText", 37, 125, 252);
            colors.add("selectedNonexclusiveTexturedText_disabled", 37, 125, 252, 120);
        } else {
            colors.add("selectedNonexclusiveTexturedText", "controlAccent");
            colors.add("selectedNonexclusiveTexturedText_rollover", "controlAccent");
            colors.add("selectedNonexclusiveTexturedText_disabled", "controlAccent_disabled");
            colors.add("selectedNonexclusiveTexturedText_inactive", "segmentedText_disabled");
        }

        if (OSVersion >= 1016) {
            // Exclusive textured is like rounded, except for selected disabled and selected inactive
            colors.add("texturedSegmentedText", "controlText");
            colors.add("selectedTexturedSegmentedText_disabled", 64);
            colors.add("nonexclusiveTexturedToolbarText", 0, 178);
            colors.add("selectedTexturedSegmentedToolbarText", 0, 192);
            colors.add("texturedSegmentedToolbarText", 0, 178);
            colors.add("selectedNonexclusiveTexturedToolbarText_inactive", 0, 152);
            colors.add("selectedNonexclusiveTexturedToolbarText_rollover", "controlAccent");
            colors.add("nonexclusiveTexturedToolbarText_inactive", 0, 86);
            colors.add("texturedSegmentedToolbarText_inactive", 0, 86);
            colors.add("nonexclusiveTexturedToolbarText_disabled", 0, 86);
            colors.add("texturedSegmentedToolbarText_disabled", 0, 86);
            colors.add("selectedNonexclusiveTexturedToolbarText_inactive_disabled", 0, 36);
            colors.add("nonexclusiveTexturedToolbarText_inactive_disabled", 0, 36);
            colors.add("selectedTexturedSegmentedToolbarText_inactive_disabled", 0, 20);
            colors.add("texturedSegmentedToolbarText_inactive_disabled", 0, 36);
        } else {
            colors.add("texturedSegmentedText", 0, 150);
            colors.add("texturedSegmentedText_disabled", 0, 64);
            colors.add("texturedSegmentedText_inactive", 0, 64);
            colors.add("texturedSegmentedText_inactive_disabled", 0, 32);
            colors.add("selectedTexturedSegmentedText_disabled", 255, 144);
            colors.add("selectedTexturedSegmentedText_inactive_disabled", 0, 32);
        }
        colors.add("selectedTexturedSegmentedText_inactive", 0, 64);

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
            colors.add("inlineButtonText_inactive", "inlineButtonText");
            colors.add("inlineButtonText_disabled", 0, 64);
            colors.add("inlineButtonText_pressed", 255, 224);
        } else if (OSVersion < 1016) {
            colors.add("inlineButtonText", 255, 224);
            colors.add("inlineButtonText_inactive", "inlineButtonText");
            colors.add("inlineButtonText_disabled", 0, 64);
            colors.add("inlineButtonText_pressed", 255, 224);
        } else {
            colors.add("inlineButtonText", 0, 140);
            //colors.add("selectedInlineButtonText", 0, 140);
            colors.add("inlineButtonText_disabled", 0, 80);
            //colors.add("selectedInlineButtonText_disabled", 0, 80);
        }

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

        return colors.get();
    }

    private @NotNull BasicColors createHighContrastLightColors(int OSVersion) {
        BasicColorsBuilder colors = new BasicColorsBuilder("High Contrast Light", instrumentation, log);

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
        colors.add("tagBlue_disabled", 163, 214, 242);
        colors.add("tagGray_disabled", 210, 210, 211);
        colors.add("tagGreen_disabled", 166, 223, 168);
        colors.add("tagOrange_disabled", 245, 202, 162);
        colors.add("tagPurple_disabled", 208, 182, 222);
        colors.add("tagRed_disabled", 255, 177, 175);  // unchanged
        colors.add("tagYellow_disabled", 252, 224, 167);

        // support for the file chooser
        colors.add("scrollPaneGrabber", 87);

        // support for scroll panes
        colors.add("scrollPaneBorder", 0, 128);

        return colors.get();
    }

    private @NotNull BasicColors createDarkColors(int OSVersion) {
        BasicColorsBuilder colors = new BasicColorsBuilder("Dark", instrumentation, log);

        colors.add("capsLockIcon", 255, 96);
        colors.add("searchFieldPrompt", 255, 63);

        // colors related to the unified title and toolbar window style (dark mode)
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
        colors.add("tagBlue_disabled", 47, 94, 142);
        colors.add("tagGray_disabled", 100, 101, 103);
        colors.add("tagGreen_disabled", 64, 126, 72);
        colors.add("tagOrange_disabled", 142, 104, 53);
        colors.add("tagPurple_disabled", 117, 77, 137);
        colors.add("tagRed_disabled", 142, 68, 64);
        colors.add("tagYellow_disabled", 142, 126, 55);

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
        if (OSVersion < 1016) {
            colors.add("texturedText", "controlText");
        } else {
            colors.add("texturedText", 255, 115);
        }
        colors.add("texturedText_focused", 0, 192);
        colors.add("texturedText_disabled", "disabledControlText");
        colors.add("texturedText_inactive", "disabledControlText");
        colors.add("texturedText_inactive_disabled", 0, 96);

        if (OSVersion < 1016) {
            colors.add("selectedTexturedText", 0, 192);
            colors.add("selectedTexturedText_disabled", 0, 64);
            if (OSVersion == 1015) {
                colors.add("selectedTexturedText_inactive", 0, 192);
            } else {
                colors.add("selectedTexturedText_inactive", 0, 128);
            }
            colors.add("selectedTexturedText_inactive_disabled", 0, 64);
        } else {
            colors.addAll("selectedTexturedText", "controlText");
        }

        colors.add("texturedToolbarText", "controlText");
        colors.add("texturedToolbarText_inactive", 255, 64);
        colors.add("texturedToolbarText_disabled", "disabledControlText");
        colors.add("texturedToolbarText_inactive_disabled", 255, 32);

        if (OSVersion < 1016) {
            colors.add("selectedTexturedToolbarText", 0, 128);
            colors.add("selectedTexturedToolbarText_inactive", "selectedTexturedText_inactive");
            colors.add("selectedTexturedToolbarText_disabled", 0, 32);
            colors.add("selectedTexturedToolbarText_inactive_disabled", "selectedTexturedText_inactive_disabled");
        } else {
            colors.addAll("selectedTexturedToolbarText", "controlText");
        }

        // colors related to textured segmented buttons (dark mode)

        // the "Off" state generally follows the "Off" state of a toggle button which is the same as
        // the normal state of a stateless button (a push button)

        // default synonyms are used for others

        colors.add("texturedSegmentedText_inactive", 255, 64);
        colors.add("texturedSegmentedText_inactive_disabled", 255, 32);

        colors.add("nonexclusiveTexturedText", 0, 224);
        colors.add("selectedNonexclusiveTexturedText", "controlAccent_rollover");
        colors.add("selectedNonexclusiveTexturedText_rollover", "controlAccent_rollover");
        colors.add("selectedNonexclusiveTexturedText_disabled", "controlAccent_disabled");
        colors.add("selectedNonexclusiveTexturedText_inactive", 0, 96);
        colors.add("selectedNonexclusiveTexturedText_inactive_disabled", 0, 48);
        colors.add("nonexclusiveTexturedToolbarText_inactive", 255, 80);
        colors.add("selectedNonexclusiveTexturedToolbarText_inactive", 255, 80);

        if (OSVersion < 1016) {

            // In 10.14 (and perhaps other releases), textured segmented buttons in the active state are the same
            // in a window or in a toolbar, but they differ when inactive.

            colors.add("selectedTexturedSegmentedText", 0, 200);
            colors.add("selectedTexturedSegmentedText_disabled", 0, 72);
            colors.add("selectedTexturedSegmentedText_inactive", 0, 117);
            colors.add("selectedTexturedSegmentedText_inactive_disabled", 0, 64);

            // selected nonexclusive enabled/disabled use accent color
            colors.add("selectedNonexclusiveTexturedText_inactive", 0, 108);
            colors.add("selectedNonexclusiveTexturedText_inactive_disabled", 0, 64);

            // toolbar buttons in the active state are the same as the non-toolbar buttons
            colors.addAll("selectedTexturedSegmentedToolbarText", "selectedTexturedSegmentedText");

            // things change when inactive
            // in fact, in the select-any case, there is no difference between On and Off
            // therefore, my choice for the active case is to use the normally bright accent color

            colors.add("selectedTexturedSegmentedToolbarText_inactive_disabled", 255, 48);
            colors.add("selectedNonexclusiveTexturedToolbarText_inactive_disabled", 255, 64);

        } else {
            colors.add("selectedTexturedSegmentedText", "controlText");
            colors.add("selectedTexturedSegmentedText_disabled", "texturedText_disabled");
            colors.add("selectedTexturedSegmentedText_inactive", "texturedText_inactive");
            colors.add("selectedTexturedSegmentedText_inactive_disabled", "texturedText_inactive_disabled");

            colors.add("texturedSegmentedText", "controlText");

            colors.add("texturedSegmentedToolbarText", "texturedText");
            colors.add("texturedSegmentedToolbarText_disabled", "texturedText_disabled");
            colors.add("texturedSegmentedToolbarText_inactive", "texturedText_inactive");
            colors.add("texturedSegmentedToolbarText_inactive_disabled", "texturedText_inactive_disabled");

            colors.add("selectedTexturedSegmentedToolbarText", "controlText");
            colors.add("selectedTexturedSegmentedToolbarText_disabled", "selectedTexturedText_disabled");
            colors.add("selectedTexturedSegmentedToolbarText_inactive", "selectedTexturedText_inactive");
            colors.add("selectedTexturedSegmentedToolbarText_inactive_disabled", "selectedTexturedText_inactive_disabled");

            colors.add("nonexclusiveTexturedToolbarText", 255, 148);
            colors.add("selectedTexturedSegmentedToolbarText", 255, 226);
            colors.add("texturedSegmentedToolbarText", 255, 148);

            colors.add("selectedNonexclusiveTexturedToolbarText_inactive", 255, 136);
            colors.add("nonexclusiveTexturedToolbarText_inactive", 255, 60);
            colors.add("selectedTexturedSegmentedToolbarText_inactive", 255, 48);
            colors.add("texturedSegmentedToolbarText_inactive", 255, 60);

            colors.add("nonexclusiveTexturedToolbarText_disabled", 255, 36);
            colors.add("selectedTexturedSegmentedToolbarText_disabled", 255, 60);
            colors.add("texturedSegmentedToolbarText_disabled", 255, 36);

            colors.add("selectedNonexclusiveTexturedToolbarText_inactive_disabled", 255, 32);
            colors.add("nonexclusiveTexturedToolbarText_inactive_disabled", 255, 32);
            colors.add("selectedTexturedSegmentedToolbarText_inactive_disabled", 255, 16);
            colors.add("texturedSegmentedToolbarText_inactive_disabled", 255, 32);
            colors.add("selectedNonexclusiveTexturedToolbarText_rollover", "controlAccent_rollover");
        }

        // colors related to recessed buttons (dark mode)
        colors.addAll("recessedText", "controlText");
        if (OSVersion >= 1015) {
            colors.add("recessedText", 255, 115);
        }

        colors.add("selectedRecessedText_pressed", "controlText_pressed");
        colors.add("selectedRecessedText_rollover", "controlText_rollover");
        if (OSVersion >= 1016) {
            colors.add("selectedRecessedText", 255, 192);
            colors.add("selectedRecessedText_disabled", 255, 64);
        } else {
            colors.add("selectedRecessedText", 0, 192);
            colors.add("selectedRecessedText_disabled", 0, 64);
            colors.add("selectedRecessedText_inactive", 0, 192);
        }

        // colors related to push buttons (dark mode)
        colors.add("pushButtonText", "controlText");
        colors.add("pushButtonText_disabled", "disabledControlText");
        colors.add("pushButtonText_pressed", "controlText_pressed");
        colors.add("pushButtonText_focused", "controlText");
        if (OSVersion < 1016) {
            colors.add("selectedPushButtonText", "controlText");
        }

        // colors related to pop up and pop down buttons (dark mode)
        colors.add("pushPopText_pressed", "controlText_pressed");

        // colors related to segmented buttons (dark mode)
        if (OSVersion < 1016) {
            colors.add("segmentedText", "controlText");
            colors.add("segmentedText_focused", "controlText");
        } else {
            colors.add("tabText", "controlText");
            colors.add("tabText_focused", "controlText");
            colors.add("tabText_inactive", "controlText");
            colors.add("segmentedText", 255, 192);
            colors.add("segmentedText_disabled", 255, 48);
            colors.add("segmentedText_inactive", "segmentedText");
            colors.add("segmentedText_inactive_disabled", "segmentedText_disabled");
        }

        colors.addAll("selectedSegmentedText", "controlText");
        //colors.add("selectedSegmentedText_disabled", 0, 64);

        if (OSVersion == 1014 || OSVersion == 1015) {
            colors.add("selectedTabText_disabled", 0, 96);
            colors.add("selectedTabText_inactive_disabled", 0, 96);
            colors.add("selectedSegmentedText_inactive", 0, 192);
        } else if (OSVersion >= 1016) {
//            colors.add("selectedSegmentedText", 0, 192);
//            colors.add("selectedSegmentedText_disabled", 0, 64);
//            colors.add("selectedSegmentedText_inactive", "selectedSegmentedText");
//            colors.add("selectedSegmentedText_inactive_disabled", "selectedSegmentedText_disabled");
        }

        colors.add("nonexclusiveText_disabled", "disabledControlText");
        colors.add("nonexclusiveText_inactive", "controlText");

        // colors related to gradient buttons (dark mode)
        colors.addAll("gradientText", "controlText");

        if (OSVersion == 1014) {
            colors.add("selectedGradientText", 0, 180);
            colors.add("selectedGradientText_disabled", 0, 64);
            colors.add("selectedGradientText_inactive", 0, 180);
            colors.add("selectedGradientText_inactive_disabled", 0, 64);
        }

        colors.addAll("gradientSegmentedText", "controlText");
        colors.add("selectedGradientSegmentedText_inactive", 0, 192);

        if (OSVersion >= 1016) {
            colors.add("selectedGradientSegmentedText_inactive_disabled", 0, 72);
        }

//            colors.add("selectedGradientSegmentedText_disabled", 255, 108);
//            colors.add("selectedGradientSegmentedText_inactive_disabled", 255, 76);
//            colors.add("gradientSegmentedText_inactive", "controlText");
//            colors.add("selectedGradientSegmentedText_inactive", 0, 192);
//            colors.add("selectedGradientSegmentedText_inactive_disabled", 0, 64);
//            colors.add("selectedGradientSegmentedText", "controlText");

        // colors related to rounded rect buttons (dark mode)

        colors.addAll("selectedRoundedRectText", "roundedRectText");

        // colors related to bevel buttons (dark mode)

        colors.addAll("bevelText", "roundedRectText", BasicColorsBuilder.NO_INACTIVE);

        // colors related to round buttons (dark mode)
        colors.add("roundText", "controlText");
        colors.add("selectedRoundText", 0, 224);
        colors.add("roundText_disabled", "disabledControlText");
        colors.add("selectedRoundText_disabled", 0, 64);

        // colors related to toolbar items (dark mode)
        colors.add("toolbarItemText", "controlText");
        colors.add("toolbarItemText_inactive", "disabledControlText");
        colors.add("toolbarItemText_focused", "alternateSelectedControlText");
        colors.add("selectedToolbarItemText", "alternateSelectedControlText");
        colors.add("selectedToolbarItemText_disabled", "alternateSelectedControlText_disabled");
        colors.add("selectedToolbarItemText_inactive", "alternateSelectedControlText_disabled");
        colors.add("selectedToolbarItemText_inactive_disabled", "alternateSelectedControlText_disabled");

        // colors related to inline buttons (dark mode)
        //  note: native rendering appears to use a blending operation
        if (OSVersion < 1016) {
            colors.add("inlineButtonText", 50);
        } else {
            colors.add("inlineButtonText", 255, 140);
            colors.add("inlineButtonText_inactive", 255, 115);
            colors.add("inlineButtonText_disabled", 255, 115);
            colors.add("inlineButtonText_inactive_disabled", 255, 115);
            colors.add("selectedInlineButtonText", 255, 180);
        }

        // colors related to Tab buttons

        if (OSVersion == 1014 || OSVersion == 1015) {
            // This rule needed because AquaColors search rule favors mapping to disabled, which is probably a
            // mistake.
            colors.add("selectedTabText_inactive", 0, 192);
        }

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

        colors.add("browserColumnSeparator", 60);  // my choice, 0 in current beta is invisible

        // colors related to window content borders (dark mode)

        colors.addMagicAlphaGradient("topTexturedWindowMarginBackground", 255, 24, 8);
        colors.add("topTexturedWindowMarginBackground_disabled", new TintedEraser(0, 32));
        colors.addColorGradient("bottomTexturedWindowMarginBackground", 60, 38, 128);
        colors.addAlphaGradient("topWindowMarginBackground", 255, 24, 8);
        colors.add("topWindowMarginBackground_disabled", new TintedEraser(32, 64));
        // using translucent colors will reveal a vibrant background, which is incorrect for the bottom margin
        colors.addColorGradient("bottomWindowMarginBackground", 50, 40, 255);

        colors.add("topWindowDivider_disabled", 55);  // flat divider over vibrant background

        return colors.get();
    }

    private @NotNull BasicColors createHighContrastDarkColors(int OSVersion) {
        BasicColorsBuilder colors = new BasicColorsBuilder("High Contrast Dark", instrumentation, log);

        colors.add("windowBackground", 50);
        colors.add("texturedWindowBackground", 53);

        // file tag colors (dark mode)
        colors.add("tagBlue", 103, 176, 255);
        colors.add("tagGray", 172, 172, 176);
        colors.add("tagGreen", 91, 215, 105);  // same as ordinary light mode? mistake?
        colors.add("tagOrange", 255, 170, 71);
        colors.add("tagPurple", 226, 165, 254);
        colors.add("tagRed", 255, 135, 129);
        colors.add("tagYellow", 254, 214, 75);  // same as ordinary light mode? mistake?
        colors.add("tagBlue_disabled", 67, 104, 144);
        colors.add("tagGray_disabled", 102, 102, 104);
        colors.add("tagGreen_disabled", 62, 123, 69);
        colors.add("tagOrange_disabled", 143, 101, 52);
        colors.add("tagPurple_disabled", 128, 98, 143);
        colors.add("tagRed_disabled", 143, 83, 80);
        colors.add("tagYellow_disabled", 143, 123, 55);

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

        return colors.get();
    }
}
