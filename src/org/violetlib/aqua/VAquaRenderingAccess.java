/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Support access to the VAquaRendering library.
 */
public class VAquaRenderingAccess {

    // Widgets that may not be available depending upon the version of VAquaRendering
    public static final @Nullable AquaUIPainter.SegmentedButtonWidget SLIDER_WIDGET = getSegmentedSliderWidget();
    public static final @Nullable AquaUIPainter.SegmentedButtonWidget SLIDER_TOOLBAR_WIDGET = getSegmentedSliderToolbarWidget();
    public static final @Nullable AquaUIPainter.SegmentedButtonWidget SLIDER_TOOLBAR_ICONS_WIDGET = getSegmentedSliderToolbarIconsWidget();
    public static final @Nullable AquaUIPainter.ButtonWidget BUTTON_TEXTURED_TOOLBAR_ICONS = getTexturedToolbarIconsWidget();
    public static final @Nullable AquaUIPainter.SegmentedButtonWidget SEGMENTED_TEXTURED_TOOLBAR_ICONS_WIDGET = getSegmentedTexturedToolbarIconsWidget();
    public static final @Nullable AquaUIPainter.SegmentedButtonWidget TEXTURED_SEPARATED_TOOLBAR_ICONS_WIDGET = getSegmentedTexturedSeparatedToolbarIconsWidget();

    private static @Nullable AquaUIPainter.SegmentedButtonWidget getSegmentedSliderWidget() {
        // This field was introduced in release 10 of VAquaRendering
        try {
            return AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SLIDER;
        } catch (NoSuchFieldError e) {
            return null;
        }
    }

    private static @Nullable AquaUIPainter.SegmentedButtonWidget getSegmentedSliderToolbarWidget() {
        // This field was introduced in release 10 of VAquaRendering
        try {
            return AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SLIDER_TOOLBAR;
        } catch (NoSuchFieldError e) {
            return null;
        }
    }

    private static @Nullable AquaUIPainter.SegmentedButtonWidget getSegmentedSliderToolbarIconsWidget() {
        // This field was introduced in release 10 of VAquaRendering
        try {
            return AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_SLIDER_TOOLBAR_ICONS;
        } catch (NoSuchFieldError e) {
            return null;
        }
    }

    private static @Nullable AquaUIPainter.ButtonWidget getTexturedToolbarIconsWidget() {
        // This field was introduced in release 10 of VAquaRendering
        try {
            return AquaUIPainter.ButtonWidget.BUTTON_TEXTURED_TOOLBAR_ICONS;
        } catch (NoSuchFieldError e) {
            return null;
        }
    }

    private static @Nullable AquaUIPainter.SegmentedButtonWidget getSegmentedTexturedToolbarIconsWidget() {
        // This field was introduced in release 10 of VAquaRendering
        try {
            return AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_TEXTURED_TOOLBAR_ICONS;
        } catch (NoSuchFieldError e) {
            return null;
        }
    }

    private static @Nullable AquaUIPainter.SegmentedButtonWidget getSegmentedTexturedSeparatedToolbarIconsWidget() {
        // This field was introduced in release 10 of VAquaRendering
        try {
            return AquaUIPainter.SegmentedButtonWidget.BUTTON_SEGMENTED_TEXTURED_SEPARATED_TOOLBAR_ICONS;
        } catch (NoSuchFieldError e) {
            return null;
        }
    }
}
