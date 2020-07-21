/*
 * Changes Copyright (c) 2015-2020 Alan Snyder.
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

import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.Position;
import org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

import static org.violetlib.jnr.aqua.SegmentedButtonConfiguration.DividerState;

/**
 * A border for a button that is painted by a native painter configured using a segmented button widget. The native
 * painter defines the border insets, which capture the visual size of the border.
 */
public class AquaSegmentedButtonBorder extends AquaButtonBorder implements FocusRingOutlineProvider {

    protected final @NotNull SegmentedButtonWidget widget;
    protected final @NotNull AquaButtonExtendedTypes.WidgetInfo info;
    protected final @NotNull Position position;

    /**
     * Create a border for a segmented button. The widget is provisional. The widget may be superseded in some cases
     * based on the group membership of the button at the time the widget is used.
     */
    public AquaSegmentedButtonBorder(@NotNull SegmentedButtonWidget widget,
                                     @NotNull AquaButtonExtendedTypes.WidgetInfo info,
                                     @NotNull Position position) {
        this.widget = widget;
        this.info = info;
        this.position = position;
    }

    @Override
    public @NotNull SegmentedButtonWidget getButtonWidget(@NotNull AbstractButton b) {
        AquaButtonUI ui = AquaUtils.getUI(b, AquaButtonUI.class);
        if (ui != null && ui.isGroupMember(b)) {
            return getWidgetForGroupMember(b);
        }
        return widget;
    }

    private @NotNull SegmentedButtonWidget getWidgetForGroupMember(@NotNull AbstractButton b) {
        // Special case for exclusive rounded buttons starting in macOS 11
        if (OSXSystemProperties.OSVersion >= 1016 && widget == SegmentedButtonWidget.BUTTON_SEGMENTED) {
            SegmentedButtonWidget w = getSegmentedSliderWidget();
            if (w != null) {
                return w;
            }
        }
        return widget;
    }

    @Override
    protected @NotNull AquaButtonExtendedTypes.WidgetInfo getWidgetInfo(@NotNull AbstractButton b) {
        return info;
    }

    @Override
    public @Nullable GenericButtonConfiguration getConfiguration(@NotNull AbstractButton b, int width, int height) {
        SegmentedButtonLayoutConfiguration g = (SegmentedButtonLayoutConfiguration) getLayoutConfiguration(b);
        if (g == null) {
            // should not happen
            return null;
        }

        ButtonModel model = b.getModel();
        State state = getState(b);
        boolean isFocused = (state != State.DISABLED && state != State.INACTIVE && state != State.DISABLED_INACTIVE)
                && b.isFocusPainted() && b.hasFocus();

        AquaButtonExtendedTypes.WidgetInfo info = getWidgetInfo(b);

        boolean isSelected = model.isSelected();
        if (isSelected) {
            // Special case for nonexclusive selected textured segmented buttons. Use the same background as the
            // non-selected button.
            if (info.isTextured()) {
                boolean useNonexclusive = shouldUseNonexclusiveStyle(b, info);
                if (useNonexclusive) {
                    isSelected = false;
                }
            }
        }

        SegmentedButtonWidget widget = getButtonWidget(b);

        // Special case for exclusive textured segmented buttons in light mode on 10.14: make them active-insensitive.
        // This is not what macOS 10.14 actually does, but it is much more readable, and it is similar to what macOS
        // 10.15 does, which is to make all textured segmented buttons active-insensitive.

        if (OSXSystemProperties.OSVersion == 1014
                && state.isInactive()
                && widget.isTextured()
                && !AppearanceManager.getAppearance(b).isDark()
                && isButtonInGroup(b)) {
            state = state.toActive();
        }

        // Swing does not know about segmented buttons. They are just buttons that happen to be arranged in a row and
        // (hopefully) configured properly. Therefore, we cannot determine anything about the button to the left or
        // right.

        Size sz = g.getSize();
        AquaUIPainter.Direction d = AquaUIPainter.Direction.NONE;
        Position pos = g.getPosition();
        DividerState leftState = AquaSegmentedButtonBorder.getDividerState(false, false);
        DividerState rightState = AquaSegmentedButtonBorder.getDividerState(pos == Position.FIRST || pos == Position.MIDDLE, false);
        return new SegmentedButtonConfiguration(widget, sz, state, isSelected, isFocused, d, pos, leftState, rightState);
    }

    private @Nullable SegmentedButtonWidget getSegmentedSliderWidget() {
        // This field was introduced in release 10 of VAquaRendering
        try {
            return SegmentedButtonWidget.BUTTON_SEGMENTED_SLIDER;
        } catch (NoSuchFieldError e) {
            return null;
        }
    }

    @Override
    public @NotNull SegmentedButtonLayoutConfiguration determineLayoutConfiguration(@NotNull AbstractButton b) {
        SegmentedButtonWidget widget = getButtonWidget(b);
        Size size = AquaUtilControlSize.getUserSizeFrom(b);
        return new SegmentedButtonLayoutConfiguration(widget, size, position);
    }

    public static SegmentedButtonConfiguration.DividerState getDividerState(boolean isPainted, boolean isSelected) {
        if (!isPainted) {
            return SegmentedButtonConfiguration.DividerState.NONE;
        }
        return isSelected
                ? SegmentedButtonConfiguration.DividerState.SELECTED
                : SegmentedButtonConfiguration.DividerState.ORDINARY;
    }
}
