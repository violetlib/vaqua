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

import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Position;
import org.violetlib.jnr.aqua.AquaUIPainter.SegmentedButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.Configuration;
import org.violetlib.jnr.aqua.SegmentedButtonConfiguration;
import org.violetlib.jnr.aqua.SegmentedButtonLayoutConfiguration;

import javax.swing.*;

import static org.violetlib.jnr.aqua.SegmentedButtonConfiguration.DividerState;

/**
 * A border for a button that is painted by a native painter configured using a segmented button widget. The native
 * painter defines the border insets, which capture the visual size of the border.
 */
public class AquaSegmentedButtonBorder extends AquaButtonBorder implements FocusRingOutlineProvider {

    protected final SegmentedButtonWidget widget;
    protected final AquaButtonExtendedTypes.WidgetInfo info;
    protected final Position position;

    public AquaSegmentedButtonBorder(SegmentedButtonWidget widget, AquaButtonExtendedTypes.WidgetInfo info, Position position) {
        this.widget = widget;
        this.info = info;
        this.position = position;
    }

    @Override
    public SegmentedButtonWidget getButtonWidget(AbstractButton b) {
        return widget;
    }

    @Override
    protected AquaButtonExtendedTypes.WidgetInfo getWidgetInfo(AbstractButton b) {
        return info;
    }

    @Override
    public Configuration getConfiguration(AbstractButton b, int width, int height) {
        SegmentedButtonLayoutConfiguration g = (SegmentedButtonLayoutConfiguration) getLayoutConfiguration(b);
        if (g == null) {
            // should not happen
            return null;
        }

        ButtonModel model = b.getModel();
        final State state = getState(b);
        boolean isFocused = (state != State.DISABLED && state != State.INACTIVE && state != State.DISABLED_INACTIVE) && b.isFocusPainted() && b.hasFocus();

        // Swing does not know about segmented buttons. They are just buttons that happen to be arranged in a row and
        // (hopefully) configured properly. Therefore, we cannot determine anything about the button to the left or
        // right.

        boolean isSelected = model.isSelected();
        AquaUIPainter.Direction d = AquaUIPainter.Direction.NONE;
        DividerState leftState = AquaSegmentedButtonBorder.getDividerState(false, false);
        DividerState rightState = AquaSegmentedButtonBorder.getDividerState(true, false);
        return new SegmentedButtonConfiguration(g, state, isSelected, isFocused, d, leftState, rightState);
    }

    @Override
    public SegmentedButtonLayoutConfiguration determineLayoutConfiguration(AbstractButton b) {
        Size size = AquaUtilControlSize.getUserSizeFrom(b);
        return new SegmentedButtonLayoutConfiguration(widget, size, position);
    }

    public static SegmentedButtonConfiguration.DividerState getDividerState(boolean isPainted, boolean isSelected) {
        if (!isPainted) {
            return SegmentedButtonConfiguration.DividerState.NONE;
        }
        return isSelected ? SegmentedButtonConfiguration.DividerState.SELECTED : SegmentedButtonConfiguration.DividerState.ORDINARY;
    }
}
