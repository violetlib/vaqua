/*
 * Copyright (c) 2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Support for vibrant effects. An NSVisualEffectView is created for the menu background and for the selected menu item
 * background region.
 */

public class MenuVibrantEffects extends VisualEffectView {
    protected @Nullable MenuSelectionBoundsTracker bt;
    protected final int style;

    public MenuVibrantEffects(@NotNull JComponent top, @NotNull JPopupMenu menu, int style) {
        super(top, style, true);

        this.style = style;
        bt = new MenuSelectionBoundsTracker(menu, this::updateSelectionBackgrounds) {
            @Override
            protected int convertRowYCoordinateToSelectionDescription(int y) {
                if (top != menu) {
                    Point p = SwingUtilities.convertPoint(menu, 0, y, top);
                    return p.y;
                } else {
                    return y;
                }
            }
        };
        menu.putClientProperty(AquaPopupMenuUI.POP_UP_TRACKER, bt);

        //menu.getSelectionModel().addChangeListener(e -> update());
    }

    public void update() {
        if (bt != null) {
            bt.update();
        }
    }

    public void dispose() {
        super.dispose();
        if (bt != null) {
            bt.dispose();
            bt = null;
        }
    }

    @Override
    protected void windowChanged(Window newWindow) {
        super.windowChanged(newWindow);
        if (bt != null) {
            bt.reset();
        }
    }
}
