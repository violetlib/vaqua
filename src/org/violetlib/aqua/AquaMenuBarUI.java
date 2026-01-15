/*
 * Copyright (c) 2018-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuBarUI;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Support for menu bars that are not using the screen menu bar.
 */
public class AquaMenuBarUI extends BasicMenuBarUI implements AquaComponentUI {

    public static ComponentUI createUI(JComponent c) {
        return new AquaMenuBarUI();
    }

    protected @NotNull BasicContextualColors colors;

    public AquaMenuBarUI() {
        colors = AquaColors.CONTROL_COLORS;
    }

    protected void installDefaults() {
        super.installDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        AppearanceManager.install(menuBar);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstall(menuBar);
        super.uninstallListeners();
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceManager.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {
        AquaUIPainter.State state = AquaUIPainter.State.ACTIVE;
        AppearanceContext appearanceContext = new AppearanceContext(pc.appearance, state, false, false);
        AquaColors.installColors(menuBar, appearanceContext, colors);
    }
}
