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
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuBarUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Support for menu bars that are not using the screen menu bar.
 */
public class AquaMenuBarUI extends BasicMenuBarUI implements AquaComponentUI {

    public static ComponentUI createUI(JComponent c) {
        return new AquaMenuBarUI();
    }

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    public AquaMenuBarUI() {
        colors = AquaColors.CONTROL_COLORS;
    }

    protected void installDefaults() {
         super.installDefaults();
         configureAppearanceContext(null);
     }

    @Override
    protected void installListeners() {
        super.installListeners();
        AppearanceManager.installListeners(menuBar);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListeners(menuBar);
        super.uninstallListeners();
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        configureAppearanceContext(null);
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(menuBar);
        }
        AquaUIPainter.State state = AquaUIPainter.State.ACTIVE;
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        AquaColors.installColors(menuBar, appearanceContext, colors);
        menuBar.repaint();
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
    }
}
