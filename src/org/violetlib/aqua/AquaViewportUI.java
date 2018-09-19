/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ViewportUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * The viewport UI supports vibrant backgrounds and appearances.
 */
public class AquaViewportUI extends ViewportUI implements AquaComponentUI {

    public static ComponentUI createUI(JComponent c) {
        return new AquaViewportUI();
    }

    protected @Nullable JViewport viewport;
    protected final @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    public AquaViewportUI() {
        colors = AquaColors.CONTROL_COLORS;
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        viewport = (JViewport) c;
        AquaVibrantSupport.installVibrantStyle(c);
        AppearanceManager.installListener(c);
        configureAppearanceContext(null);
    }

    @Override
    public void uninstallUI(JComponent c) {
        AppearanceManager.uninstallListener(c);
        AquaVibrantSupport.uninstallVibrantStyle(c);
        viewport = null;
        super.uninstallUI(c);
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
        assert viewport != null;
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(viewport);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        AquaColors.installColors(viewport, appearanceContext, colors);
        viewport.repaint();
    }

    protected AquaUIPainter.State getState() {
        return AquaFocusHandler.isActive(viewport) ? AquaUIPainter.State.ACTIVE : AquaUIPainter.State.INACTIVE;
    }

    @Override
    public final void update(@NotNull Graphics g, @NotNull JComponent c) {

        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);

        if (c.isOpaque() || AquaVibrantSupport.isVibrant(c)) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_VIBRANT);
        }

        paint(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }
}
