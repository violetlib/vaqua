/*
 * Copyright (c) 2015-2026 Alan Snyder.
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

import org.jetbrains.annotations.*;
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

    public AquaViewportUI() {
        colors = AquaColors.CONTROL_COLORS;
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        viewport = (JViewport) c;
        viewport.setOpaque(false);  // needed in JDKs prior to 17 (see JDK-8253266)
        AquaVibrantSupport.installVibrantStyle(c);
        //AppearanceManager.install(c);
    }

    @Override
    public void uninstallUI(JComponent c) {
        //AppearanceManager.uninstall(c);
        AquaVibrantSupport.uninstallVibrantStyle(c);
        viewport = null;
        super.uninstallUI(c);
    }

    public boolean shouldSuppressBackground()
    {
        JComponent view = viewport != null ? (JComponent) viewport.getView() : null;
        return view != null && AquaVibrantSupport.isVibrant(view);
    }

    protected AquaUIPainter.State getState() {
        return AquaFocusHandler.isActive(viewport) ? AquaUIPainter.State.ACTIVE : AquaUIPainter.State.INACTIVE;
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
        AquaUIPainter.State state = getState();
        AppearanceContext appearanceContext = new AppearanceContext(pc.appearance, state, false, false);
        AquaColors.installColors(c, appearanceContext, colors);
        if (c.isOpaque() || AquaVibrantSupport.isVibrant(c)) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_VIBRANT);
        }
    }
}
