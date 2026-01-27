/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.function.Consumer;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import org.jetbrains.annotations.*;

/**
 *
 */

public class AquaAppearancePanelUI
  extends AquaPanelUI
{
    public static @NotNull ComponentUI createUI(JComponent x)
    {
        return new AquaAppearancePanelUI();
    }

    @Override
    public void installUI(JComponent c)
    {
        super.installUI(c);
    }

    public void paint(@NotNull JComponent jc, @NotNull String appearanceName, @NotNull Consumer<Color> r)
    {
        AppearanceManager.paintAppearancePanel(jc, appearanceName, r);
    }
}
