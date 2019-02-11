/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import org.jetbrains.annotations.NotNull;

/**
 * The UI for JTextPane.
 */
public class AquaTextPaneUI extends AquaTextPaneUIBase {

    public static @NotNull ComponentUI createUI(@NotNull JComponent c) {
        return new AquaTextPaneUI();
    }

    public AquaTextPaneUI() {
        super(new AquaTextPaneUIDelegate());
    }
}
