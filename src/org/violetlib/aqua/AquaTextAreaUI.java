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
 * The UI for JTextArea.
 */
public class AquaTextAreaUI extends AquaTextPaneUIBase {

    public static @NotNull ComponentUI createUI(@NotNull JComponent c) {
        return new AquaTextAreaUI();
    }

    public AquaTextAreaUI() {
        super(new AquaTextAreaUIDelegate());
    }
}
