/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.NotNull;

/**
 * Supports UI behavior specific to JTextPane.
 */

public class AquaTextPaneUIDelegate extends AquaEditorPaneUIDelegate {

    @Override
    public @NotNull String getPropertyPrefix() {
        return "TextPane";
    }
}
