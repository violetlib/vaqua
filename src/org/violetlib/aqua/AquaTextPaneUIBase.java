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
 * A base class for multiple-line text component UIs.
 */
public class AquaTextPaneUIBase extends AquaTextComponentUIBase {

    public AquaTextPaneUIBase(@NotNull AquaTextComponentUIDelegate delegate) {
        super(delegate);
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        AquaKeyBindings bindings = AquaKeyBindings.instance();
        bindings.installAquaUpDownActions(editor);
    }
}
