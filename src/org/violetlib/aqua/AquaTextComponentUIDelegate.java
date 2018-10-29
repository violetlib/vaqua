/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.View;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The interface supported by a text component UI delegate. The UI delegate supports the custom behaviors for a
 * specific type of text component.
 */
public interface AquaTextComponentUIDelegate {

    @NotNull String getPropertyPrefix();

    void install(@NotNull JTextComponent c);

    void uninstall(@NotNull JTextComponent c);

    /**
     * Respond to a property change on the text component.
     * @return true if the model should be updated.
     */
    boolean propertyChange(@NotNull PropertyChangeEvent ev);

    @Nullable EditorKit getEditorKit(@NotNull JTextComponent c);

    @Nullable View create(@NotNull JTextComponent c,  Element elem);

    int getBaseline(@NotNull JTextComponent c, int width, int height);

    @NotNull Component.BaselineResizeBehavior getBaselineResizeBehavior(JTextComponent c);

    @Nullable ActionMap getActionMap(@NotNull JTextComponent c);
}
