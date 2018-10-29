/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.plaf.basic.BasicTextUI;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.View;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A base class for implementing a text component UI using a delegate to provide the custom behavior for a specific text
 * component class. This approach is designed to support generic LAF customization of text component UIs.
 */
public class AquaTextComponentDelegatedUIBase extends BasicTextUI {

    // This restructuring requires some copying of code from the basic LAF.
    // The only tricky part is supporting a custom action map for JEditorPane, as this uses a non-exported
    // overridden method in BasicTextUI.

    protected final @NotNull AquaTextComponentUIDelegate delegate;
    private final @NotNull String propertyPrefix;

    protected JTextComponent editor;

    protected AquaTextComponentDelegatedUIBase(@NotNull AquaTextComponentUIDelegate delegate) {
        this.delegate = delegate;
        this.propertyPrefix = delegate.getPropertyPrefix();
    }

    @Override
    protected @NotNull String getPropertyPrefix() {
        return propertyPrefix;
    }

    @Override
    public void installUI(@NotNull JComponent c) {
        if (c instanceof JTextComponent) {
            editor = (JTextComponent) c;
            super.installUI(c);
            delegate.install(editor);
        }
    }

    @Override
    public void uninstallUI(@NotNull JComponent c) {
        delegate.uninstall((JTextComponent) c);
        super.uninstallUI(c);
        editor = null;
    }

    @Override
    public @NotNull EditorKit getEditorKit(@NotNull JTextComponent tc) {
        EditorKit kit = delegate.getEditorKit(tc);
        return kit != null ? kit : super.getEditorKit(tc);
    }

    @Override
    public @Nullable View create(@NotNull Element elem) {
        return delegate.create(editor, elem);
    }

    @Override
    public int getBaseline(@NotNull JComponent c, int width, int height) {
        super.getBaseline(c, width, height);  // for error checking
        return delegate.getBaseline((JTextComponent) c, width, height);
    }

    @Override
    public @NotNull Component.BaselineResizeBehavior getBaselineResizeBehavior(@NotNull JComponent c) {
        super.getBaselineResizeBehavior(c);  // for error checking
        return delegate.getBaselineResizeBehavior((JTextComponent) c);
    }

    @Override
    protected void propertyChange(@NotNull PropertyChangeEvent evt) {
        super.propertyChange(evt);

        if (delegate.propertyChange(evt)) {
            modelChanged();
        }

        String prop = evt.getPropertyName();
        if ("focusAccelerator".equals(prop)) {
            // In BasicTextUI, this property change calls a private method updateFocusAcceleratorBinding, which may
            // update the action map (if the component is a JEditorPane). Because the order of property change listener
            // invocation is not specified, use invokeLater to ensure that our update of the action map happens last.
            SwingUtilities.invokeLater(() -> updateActionMap(editor));
        }
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();

        // In BasicTextUI, a static method is used to construct the default action map. If the component is a
        // JEditorPane, a custom action map is used instead.
        updateActionMap(editor);
    }

    private void updateActionMap(@NotNull JTextComponent c) {
        // Install a custom action map, if the delegate provides one.
        ActionMap map = delegate.getActionMap(c);
        if (map != null) {
            SwingUtilities.replaceUIActionMap(editor, map);
        }
    }
}
