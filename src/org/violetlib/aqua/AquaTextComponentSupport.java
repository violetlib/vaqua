/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.HashSet;
import java.util.Set;
import javax.swing.*;
import javax.swing.plaf.basic.BasicTextUI;
import javax.swing.text.*;
import javax.swing.text.html.HTML;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**

 */

public class AquaTextComponentSupport {

    public static @NotNull View getRootView(@NotNull JTextComponent c) {
        BasicTextUI ui = AquaUtils.getUI(c, BasicTextUI.class);
        if (ui != null) {
            return ui.getRootView(c);
        }
        throw new IllegalArgumentException("Unsupported text component");
    }

    // The following method is copied from BasicTextUI
    @SuppressWarnings("deprecation")
    public static void updateFocusTraversalKeys(@NotNull JTextComponent editor, @Nullable EditorKit editorKit) {
        if (editorKit instanceof DefaultEditorKit) {
            Set<AWTKeyStroke> storedForwardTraversalKeys = editor.
                getFocusTraversalKeys(KeyboardFocusManager.
                                      FORWARD_TRAVERSAL_KEYS);
            Set<AWTKeyStroke> storedBackwardTraversalKeys = editor.
                getFocusTraversalKeys(KeyboardFocusManager.
                                      BACKWARD_TRAVERSAL_KEYS);
            Set<AWTKeyStroke> forwardTraversalKeys = new HashSet<AWTKeyStroke>(storedForwardTraversalKeys);
            Set<AWTKeyStroke> backwardTraversalKeys = new HashSet<AWTKeyStroke>(storedBackwardTraversalKeys);
            if (editor.isEditable()) {
                forwardTraversalKeys.
                    remove(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0));
                backwardTraversalKeys.
                    remove(KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
                                                  InputEvent.SHIFT_MASK));
            } else {
                forwardTraversalKeys.add(KeyStroke.
                                         getKeyStroke(KeyEvent.VK_TAB, 0));
                backwardTraversalKeys.
                    add(KeyStroke.
                        getKeyStroke(KeyEvent.VK_TAB, InputEvent.SHIFT_MASK));
            }
            LookAndFeel.installProperty(editor,
                                        "focusTraversalKeysForward",
                                         forwardTraversalKeys);
            LookAndFeel.installProperty(editor,
                                        "focusTraversalKeysBackward",
                                         backwardTraversalKeys);
        }
    }

    // The following method is copied from BasicHTML
    public static int getBaseline(View view, int w, int h) {
        if (hasParagraph(view)) {
            view.setSize(w, h);
            return getBaseline(view, new Rectangle(0, 0, w, h));
        }
        return -1;
    }

    private static int getBaseline(View view, Shape bounds) {
        if (view.getViewCount() == 0) {
            return -1;
        }
        AttributeSet attributes = view.getElement().getAttributes();
        Object name = null;
        if (attributes != null) {
            name = attributes.getAttribute(StyleConstants.NameAttribute);
        }
        int index = 0;
        if (name == HTML.Tag.HTML && view.getViewCount() > 1) {
            // For html on widgets the header is not visible, skip it.
            index++;
        }
        bounds = view.getChildAllocation(index, bounds);
        if (bounds == null) {
            return -1;
        }
        View child = view.getView(index);
        if (view instanceof javax.swing.text.ParagraphView) {
            Rectangle rect;
            if (bounds instanceof Rectangle) {
                rect = (Rectangle)bounds;
            }
            else {
                rect = bounds.getBounds();
            }
            return rect.y + (int)(rect.height * child.getAlignment(View.Y_AXIS));
        }
        return getBaseline(child, bounds);
    }

    private static boolean hasParagraph(View view) {
        if (view instanceof javax.swing.text.ParagraphView) {
            return true;
        }
        if (view.getViewCount() == 0) {
            return false;
        }
        AttributeSet attributes = view.getElement().getAttributes();
        Object name = null;
        if (attributes != null) {
            name = attributes.getAttribute(StyleConstants.NameAttribute);
        }
        int index = 0;
        if (name == HTML.Tag.HTML && view.getViewCount() > 1) {
            // For html on widgets the header is not visible, skip it.
            index = 1;
        }
        return hasParagraph(view.getView(index));
    }
}
