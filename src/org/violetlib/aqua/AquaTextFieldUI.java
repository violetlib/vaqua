/*
 * Changes Copyright (c) 2015-2018 Alan Snyder.
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
import java.awt.event.FocusEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;

public class AquaTextFieldUI extends AquaTextComponentUIBase implements ToolbarSensitiveUI {

    public static final String TEXT_FIELD_STYLE_KEY = "JTextField.style";
    public static final String TEXT_FIELD_VARIANT_KEY = "JTextField.variant";   // legacy from Aqua LAF
    public static final String QUAQUA_TEXT_FIELD_STYLE_KEY = "Quaqua.TextField.style";

    public static @NotNull ComponentUI createUI(@NotNull JComponent c) {
        return new AquaTextFieldUI();
    }

    protected HierarchyListener hierarchyListener;
    protected View topView;
    protected boolean isToolbar;

    public AquaTextFieldUI() {
        super(new AquaTextFieldUIDelegate());
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        isToolbar = AquaUtils.isOnToolbar(c);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        installBorder();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        hierarchyListener = new AquaHierarchyListener();
        editor.addHierarchyListener(hierarchyListener);
        AquaUtils.installToolbarSensitivity(editor);
    }

    @Override
    protected void uninstallListeners() {
        AquaUtils.uninstallToolbarSensitivity(editor);
        editor.removeHierarchyListener(hierarchyListener);
        hierarchyListener = null;
        super.uninstallListeners();
    }

    protected void installBorder() {
        Border b = editor.getBorder();
        if ((b == null) || (b instanceof UIResource)) {
            editor.setBorder(new AquaTextComponentBorder(editor));
        }
    }

    @Override
    protected @NotNull AquaFocusHandler createFocusHandler() {
        return new AquaTextFieldFocusHandler();
    }

    protected class AquaTextFieldFocusHandler extends AquaFocusHandler {
        @Override
        public void focusLost(@NotNull FocusEvent ev) {
            super.focusLost(ev);
            if (!ev.isTemporary()) {
                Caret c = editor.getCaret();
                if (c != null) {
                    c.setDot(0);
                }
            }
        }
    }

    @Override
    protected void propertyChange(@NotNull PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String prop = evt.getPropertyName();
        if (isStyleProperty(prop) || AquaCellEditorPolicy.isCellEditorProperty(prop)) {
            updateStyle();
        } else if (AquaColors.COMPONENT_COLORS_KEY.equals(prop)) {
            Object o = evt.getNewValue();
            if (o instanceof BasicContextualColors) {
                colors = (BasicContextualColors) o;
                configureAppearanceContext(null);
            }
        }
    }

    protected class AquaHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            // A change in the hierarchy may change the "cell editor" status of the text field.
            if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0) {
                updateStyle();
            }
        }
    }

    @Override
    public void toolbarStatusChanged(@NotNull JComponent c) {
        boolean b = AquaUtils.isOnToolbar(c);
        if (b != isToolbar) {
            isToolbar = b;
            c.revalidate();
            c.repaint();
        }
    }

    public void updateStyle() {
        if (wantsToBeASearchField()) {
            AquaTextFieldSearch.installSearchField(editor);
        } else {
            AquaTextFieldSearch.uninstallSearchField(editor);
        }

        if (topView instanceof AquaMarginView) {
            AquaMarginView v = (AquaMarginView) topView;
            int textMargin = getTextMargin();
            v.setMargin(textMargin);
        }

        editor.revalidate();
        editor.repaint();
    }

    protected boolean wantsToBeASearchField() {
        String style = getStyleProperty();
        return "search".equals(style);
    }

    protected boolean isStyleProperty(String prop) {
        return AquaUtils.isProperty(prop, TEXT_FIELD_STYLE_KEY, TEXT_FIELD_VARIANT_KEY, QUAQUA_TEXT_FIELD_STYLE_KEY);
    }

    protected @Nullable String getStyleProperty() {
        return AquaUtils.getProperty(editor, TEXT_FIELD_STYLE_KEY, TEXT_FIELD_VARIANT_KEY, QUAQUA_TEXT_FIELD_STYLE_KEY);
    }

    @Override
    public final @Nullable View create(@NotNull Element elem) {
        return topView = createView(elem);
    }

    private @Nullable View createView(@NotNull Element elem) {
        View base = createBasicView(elem);
        int textMargin = getTextMargin();
        if (textMargin > 0) {
            if (base instanceof FieldView) {
                return new AquaMarginView(base, textMargin);
            }
            // TBD: support the complex layout cases: GlyphView and I18nFieldView
        }
        return base;
    }

    protected @Nullable View createBasicView(@NotNull Element elem) {
        View view = delegate.create(editor, elem);
        if (view != null) {
            Class c = view.getClass();
            if (c == FieldView.class) {
                return new AquaFieldView(elem);
            }
        }
        return view;
    }

    /**
     * Return the default text margin. The text margin is like a border margin, except that it is part of the text
     * display and therefore scrolls left and right along with the text. The result is that the full width of the text
     * field (exclusive of the actual border margin) is available for editing.
     */
    public int getTextMargin() {
        Border b = editor.getBorder();
        if (b instanceof AquaTextComponentBorder) {
            AquaTextComponentBorder bb = (AquaTextComponentBorder) b;
            return bb.getTextMargin();
        }
        return 0;
    }

    @Override
    public Dimension getMaximumSize(JComponent c) {
        return getLayoutSize(LayoutOption.MAXIMUM);
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        return getLayoutSize(LayoutOption.PREFERRED);
    }

    @Override
    public Dimension getMinimumSize(JComponent c) {
        return getLayoutSize(LayoutOption.MINIMUM);
    }

    protected Dimension getLayoutSize(LayoutOption opt) {
        Dimension size = getLayoutSizeFromText(opt);
        Border b = editor.getBorder();
        if (b instanceof AquaTextComponentBorder) {
            AquaTextComponentBorder tb = (AquaTextComponentBorder) b;
            LayoutInfo info = tb.getLayoutInfo();
            int width = (int) Math.max(size.width, info.getMinimumVisualWidth());
            int height = (int) Math.max(size.height, info.getMinimumVisualHeight());
            return new Dimension(width, height);
        }
        return size;
    }

    protected Dimension getLayoutSizeFromText(LayoutOption opt) {
        // The text size includes the margin
        Dimension td = getTextSize(opt);
        int textWidth = td.width;
        int textHeight = td.height;
        Border b = editor.getBorder();
        if (b instanceof AquaTextComponentBorder) {
            // Our painter can tell us the minimum size
            // For the preferred size and maximum size, leave some extra room at the top and bottom
            AquaTextComponentBorder tb = (AquaTextComponentBorder) b;
            Insetter insets = tb.getTextInsets();
            if (insets != null) {
                int extraHeight = opt != LayoutOption.MINIMUM ? tb.getExtraHeight() : 0;
                Dimension size = insets.expand(new Dimension(textWidth, textHeight + extraHeight));
                return size;
            }
        }
        Insets insets = editor.getInsets();
        return new Dimension(textWidth +  insets.left + insets.right, textHeight + insets.top + insets.bottom);
    }

    protected enum LayoutOption { MINIMUM, PREFERRED, MAXIMUM }

    protected Dimension getTextSize(LayoutOption opt) {
        Dimension d = new Dimension();
        if (topView != null) {
            Document doc = editor.getDocument();
            if (doc instanceof AbstractDocument) {
                ((AbstractDocument)doc).readLock();
            }

            if (opt == LayoutOption.PREFERRED) {
                Dimension size = editor.getSize();
                if (size.width == 0 && size.height == 0) {
                    // Probably haven't been layed out yet, force some sort of initial sizing.
                    topView.setSize(Integer.MAX_VALUE, Integer.MAX_VALUE);
                } else {
                    Insets s = editor.getInsets();
                    if ((size.width > (s.left + s.right)) && (size.height > (s.top + s.bottom))) {
                        topView.setSize(size.width - s.left - s.right, size.height - s.top - s.bottom);
                    }
                }
            }

            try {
                float width = 0;
                float height = 0;

                switch (opt) {
                    case MINIMUM:
                        width = topView.getMinimumSpan(View.X_AXIS);
                        height = topView.getMinimumSpan(View.Y_AXIS);
                        break;
                    case PREFERRED:
                        width = topView.getPreferredSpan(View.X_AXIS);
                        height = topView.getPreferredSpan(View.Y_AXIS);
                        break;
                    case MAXIMUM:
                        width = topView.getMaximumSpan(View.X_AXIS);
                        height = topView.getMaximumSpan(View.Y_AXIS);
                }

                // increase the width by 3 pixels
                // 1 pixel for the caret
                // 2 pixels because the width calculation appears to be incorrect
                // the goal is no need for horizontal scrolling when the editor is sized at the preferred size
                d.width = (int) Math.min(100000, width + 3);
                d.height = (int) Math.min(100000, height);
            } finally {
                if (doc instanceof AbstractDocument) {
                    ((AbstractDocument)doc).readUnlock();
                }
            }
        }

        return d;
    }

    @Override
    protected void paintBackgroundSafely(@NotNull Graphics g, @Nullable Color background) {
        int width = editor.getWidth();
        int height = editor.getHeight();

        Border b = editor.getBorder();

        if (!(b instanceof AquaTextComponentBorder)) {
            // developer must have set a custom border

            // The effect of this code is to make isOpaque=true the default when a custom border is used.
            // This code comes from Aqua LAF.
            // TBD: why is this a good idea?

            boolean isOpaque = editor.isOpaque();
            if (!isOpaque && JavaSupport.hasOpaqueBeenExplicitlySet(editor)) {
                return;
            }

            // must fill whole region with background color if opaque
            if (background != null) {
                g.setColor(background);
                g.fillRect(0, 0, width, height);
            }
            return;
        }

        // using our own border
        AquaTextComponentBorder tb = (AquaTextComponentBorder) b;
        tb.paintBackground(editor, g, background);
    }
}
