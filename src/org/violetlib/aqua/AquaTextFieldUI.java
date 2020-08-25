/*
 * Changes Copyright (c) 2015 Alan Snyder.
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
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicTextFieldUI;
import javax.swing.text.*;

import org.violetlib.aqua.AquaUtils.JComponentPainter;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insetter;

public class AquaTextFieldUI extends BasicTextFieldUI implements FocusRingOutlineProvider {

    public static final String TEXT_FIELD_STYLE_KEY = "JTextField.style";
    public static final String TEXT_FIELD_VARIANT_KEY = "JTextField.variant";   // legacy from Aqua LAF
    public static final String QUAQUA_TEXT_FIELD_STYLE_KEY = "Quaqua.TextField.style";

    public static ComponentUI createUI(final JComponent c) {
        return new AquaTextFieldUI();
    }

    protected JComponentPainter delegate;
    protected AquaFocusHandler handler;
    protected PropertyChangeListener propertyChangeListener;
    protected HierarchyListener hierarchyListener;

    protected JTextComponent editor;
    protected View topView;

    @Override
    public void installUI(JComponent c) {
        if (c instanceof JTextComponent) {
            editor = (JTextComponent) c;
            super.installUI(c);
        }
    }

    @Override
    public void uninstallUI(JComponent c) {
        super.uninstallUI(c);
        editor = null;
    }

    protected void installListeners() {
        super.installListeners();

        handler = new AquaTextFieldFocusHandler();
        editor.addFocusListener(handler);
        editor.addPropertyChangeListener(handler);

        propertyChangeListener = new AquaPropertyChangeHandler();
        editor.addPropertyChangeListener(propertyChangeListener);

        hierarchyListener = new AquaHierarchyListener();
        editor.addHierarchyListener(hierarchyListener);

        LookAndFeel.installProperty(editor, "opaque", UIManager.getBoolean(getPropertyPrefix() + "opaque"));
        AquaUtilControlSize.addSizePropertyListener(editor);
    }

    protected void uninstallListeners() {
        AquaUtilControlSize.removeSizePropertyListener(editor);
        editor.removeFocusListener(handler);
        editor.removePropertyChangeListener(handler);
        editor.removePropertyChangeListener(propertyChangeListener);
        editor.removeHierarchyListener(hierarchyListener);
        handler = null;

        super.uninstallListeners();
    }

    protected class AquaTextFieldFocusHandler extends AquaFocusHandler {
        @Override
        public void focusLost(FocusEvent ev) {
            super.focusLost(ev);
            if (!ev.isTemporary()) {
                Caret c = editor.getCaret();
                if (c != null) {
                    c.setDot(0);
                }
            }
        }
    }

    protected class AquaPropertyChangeHandler implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent e) {
            final String prop = e.getPropertyName();
            if (isStyleProperty(prop) || AquaCellEditorPolicy.isCellEditorProperty(prop)) {
                updateStyle();
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

    protected String getStyleProperty() {
        return AquaUtils.getProperty(editor, TEXT_FIELD_STYLE_KEY, TEXT_FIELD_VARIANT_KEY, QUAQUA_TEXT_FIELD_STYLE_KEY);
    }

    boolean oldDragState = false;
    protected void installDefaults() {
        if (!GraphicsEnvironment.isHeadless()) {
            oldDragState = editor.getDragEnabled();
            editor.setDragEnabled(true);
        }

        super.installDefaults();
    }

    protected void uninstallDefaults() {
        super.uninstallDefaults();

        if (!GraphicsEnvironment.isHeadless()) {
            getComponent().setDragEnabled(oldDragState);
        }
    }

    // Install a default keypress action which handles Cmd and Option keys properly
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        AquaKeyBindings.instance().setDefaultAction(getKeymapName());
    }

    @Override
    public final View create(Element elem) {
        return topView = createView(elem);
    }

    private View createView(Element elem) {
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

    protected View createBasicView(Element elem) {
        View view = super.create(elem);
        Class c = view.getClass();
        if (c == FieldView.class) {
            return new AquaFieldView(elem);
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
        if (b instanceof AquaTextFieldBorder) {
            AquaTextFieldBorder bb = (AquaTextFieldBorder) b;
            return bb.getTextMargin(editor);
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
        // The text size includes the margin
        Dimension td = getTextSize(opt);
        int textWidth = td.width;
        int textHeight = td.height;
        Border b = editor.getBorder();
        if (b instanceof AquaTextFieldBorder) {
            // Our painter can tell us the minimum size
            // For the preferred size and maximum size, leave some extra room at the top and bottom
            AquaTextFieldBorder tb = (AquaTextFieldBorder) b;
            Insetter insets = tb.getTextInsets(editor);
            if (insets != null) {
                int extraHeight = opt != LayoutOption.MINIMUM ? tb.getExtraHeight(editor) : 0;
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

                d.width = (int) Math.min(100000, width + 1);   // leave extra room for the caret to avoid scrolling
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
    public Shape getFocusRingOutline(JComponent c) {
        Border b = c.getBorder();
        if (b instanceof FocusRingOutlineProvider) {
            FocusRingOutlineProvider p = (FocusRingOutlineProvider) b;
            return p.getFocusRingOutline(c);
        }
        return new Rectangle(0, 0, c.getWidth(), c.getHeight());
    }

    protected void paintSafely(final Graphics g) {
        paintBackgroundSafely(g);

        // If the painter has specified a non-integer top inset, attempt to support that by translation.
        // This works with AquaTextFieldBorder, which will have rounded down the top inset.

        if (g instanceof Graphics2D) {
            Border b = editor.getBorder();
            if (b instanceof Border2D) {
                Border2D ab = (Border2D) b;
                Insets2D n = ab.getBorderInsets2D(editor);
                if (n != null) {
                    float top = n.getTop();
                    float floor = (float) Math.floor(top);
                    if (top - floor > 0.001f) {
                        Graphics2D gg = (Graphics2D) g.create();
                        gg.translate(0, top - floor);
                        super.paintSafely(gg);
                        gg.dispose();
                        return;
                    }
                }
            }
        }

        super.paintSafely(g);
    }

    protected void paintBackgroundSafely(final Graphics g) {
        final int width = editor.getWidth();
        final int height = editor.getHeight();

        // a delegate takes precedence
        if (delegate != null) {
            delegate.paint(editor, g, 0, 0, width, height);
            return;
        }

        final boolean isOpaque = editor.isOpaque();
        Border b = editor.getBorder();

        if (!(b instanceof AquaTextFieldBorder)) {
            // developer must have set a custom border
            if (!isOpaque && AquaUtils.hasOpaqueBeenExplicitlySet(editor)) return;

            // must fill whole region with background color if opaque
            g.setColor(editor.getBackground());
            g.fillRect(0, 0, width, height);
            return;
        }

        // using our own border
        AquaTextFieldBorder tb = (AquaTextFieldBorder) b;
        tb.paintBackground(editor, g);
//
//
//
//        final Insets margin = c.getMargin();
//        Insets insets = c.getInsets();
//
//        if (insets == null) insets = new Insets(0, 0, 0, 0);
//        if (margin != null) {
//            insets.top -= margin.top;
//            insets.left -= margin.left;
//            insets.bottom -= margin.bottom;
//            insets.right -= margin.right;
//        }
//
//        // the common case
//        final int shrinkage = AquaTextFieldBorder.getShrinkageFor(c, height);
//        g.fillRect(insets.left - 2, insets.top - shrinkage - 1, width - insets.right - insets.left + 4, height - insets.bottom - insets.top + shrinkage * 2 + 2);
    }

    protected void paintBackground(final Graphics g) {
        // we have already ensured that the background is painted to our liking
        // by paintBackgroundSafely(), called from paintSafely().
    }

    protected Caret createCaret() {
        final Window owningWindow = SwingUtilities.getWindowAncestor(editor);
        return new AquaCaret(owningWindow, editor);
    }

    protected Highlighter createHighlighter() {
        return new AquaHighlighter();
    }

    protected void setPaintingDelegate(final JComponentPainter delegate) {
        this.delegate = delegate;
    }
}
