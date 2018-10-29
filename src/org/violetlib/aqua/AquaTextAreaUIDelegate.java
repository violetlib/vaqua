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
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.text.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Supports UI behavior specific to JTextArea. This code copied from BasicTextAreaUI.
 */

public class AquaTextAreaUIDelegate implements AquaTextComponentUIDelegate {

    @Override
    public @NotNull String getPropertyPrefix() {
        return "TextArea";
    }

    @Override
    public void install(@NotNull JTextComponent c) {
    }

    @Override
    public void uninstall(@NotNull JTextComponent c) {
    }

    @Override
    public boolean propertyChange(@NotNull PropertyChangeEvent evt) {
        JTextComponent c = (JTextComponent) evt.getSource();
        if (evt.getPropertyName().equals("lineWrap") ||
            evt.getPropertyName().equals("wrapStyleWord") ||
                evt.getPropertyName().equals("tabSize")) {
            // rebuild the view
            return true;
        } else if ("editable".equals(evt.getPropertyName())) {
            AquaTextComponentSupport.updateFocusTraversalKeys(c, null);
        }
        return false;
    }

    @Override
    public @Nullable ActionMap getActionMap(@NotNull JTextComponent c) {
        return null;
    }

    @Override
    public @Nullable EditorKit getEditorKit(@NotNull JTextComponent c) {
        return null;
    }

    @Override
    public @Nullable View create(@NotNull JTextComponent c, @NotNull Element elem) {
        Document doc = elem.getDocument();
        Object i18nFlag = doc.getProperty("i18n"/*AbstractDocument.I18NProperty*/);
        if ((i18nFlag != null) && i18nFlag.equals(Boolean.TRUE)) {
            // build a view that support bidi
            return createI18N(elem);
        } else {
            if (c instanceof JTextArea) {
                JTextArea area = (JTextArea) c;
                View v;
                if (area.getLineWrap()) {
                    v = new WrappedPlainView(elem, area.getWrapStyleWord());
                } else {
                    v = new PlainView(elem);
                }
                return v;
            }
        }
        return null;
    }

    private View createI18N(Element elem) {
        String kind = elem.getName();
        if (kind != null) {
            if (kind.equals(AbstractDocument.ContentElementName)) {
                return new PlainParagraph(elem);
            } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
                return new BoxView(elem, View.Y_AXIS);
            }
        }
        return null;
    }

    @Override
    public int getBaseline(@NotNull JTextComponent c, int width, int height) {
        Object i18nFlag = c.getDocument().getProperty("i18n");
        Insets insets = c.getInsets();
        if (Boolean.TRUE.equals(i18nFlag)) {
            View rootView = AquaTextComponentSupport.getRootView(c);
            if (rootView.getViewCount() > 0) {
                height = height - insets.top - insets.bottom;
                int baseline = insets.top;
                int fieldBaseline = AquaTextComponentSupport.getBaseline(
                        rootView.getView(0), width - insets.left - insets.right, height);
                if (fieldBaseline < 0) {
                    return -1;
                }
                return baseline + fieldBaseline;
            }
            return -1;
        }
        FontMetrics fm = c.getFontMetrics(c.getFont());
        return insets.top + fm.getAscent();
    }

    @Override
    public @NotNull Component.BaselineResizeBehavior getBaselineResizeBehavior(JTextComponent c) {
        return Component.BaselineResizeBehavior.CONSTANT_ASCENT;
    }

    static class PlainParagraph extends ParagraphView {

        PlainParagraph(Element elem) {
            super(elem);
            layoutPool = new PlainParagraph.LogicalView(elem);
            layoutPool.setParent(this);
        }

        public void setParent(View parent) {
            super.setParent(parent);
            if (parent != null) {
                setPropertiesFromAttributes();
            }
        }

        protected void setPropertiesFromAttributes() {
            Component c = getContainer();
            if ((c != null) && (! c.getComponentOrientation().isLeftToRight())) {
                setJustification(StyleConstants.ALIGN_RIGHT);
            } else {
                setJustification(StyleConstants.ALIGN_LEFT);
            }
        }

        public int getFlowSpan(int index) {
            Component c = getContainer();
            if (c instanceof JTextArea) {
                JTextArea area = (JTextArea) c;
                if (! area.getLineWrap()) {
                    // no limit if unwrapped
                    return Integer.MAX_VALUE;
                }
            }
            return super.getFlowSpan(index);
        }

        protected SizeRequirements calculateMinorAxisRequirements(int axis, SizeRequirements r) {
            SizeRequirements req = super.calculateMinorAxisRequirements(axis, r);
            Component c = getContainer();
            if (c instanceof JTextArea) {
                JTextArea area = (JTextArea) c;
                if (! area.getLineWrap()) {
                    // min is pref if unwrapped
                    req.minimum = req.preferred;
                } else {
                    req.minimum = 0;
                    req.preferred = getWidth();
                    if (req.preferred == Integer.MAX_VALUE) {
                        // We have been initially set to MAX_VALUE, but we
                        // don't want this as our preferred.
                        req.preferred = 100;
                    }
                }
            }
            return req;
        }

        /**
         * Sets the size of the view.  If the size has changed, layout
         * is redone.  The size is the full size of the view including
         * the inset areas.
         *
         * @param width the width >= 0
         * @param height the height >= 0
         */
        public void setSize(float width, float height) {
            if ((int) width != getWidth()) {
                preferenceChanged(null, true, true);
            }
            super.setSize(width, height);
        }

        /**
         * This class can be used to represent a logical view for
         * a flow.  It keeps the children updated to reflect the state
         * of the model, gives the logical child views access to the
         * view hierarchy, and calculates a preferred span.  It doesn't
         * do any rendering, layout, or model/view translation.
         */
        static class LogicalView extends CompositeView {

            LogicalView(Element elem) {
                super(elem);
            }

            protected int getViewIndexAtPosition(int pos) {
                Element elem = getElement();
                if (elem.getElementCount() > 0) {
                    return elem.getElementIndex(pos);
                }
                return 0;
            }

            protected boolean updateChildren(DocumentEvent.ElementChange ec, DocumentEvent e, ViewFactory f) {
                return false;
            }

            protected void loadChildren(ViewFactory f) {
                Element elem = getElement();
                if (elem.getElementCount() > 0) {
                    super.loadChildren(f);
                } else {
                    View v = new GlyphView(elem);
                    append(v);
                }
            }

            public float getPreferredSpan(int axis) {
                if( getViewCount() != 1 )
                    throw new Error("One child view is assumed.");

                View v = getView(0);
                return v.getPreferredSpan(axis);
            }

            protected void forwardUpdateToView(View v, DocumentEvent e, Shape a, ViewFactory f) {
                v.setParent(this);
                super.forwardUpdateToView(v, e, a, f);
            }

            // The following methods don't do anything useful, they
            // simply keep the class from being abstract.

            public void paint(Graphics g, Shape allocation) {
            }

            protected boolean isBefore(int x, int y, Rectangle alloc) {
                return false;
            }

            protected boolean isAfter(int x, int y, Rectangle alloc) {
                return false;
            }

            protected View getViewAtPoint(int x, int y, Rectangle alloc) {
                return null;
            }

            protected void childAllocation(int index, Rectangle a) {
            }
        }
    }

}
