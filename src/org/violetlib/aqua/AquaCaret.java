/*
 * Changes copyright (c) 2018-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2015, Oracle and/or its affiliates. All rights reserved.
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

import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTextUI;
import javax.swing.text.DefaultCaret;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;

import org.jetbrains.annotations.NotNull;

@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaCaret extends DefaultCaret
        implements UIResource, PropertyChangeListener {

    private final static String SELECT_CONTENT_ON_FOCUS_GAINED_KEY = "JTextComponent.selectContentOnFocusGained";

    private boolean isMultiLineEditor;

    @Override
    public void install(JTextComponent c) {
        super.install(c);
        isMultiLineEditor = c instanceof JTextArea || c instanceof JEditorPane;
        c.addPropertyChangeListener(this);
    }

    @Override
    public void deinstall(JTextComponent c) {
        c.removePropertyChangeListener(this);
        super.deinstall(c);
    }

    @Override
    protected Highlighter.HighlightPainter getSelectionPainter() {
        return BasicTextUI.BasicHighlighter.DefaultPainter;
    }

    /**
     * Only show the flashing caret if the selection range is zero
     */
    @Override
    public void setVisible(boolean e) {
        if (e) e = getDot() == getMark();
        super.setVisible(e);
    }

    @Override
    protected void fireStateChanged() {
        // Changing caret position may change caret visibility.
        JTextComponent c = getComponent();
        setVisible(c.isEnabled() && c.isEditable() && c.isFocusOwner());

        super.fireStateChanged();
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();

        if (AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(propertyName)) {
            JTextComponent comp = ((JTextComponent)evt.getSource());

            if (evt.getNewValue() == Boolean.TRUE) {
                setVisible(comp.isFocusOwner());
            } else {
                setVisible(false);
            }

            if (getDot() != getMark()) comp.getUI().damageRange(comp, getDot(), getMark());
        }
    }

    // --- FocusListener methods --------------------------

    private boolean temporaryInhibitSelectAllOnFocusGained = false;
    private boolean temporaryInhibitMouseReleaseBehavior = false; // fix for JDK-8229856

    @Override
    public void focusGained(@NotNull FocusEvent e) {

        if (shouldSelectAllOnFocusGained(e)) {
            JTextComponent c = getComponent();
            int end = c.getDocument().getLength();
            int dot = getDot();
            int mark = getMark();
            if (dot == mark) {
                if (dot == 0) {
                    c.setCaretPosition(end);
                    c.moveCaretPosition(0);
                } else if (dot == end) {
                    c.setCaretPosition(0);
                    c.moveCaretPosition(end);
                }
            }
        }

        super.focusGained(e);
    }

    private boolean shouldSelectAllOnFocusGained(@NotNull FocusEvent e) {

        if (!AquaUtils.isAutoSelectOnFocusAppropriate(e)) {
            return false;
        }

        if (temporaryInhibitSelectAllOnFocusGained) {
            // inhibited by mouse pressed
            temporaryInhibitSelectAllOnFocusGained = false;
            return false;
        }

        JTextComponent c = getComponent();
        if (!c.isEnabled() || !c.isEditable()) {
            return false;
        }

        Object o = c.getClientProperty(SELECT_CONTENT_ON_FOCUS_GAINED_KEY);
        if (isMultiLineEditor) {
            return Boolean.TRUE.equals(o);
        } else {
            return !Boolean.FALSE.equals(o);
        }
    }

    @Override
    public void focusLost(FocusEvent e) {
        temporaryInhibitSelectAllOnFocusGained = false;
        super.focusLost(e);
    }

    // This fixes the problem where when on the mac you have to ctrl left click to
    // get popup triggers the caret has code that only looks at button number.
    // see radar # 3125390
    @Override
    public void mousePressed(MouseEvent e) {
        if (e.isPopupTrigger()) {
            temporaryInhibitMouseReleaseBehavior = true;
        } else {
            temporaryInhibitMouseReleaseBehavior = false;
            super.mousePressed(e);
            temporaryInhibitSelectAllOnFocusGained = true;
        }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        if (!temporaryInhibitMouseReleaseBehavior) {
            super.mouseReleased(e);
        }
        temporaryInhibitMouseReleaseBehavior = false;
    }

// The following code has been removed because it causes caret painting problems after scrolling.

//    /**
//     * Damages the area surrounding the caret to cause
//     * it to be repainted in a new location.  If paint()
//     * is reimplemented, this method should also be
//     * reimplemented.  This method should update the
//     * caret bounds (x, y, width, and height).
//     *
//     * @param r  the current location of the caret
//     * @see #paint
//     */
//    protected synchronized void damage(Rectangle r) {
//        if (r == null || fPainting) return;
//
//        x = r.x - 4;
//        y = r.y;
//        width = 10;
//        height = r.height;
//
//        // Don't damage the border area.  We can't paint a partial border, so get the
//        // intersection of the caret rectangle and the component less the border, if any.
//        Rectangle caretRect = new Rectangle(x, y, width, height);
//        Border border = getComponent().getBorder();
//        if (border != null) {
//            Rectangle alloc = getComponent().getBounds();
//            alloc.x = alloc.y = 0;
//            Insets borderInsets = border.getBorderInsets(getComponent());
//            alloc.x += borderInsets.left;
//            alloc.y += borderInsets.top;
//            alloc.width -= borderInsets.left + borderInsets.right;
//            alloc.height -= borderInsets.top + borderInsets.bottom;
//            Rectangle2D.intersect(caretRect, alloc, caretRect);
//        }
//        x = caretRect.x;
//        y = caretRect.y;
//        width = Math.max(caretRect.width, 1);
//        height = Math.max(caretRect.height, 1);
//        repaint();
//    }
//
//    boolean fPainting = false;
//
//    // See <rdar://problem/3833837> 1.4.2_05-141.3: JTextField performance with Aqua L&F
//    // We are getting into a circular condition with the BasicCaret paint code since it doesn't know about the fact that our
//    // damage routine above elminates the border. Sadly we can't easily change either one, so we will
//    // add a painting flag and not damage during a repaint.
//    public void paint(Graphics g) {
//        if (isVisible()) {
//            fPainting = true;
//            super.paint(g);
//            fPainting = false;
//        }
//    }
}
