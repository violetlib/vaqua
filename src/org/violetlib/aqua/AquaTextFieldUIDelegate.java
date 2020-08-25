/*
 * Copyright (c) 2018 Alan Snyder.
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

import java.awt.*;
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.text.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Supports UI behavior specific to JTextField. This code copied from BasicTextFieldUI.
 */
public class AquaTextFieldUIDelegate implements AquaTextComponentUIDelegate {

    @Override
    public @NotNull String getPropertyPrefix() {
        return "TextField";
    }

    @Override
    public void install(@NotNull JTextComponent c) {

    }

    @Override
    public void uninstall(@NotNull JTextComponent c) {

    }

    @Override
    public boolean propertyChange(@NotNull PropertyChangeEvent ev) {
        return false;
    }

    @Override
    public @Nullable EditorKit getEditorKit(@NotNull JTextComponent c) {
        return null;
    }

    @Override
    public @Nullable View create(@NotNull JTextComponent c, Element elem) {
        Document doc = elem.getDocument();
        Object i18nFlag = doc.getProperty("i18n"/*AbstractDocument.I18NProperty*/);
        if (Boolean.TRUE.equals(i18nFlag)) {
            // To support bidirectional text, we build a more heavyweight
            // representation of the field.
            String kind = elem.getName();
            if (kind != null) {
                if (kind.equals(AbstractDocument.ContentElementName)) {
                    return new GlyphView(elem){
                        @Override
                        public float getMinimumSpan(int axis) {
                            // no wrap
                            return getPreferredSpan(axis);
                        }
                    };
                } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
                    return new I18nFieldView(elem);
                }
            }
            // this shouldn't happen, should probably throw in this case.
        }
        return new FieldView(elem);
    }

    @Override
    public int getBaseline(@NotNull JTextComponent c, int width, int height) {
        View rootView = AquaTextComponentSupport.getRootView(c);
        if (rootView.getViewCount() > 0) {
            Insets insets = c.getInsets();
            height = height - insets.top - insets.bottom;
            if (height > 0) {
                int baseline = insets.top;
                View fieldView = rootView.getView(0);
                int vspan = (int)fieldView.getPreferredSpan(View.Y_AXIS);
                if (height != vspan) {
                    int slop = height - vspan;
                    baseline += slop / 2;
                }
                if (fieldView instanceof I18nFieldView) {
                    int fieldBaseline = AquaTextComponentSupport.getBaseline(
                            fieldView, width - insets.left - insets.right, height);
                    if (fieldBaseline < 0) {
                        return -1;
                    }
                    baseline += fieldBaseline;
                }
                else {
                    FontMetrics fm = c.getFontMetrics(c.getFont());
                    baseline += fm.getAscent();
                }
                return baseline;
            }
        }
        return -1;
    }

    @Override
    public @NotNull Component.BaselineResizeBehavior getBaselineResizeBehavior(JTextComponent c) {
        return Component.BaselineResizeBehavior.CENTER_OFFSET;
    }

    @Override
    public @Nullable ActionMap getActionMap(@NotNull JTextComponent c) {
        return null;
    }

    private static class I18nFieldView extends ParagraphView {

        I18nFieldView(Element elem) {
            super(elem);
        }

        public int getFlowSpan(int index) {
            return Integer.MAX_VALUE;
        }

        protected void setJustification(int j) {
            // Justification is done in adjustAllocation(), so disable
            // ParagraphView's justification handling by doing nothing here.
        }

        static boolean isLeftToRight( java.awt.Component c ) {
            return c.getComponentOrientation().isLeftToRight();
        }

        Shape adjustAllocation(Shape a) {
            if (a != null) {
                Rectangle bounds = a.getBounds();
                int vspan = (int) getPreferredSpan(Y_AXIS);
                int hspan = (int) getPreferredSpan(X_AXIS);
                if (bounds.height != vspan) {
                    int slop = bounds.height - vspan;
                    bounds.y += slop / 2;
                    bounds.height -= slop;
                }

                // horizontal adjustments
                Component c = getContainer();
                if (c instanceof JTextField) {
                    JTextField field = (JTextField) c;
                    BoundedRangeModel vis = field.getHorizontalVisibility();
                    int max = Math.max(hspan, bounds.width);
                    int value = vis.getValue();
                    int extent = Math.min(max, bounds.width - 1);
                    if ((value + extent) > max) {
                        value = max - extent;
                    }
                    vis.setRangeProperties(value, extent, vis.getMinimum(),
                            max, false);
                    if (hspan < bounds.width) {
                        // horizontally align the interior
                        int slop = bounds.width - 1 - hspan;

                        int align = ((JTextField)c).getHorizontalAlignment();
                        if(isLeftToRight(c)) {
                            if(align==LEADING) {
                                align = LEFT;
                            }
                            else if(align==TRAILING) {
                                align = RIGHT;
                            }
                        }
                        else {
                            if(align==LEADING) {
                                align = RIGHT;
                            }
                            else if(align==TRAILING) {
                                align = LEFT;
                            }
                        }

                        switch (align) {
                            case SwingConstants.CENTER:
                                bounds.x += slop / 2;
                                bounds.width -= slop;
                                break;
                            case SwingConstants.RIGHT:
                                bounds.x += slop;
                                bounds.width -= slop;
                                break;
                        }
                    } else {
                        // adjust the allocation to match the bounded range.
                        bounds.width = hspan;
                        bounds.x -= vis.getValue();
                    }
                }
                return bounds;
            }
            return null;
        }

        void updateVisibilityModel() {
            Component c = getContainer();
            if (c instanceof JTextField) {
                JTextField field = (JTextField) c;
                BoundedRangeModel vis = field.getHorizontalVisibility();
                int hspan = (int) getPreferredSpan(X_AXIS);
                int extent = vis.getExtent();
                int maximum = Math.max(hspan, extent);
                extent = (extent == 0) ? maximum : extent;
                int value = maximum - extent;
                int oldValue = vis.getValue();
                if ((oldValue + extent) > maximum) {
                    oldValue = maximum - extent;
                }
                value = Math.max(0, Math.min(value, oldValue));
                vis.setRangeProperties(value, extent, 0, maximum, false);
            }
        }

        public void paint(Graphics g, Shape a) {
            Rectangle r = (Rectangle) a;
            g.clipRect(r.x, r.y, r.width, r.height);
            super.paint(g, adjustAllocation(a));
        }

        public int getResizeWeight(int axis) {
            if (axis == View.X_AXIS) {
                return 1;
            }
            return 0;
        }

        public Shape modelToView(int pos, Shape a, Position.Bias b) throws BadLocationException {
            return super.modelToView(pos, adjustAllocation(a), b);
        }

        public Shape modelToView(int p0, Position.Bias b0,
                                 int p1, Position.Bias b1, Shape a)
                throws BadLocationException
        {
            return super.modelToView(p0, b0, p1, b1, adjustAllocation(a));
        }

        public int viewToModel(float fx, float fy, Shape a, Position.Bias[] bias) {
            return super.viewToModel(fx, fy, adjustAllocation(a), bias);
        }

        public void insertUpdate(DocumentEvent changes, Shape a, ViewFactory f) {
            super.insertUpdate(changes, adjustAllocation(a), f);
            updateVisibilityModel();
        }

        public void removeUpdate(DocumentEvent changes, Shape a, ViewFactory f) {
            super.removeUpdate(changes, adjustAllocation(a), f);
            updateVisibilityModel();
        }
    }
}
