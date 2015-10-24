/*
 * Changes copyright (c) 2015 Alan Snyder.
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
import java.awt.image.BufferedImage;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;

/**
 * The base class for check box and radio button UIs.
 */
public abstract class AquaButtonLabeledUI extends AquaButtonUI {

    protected RecyclableSizingIcon regularIcon;
    protected RecyclableSizingIcon smallIcon;
    protected RecyclableSizingIcon miniIcon;

    public static class RecyclableSizingIcon extends RecyclableSingleton<Icon> {
        final int iconWidth;
        final int iconHeight;

        public RecyclableSizingIcon(final int iconSize) {
            this.iconWidth = this.iconHeight = iconSize;
        }

        public RecyclableSizingIcon(final int iconWidth, final int iconHeight) {
            this.iconWidth = iconWidth;
            this.iconHeight = iconHeight;
        }

        protected Icon getInstance() {
            return new ImageIcon(new BufferedImage(iconWidth, iconHeight, BufferedImage.TYPE_INT_ARGB_PRE));
        }
    }

    protected final AquaLabeledButtonBorder widgetBorder;

    public AquaButtonLabeledUI(AquaLabeledButtonBorder border) {
        widgetBorder = border;
    }

    @Override
    protected Border getDefaultBorder(AbstractButton b) {
        return widgetBorder;
    }

    public Icon getDefaultIcon(final JComponent c) {
        if (regularIcon == null) {
            regularIcon = createDefaultIcon(Size.REGULAR);
            smallIcon = createDefaultIcon(Size.SMALL);
            miniIcon = createDefaultIcon(Size.MINI);
        }

        final Size componentSize = AquaUtilControlSize.getUserSizeFrom(c);
        if (componentSize == Size.REGULAR) return regularIcon.get();
        if (componentSize == Size.SMALL) return smallIcon.get();
        if (componentSize == Size.MINI) return miniIcon.get();
        return regularIcon.get();
    }

    protected RecyclableSizingIcon createDefaultIcon(Size size) {
        return widgetBorder.createDefaultIcon(size);
    }

    @Override
    public Shape getFocusRingOutline(JComponent c) {
        // The focus ring goes around the button icon, not the entire component
        // TBD: much duplication of code to figure out where the button icon is

        final AbstractButton b = (AbstractButton)c;
        final Font f = c.getFont();
        final FontMetrics fm = AquaUtils.getFontMetrics(c, null, f);

        final Insets i = c.getInsets();

        Rectangle viewRect = new Rectangle(b.getWidth(), b.getHeight());
        Rectangle iconRect = new Rectangle();
        Rectangle textRect = new Rectangle();

        Icon icon = b.getIcon();
        if (icon == null) {
            icon = getDefaultIcon(b);
        }

        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = b.getWidth() - (i.right + viewRect.x);
        viewRect.height = b.getHeight() - (i.bottom + viewRect.y);

        // normal size ??
        // at some point we substitute the small icon instead of the normal icon
        // we should base this on height. Use normal unless we are under a certain size
        // see our button code!

        AquaUtils.layoutCompoundLabel(c, fm, b.getText(), icon, b.getVerticalAlignment(), b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(), viewRect, iconRect, textRect, b.getText() == null ? 0 : b.getIconTextGap());
        return widgetBorder.getFocusRingOutline(b, iconRect);
    }

    public synchronized void paint(final Graphics g, final JComponent c) {
        final AbstractButton b = (AbstractButton)c;
        final ButtonModel model = b.getModel();

        final Font f = c.getFont();
        g.setFont(f);
        final FontMetrics fm = g.getFontMetrics();

        Dimension size = b.getSize();

        final Insets i = c.getInsets();

        Rectangle viewRect = new Rectangle(b.getWidth(), b.getHeight());
        Rectangle iconRect = new Rectangle();
        Rectangle textRect = new Rectangle();

        Icon altIcon = b.getIcon();

        // TBD: I do not understand why a checkbox or radio button should be forced to paint a background when used as a
        // cell renderer.

        final boolean isCellEditor = c.getParent() instanceof CellRendererPane;

        // This was erroneously removed to fix [3155996] but really we wanted the controls to just be
        // opaque. So we put this back in to fix [3179839] (radio buttons not being translucent)
        if (b.isOpaque() || isCellEditor) {
            g.setColor(b.getBackground());
            g.fillRect(0, 0, size.width, size.height);
        }

        // only do this if borders are on!
        if (((AbstractButton)c).isBorderPainted() && !isCellEditor) {
            final Border border = c.getBorder();
            if (border instanceof AquaButtonBorder) {
                ((AquaButtonBorder)border).paintBackground(c, g, viewRect.x, viewRect.y, viewRect.width, viewRect.height);
            }
        }

        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = b.getWidth() - (i.right + viewRect.x);
        viewRect.height = b.getHeight() - (i.bottom + viewRect.y);

        // normal size ??
        // at some point we substitute the small icon instead of the normal icon
        // we should base this on height. Use normal unless we are under a certain size
        // see our button code!

        final String text = AquaUtils.layoutCompoundLabel(c, fm, b.getText(), altIcon != null ? altIcon : getDefaultIcon(b), b.getVerticalAlignment(), b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(), viewRect, iconRect, textRect, b.getText() == null ? 0 : b.getIconTextGap());

        // fill background

        // draw the native radio button stuff here.
        if (altIcon == null) {
            widgetBorder.paintBackground(c, g, iconRect.x, iconRect.y, iconRect.width, iconRect.height);
        } else {
            // Paint the button
            if (!model.isEnabled()) {
                if (model.isSelected()) {
                    altIcon = b.getDisabledSelectedIcon();
                } else {
                    altIcon = b.getDisabledIcon();
                }
            } else if (model.isPressed() && model.isArmed()) {
                altIcon = b.getPressedIcon();
                if (altIcon == null) {
                    // Use selected icon
                    altIcon = b.getSelectedIcon();
                }
            } else if (model.isSelected()) {
                if (b.isRolloverEnabled() && model.isRollover()) {
                    altIcon = b.getRolloverSelectedIcon();
                    if (altIcon == null) {
                        altIcon = b.getSelectedIcon();
                    }
                } else {
                    altIcon = b.getSelectedIcon();
                }
            } else if (b.isRolloverEnabled() && model.isRollover()) {
                altIcon = b.getRolloverIcon();
            }

            if (altIcon == null) {
                altIcon = b.getIcon();
            }

            altIcon.paintIcon(c, g, iconRect.x, iconRect.y);
        }

        // Draw the Text
        if (text != null) {
            final View v = (View)c.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, textRect);
            } else {
                paintText(g, b, textRect, text);
            }
        }
    }

    /**
     * The preferred size of the button
     */
    public Dimension getPreferredSize(final JComponent c) {
        if (c.getComponentCount() > 0) { return null; }

        final AbstractButton b = (AbstractButton)c;

        final String text = b.getText();

        Icon buttonIcon = b.getIcon();
        if (buttonIcon == null) {
            buttonIcon = getDefaultIcon(b);
        }

        final Font font = b.getFont();
        final FontMetrics fm = b.getFontMetrics(font);

        Rectangle prefViewRect = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);
        Rectangle prefIconRect = new Rectangle();
        Rectangle prefTextRect = new Rectangle();

        AquaUtils.layoutCompoundLabel(c, fm, text, buttonIcon, b.getVerticalAlignment(), b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(), prefViewRect, prefIconRect, prefTextRect, text == null ? 0 : b.getIconTextGap());

        // find the union of the icon and text rects (from Rectangle.java)
        final int x1 = Math.min(prefIconRect.x, prefTextRect.x);
        final int x2 = Math.max(prefIconRect.x + prefIconRect.width, prefTextRect.x + prefTextRect.width);
        final int y1 = Math.min(prefIconRect.y, prefTextRect.y);
        final int y2 = Math.max(prefIconRect.y + prefIconRect.height, prefTextRect.y + prefTextRect.height);
        int width = x2 - x1;
        int height = y2 - y1;

        Insets prefInsets = b.getInsets();
        width += prefInsets.left + prefInsets.right;
        height += prefInsets.top + prefInsets.bottom;
        return new Dimension(width, height);
    }
}
