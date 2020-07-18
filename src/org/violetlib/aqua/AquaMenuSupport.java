/*
 * Changes Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2013, Oracle and/or its affiliates. All rights reserved.
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
import javax.swing.*;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Shared code for menus and menu items.
 */

public class AquaMenuSupport {

    // Unicode character codes
    static final char
            kUBlackDiamond = 0x25C6,
            kUCheckMark = 0x2713,
            kUControlGlyph = 0x2303,
            kUOptionGlyph = 0x2325,
            kUEnterGlyph = 0x2324,
            kUCommandGlyph = 0x2318,
            kULeftDeleteGlyph = 0x232B,
            kURightDeleteGlyph = 0x2326,
            kUShiftGlyph = 0x21E7,
            kUCapsLockGlyph = 0x21EA;

    static final int ALT_GRAPH_MASK = 1 << 5; // New to Java2

    // Return a string with the proper modifier glyphs
    static String getKeyModifiersText(int modifiers, boolean isLeftToRight) {
        return getKeyModifiersUnicode(modifiers, isLeftToRight);
    }

    // Return a string with the proper modifier glyphs
    private static String getKeyModifiersUnicode(int modifiers, boolean isLeftToRight) {
        StringBuilder buf = new StringBuilder(2);
        // Order (from StandardMenuDef.c): control, option(alt), shift, cmd
        // reverse for right-to-left
        //$ check for substitute key glyphs for localization
        if (isLeftToRight) {
            if ((modifiers & InputEvent.CTRL_MASK) != 0) {
                buf.append(kUControlGlyph);
            }
            if ((modifiers & (InputEvent.ALT_MASK | ALT_GRAPH_MASK)) != 0) {
                buf.append(kUOptionGlyph);
            }
            if ((modifiers & InputEvent.SHIFT_MASK) != 0) {
                buf.append(kUShiftGlyph);
            }
            if ((modifiers & InputEvent.META_MASK) != 0) {
                buf.append(kUCommandGlyph);
            }
        } else {
            if ((modifiers & InputEvent.META_MASK) != 0) {
                buf.append(kUCommandGlyph);
            }
            if ((modifiers & InputEvent.SHIFT_MASK) != 0) {
                buf.append(kUShiftGlyph);
            }
            if ((modifiers & (InputEvent.ALT_MASK | ALT_GRAPH_MASK)) != 0) {
                buf.append(kUOptionGlyph);
            }
            if ((modifiers & InputEvent.CTRL_MASK) != 0) {
                buf.append(kUControlGlyph);
            }
        }
        return buf.toString();
    }

    static final RecyclableSingleton<AquaMenuSupport> sPainter = new RecyclableSingletonFromDefaultConstructor<AquaMenuSupport>(AquaMenuSupport.class);
    static AquaMenuSupport instance() {
        return sPainter.get();
    }

    public @NotNull AppearanceContext getAppearanceContext(@NotNull JMenuItem b, @Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(b);
        }
        ButtonModel model = b.getModel();
        Container ancestor = getAncestor(b);
        boolean isSelected = model.isArmed() || (b instanceof JMenu && model.isSelected());
        boolean isEnabled = model.isEnabled() && (ancestor == null || ancestor.isVisible());
        AquaUIPainter.State state = isEnabled ? AquaUIPainter.State.ACTIVE : AquaUIPainter.State.DISABLED;
        return new AppearanceContext(appearance, state, isSelected, false);
    }

    protected @NotNull AquaUIPainter.State getState(@NotNull JMenuItem b) {
        Container ancestor = getAncestor(b);
        ButtonModel model = b.getModel();
        boolean isEnabled = model.isEnabled() && (ancestor == null || ancestor.isVisible());

        if (!isEnabled) {
            return AquaUIPainter.State.DISABLED;
        } else {
            if (model.isArmed() || (b instanceof JMenu && model.isSelected())) {
                return AquaUIPainter.State.ACTIVE_DEFAULT;
            } else {
                return AquaUIPainter.State.ACTIVE;
            }
        }
    }

    protected @Nullable Container getAncestor(@NotNull JMenuItem b) {
        Container ancestor = b.getParent();
        while (ancestor != null && !(ancestor instanceof JPopupMenu)) ancestor = ancestor.getParent();
        return ancestor;
    }

    public void paintMenuItem(Graphics2D g,
                              JMenuItem b,
                              Icon checkIcon,
                              Icon arrowIcon,
                              AppearanceContext context,
                              BasicContextualColors colors,
                              int defaultTextIconGap,
                              Font acceleratorFont) {

        ButtonModel model = b.getModel();

        int menuWidth = b.getWidth();
        int menuHeight = b.getHeight();
        Insets i = b.getInsets();

        Rectangle viewRect = new Rectangle(0, 0, menuWidth, menuHeight);

        viewRect.x += i.left;
        viewRect.y += i.top;
        viewRect.width -= (i.right + viewRect.x);
        viewRect.height -= (i.bottom + viewRect.y);

        Font f = b.getFont();
        g.setFont(f);
        FontMetrics fm = g.getFontMetrics(f);
        FontMetrics fmAccel = g.getFontMetrics(acceleratorFont);

        // get Accelerator text
        KeyStroke accelerator = b.getAccelerator();
        String modifiersString = "", keyString = "";
        boolean leftToRight = AquaUtils.isLeftToRight(b);
        if (accelerator != null) {
            int modifiers = accelerator.getModifiers();
            if (modifiers > 0) {
                modifiersString = getKeyModifiersText(modifiers, leftToRight);
            }
            int keyCode = accelerator.getKeyCode();
            if (keyCode != 0) {
                keyString = KeyEvent.getKeyText(keyCode);
            } else {
                keyString += accelerator.getKeyChar();
            }
        }

        Rectangle iconRect = new Rectangle();
        Rectangle textRect = new Rectangle();
        Rectangle acceleratorRect = new Rectangle();
        Rectangle checkIconRect = new Rectangle();
        Rectangle arrowIconRect = new Rectangle();

        // layout the text and icon
        String text = layoutMenuItem(b, fm, b.getText(), fmAccel, keyString, modifiersString, b.getIcon(), checkIcon, arrowIcon, b.getVerticalAlignment(), b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(), viewRect, iconRect, textRect, acceleratorRect, checkIconRect, arrowIconRect, b.getText() == null ? 0 : defaultTextIconGap, defaultTextIconGap);

        Color foreground = colors.getForeground(context);
        g.setColor(foreground);
        boolean isSelected = context.isSelected();
        boolean isEnabled = context.getState() != AquaUIPainter.State.DISABLED;

        // We want to paint the icon after the text color is set since some icon painting depends on the correct
        // graphics color being set
        // See <rdar://problem/3792383> Menu icons missing in Java2D's Lines.Joins demo
        // Paint the Icon
        if (b.getIcon() != null) {
            paintIcon(g, b, iconRect, isEnabled);
        }

        // Paint the Check using the current text color
        if (checkIcon != null) {
            paintCheck(g, b, foreground, checkIcon, checkIconRect);
        }

        // Draw the accelerator first in case the HTML renderer changes the color
        if (!keyString.equals("")) {
            int yAccel = acceleratorRect.y + fm.getAscent();
            if (modifiersString.equals("")) {
                // just draw the keyString
                JavaSupport.drawString(b, g, keyString, acceleratorRect.x, yAccel);
            } else {
                int modifiers = accelerator.getModifiers();
                int underlinedChar = 0;
                if ((modifiers & ALT_GRAPH_MASK) > 0) underlinedChar = kUOptionGlyph; // This is a Java2 thing, we won't be getting kOptionGlyph
                // The keyStrings should all line up, so always adjust the width by the same amount
                // (if they're multi-char, they won't line up but at least they won't be cut off)
                int emWidth = Math.max(fm.charWidth('M'), SwingUtilities.computeStringWidth(fm, keyString));

                if (leftToRight) {
                    g.setFont(acceleratorFont);
                    drawString(g, b, modifiersString, underlinedChar, acceleratorRect.x, yAccel, isEnabled, isSelected);
                    g.setFont(f);
                    JavaSupport.drawString(b, g, keyString, acceleratorRect.x + acceleratorRect.width - emWidth, yAccel);
                } else {
                    int xAccel = acceleratorRect.x + emWidth;
                    g.setFont(acceleratorFont);
                    drawString(g, b, modifiersString, underlinedChar, xAccel, yAccel, isEnabled, isSelected);
                    g.setFont(f);
                    JavaSupport.drawString(b, g, keyString, xAccel - fm.stringWidth(keyString), yAccel);
                }
            }
        }

        // Draw the Text
        if (text != null && !text.equals("")) {
            View v = (View)b.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, textRect);
            } else {
                int mnemonic = (AquaMnemonicHandler.isMnemonicHidden() ? -1 : model.getMnemonic());
                drawString(g, b, text, mnemonic, textRect.x, textRect.y + fm.getAscent(), isEnabled, isSelected);
            }
        }

        // Paint the Arrow
        if (arrowIcon != null) {
            paintArrow(g, b, model, foreground, arrowIcon, arrowIconRect);
        }
    }

    // All this had to be copied from BasicMenuItemUI, just to get the right keyModifiersText fn
    // and a few Mac tweaks
    protected Dimension getPreferredMenuItemSize(JComponent c, Icon checkIcon, Icon arrowIcon, int defaultTextIconGap, Font acceleratorFont) {
        JMenuItem b = (JMenuItem)c;
        Icon icon = b.getIcon();
        String text = b.getText();
        KeyStroke accelerator = b.getAccelerator();
        String keyString = "", modifiersString = "";

        if (accelerator != null) {
            int modifiers = accelerator.getModifiers();
            if (modifiers > 0) {
                modifiersString = getKeyModifiersText(modifiers, true); // doesn't matter, this is just for metrics
            }
            int keyCode = accelerator.getKeyCode();
            if (keyCode != 0) {
                keyString = KeyEvent.getKeyText(keyCode);
            } else {
                keyString += accelerator.getKeyChar();
            }
        }

        Font font = b.getFont();
        FontMetrics fm = b.getFontMetrics(font);
        FontMetrics fmAccel = b.getFontMetrics(acceleratorFont);

        Rectangle iconRect = new Rectangle();
        Rectangle textRect = new Rectangle();
        Rectangle acceleratorRect = new Rectangle();
        Rectangle checkIconRect = new Rectangle();
        Rectangle arrowIconRect = new Rectangle();
        Rectangle viewRect = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);

        layoutMenuItem(b, fm, text, fmAccel, keyString, modifiersString, icon, checkIcon, arrowIcon, b.getVerticalAlignment(), b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(), viewRect, iconRect, textRect, acceleratorRect, checkIconRect, arrowIconRect, text == null ? 0 : defaultTextIconGap, defaultTextIconGap);
        // find the union of the icon and text rects
        Rectangle r = new Rectangle();
        r.setBounds(textRect);
        r = SwingUtilities.computeUnion(iconRect.x, iconRect.y, iconRect.width, iconRect.height, r);
        //   r = iconRect.union(textRect);

        // Add in the accelerator
        boolean acceleratorTextIsEmpty = keyString.equals("");

        if (!acceleratorTextIsEmpty) {
            r.width += acceleratorRect.width;
        }

        if (!isTopLevelMenu(b)) {
            // Add in the checkIcon
            r.width += checkIconRect.width;
            r.width += defaultTextIconGap;

            // Add in the arrowIcon space
            r.width += defaultTextIconGap;
            r.width += arrowIconRect.width;
        }

        Insets insets = b.getInsets();
        if (insets != null) {
            r.width += insets.left + insets.right;
            r.height += insets.top + insets.bottom;
        }

        // Tweak for Mac
        r.width += 4 + defaultTextIconGap;
        r.height = Math.max(r.height, 18);

        return r.getSize();
    }

    protected void paintCheck(Graphics g, JMenuItem item, Color color, Icon checkIcon, Rectangle checkIconRect) {
        if (isTopLevelMenu(item) || !item.isSelected()) return;

        Image im = AquaImageFactory.getProcessedImage(checkIcon, color);
        g.drawImage(im, checkIconRect.x, checkIconRect.y, null);
    }

    protected void paintArrow(Graphics g, JMenuItem c, ButtonModel model, Color color, Icon arrowIcon, Rectangle arrowIconRect) {
        if (isTopLevelMenu(c)) return;

        Image im = AquaImageFactory.getProcessedImage(arrowIcon, color);
        g.drawImage(im, arrowIconRect.x, arrowIconRect.y, null);
    }

    protected void paintIcon(Graphics g, JMenuItem c, Rectangle localIconRect, boolean isEnabled) {
        ButtonModel model = c.getModel();
        Icon icon;
        if (!isEnabled) {
            icon = c.getDisabledIcon();
        } else if (model.isPressed() && model.isArmed()) {
            icon = c.getPressedIcon();
            if (icon == null) {
                // Use default icon
                icon = c.getIcon();
            }
        } else {
            icon = c.getIcon();
        }

        if (icon != null) {
            icon.paintIcon(c, g, localIconRect.x, localIconRect.y);
        }
    }

    /** Draw a string with the graphics g at location (x,y) just like g.drawString() would.
     *  The first occurrence of underlineChar in text will be underlined. The matching is
     *  not case sensitive.
     */
    public void drawString(Graphics g, JComponent c, String text, int underlinedChar, int x, int y, boolean isEnabled, boolean isSelected) {
        char lc, uc;
        int index = -1, lci, uci;

        if (underlinedChar != '\0') {
            uc = Character.toUpperCase((char)underlinedChar);
            lc = Character.toLowerCase((char)underlinedChar);

            uci = text.indexOf(uc);
            lci = text.indexOf(lc);

            if (uci == -1) index = lci;
            else if (lci == -1) index = uci;
            else index = (lci < uci) ? lci : uci;
        }

        JavaSupport.drawStringUnderlineCharAt(c, (Graphics2D) g, text, index, x, y);
    }

    /*
     * Returns false if the component is a JMenu and it is a top
     * level menu (on the menubar).
     */
    private static boolean isTopLevelMenu(JMenuItem menuItem) {
        return (menuItem instanceof JMenu) && (((JMenu)menuItem).isTopLevelMenu());
    }

    private String layoutMenuItem(JMenuItem menuItem, FontMetrics fm, String text, FontMetrics fmAccel,
                                  String keyString, String modifiersString, Icon icon, Icon checkIcon,
                                  Icon arrowIcon, int verticalAlignment, int horizontalAlignment,
                                  int verticalTextPosition, int horizontalTextPosition, Rectangle viewR,
                                  Rectangle iconR, Rectangle textR, Rectangle acceleratorR, Rectangle checkIconR,
                                  Rectangle arrowIconR, int textIconGap, int menuItemGap) {
        // Force it to do "LEFT", then flip the rects if we're right-to-left
        Dimension iconSize = icon != null ? new Dimension(icon.getIconWidth(), icon.getIconHeight()) : null;
        AquaUtils.layoutCompoundLabel(menuItem, fm, text, iconSize, verticalAlignment, SwingConstants.LEFT, verticalTextPosition, horizontalTextPosition, viewR, iconR, textR, textIconGap);

        boolean acceleratorTextIsEmpty = (keyString == null) || keyString.equals("");

        if (acceleratorTextIsEmpty) {
            acceleratorR.width = acceleratorR.height = 0;
            keyString = "";
        } else {
            // Accel space doesn't overlap arrow space, even though items can't have both
            acceleratorR.width = SwingUtilities.computeStringWidth(fmAccel, modifiersString);
            // The keyStrings should all line up, so always adjust the width by the same amount
            // (if they're multi-char, they won't line up but at least they won't be cut off)
            acceleratorR.width += Math.max(fm.charWidth('M'), SwingUtilities.computeStringWidth(fm, keyString));
            acceleratorR.height = fmAccel.getHeight();
        }

        /* Initialize the checkIcon bounds rectangle checkIconR.
         */

        boolean isTopLevelMenu = isTopLevelMenu(menuItem);
        if (!isTopLevelMenu) {
            if (checkIcon != null) {
                checkIconR.width = checkIcon.getIconWidth();
                checkIconR.height = checkIcon.getIconHeight();
            } else {
                checkIconR.width = checkIconR.height = 16;
            }

            /* Initialize the arrowIcon bounds rectangle arrowIconR.
             */

            if (arrowIcon != null) {
                arrowIconR.width = arrowIcon.getIconWidth();
                arrowIconR.height = arrowIcon.getIconHeight();
            } else {
                arrowIconR.width = arrowIconR.height = 16;
            }

            textR.x += 12;
            iconR.x += 12;
        }

        Rectangle labelR = iconR.union(textR);

        // Position the Accelerator text rect
        // Menu shortcut text *ought* to have the letters left-justified - look at a menu with an "M" in it
        acceleratorR.x += (viewR.width - arrowIconR.width - acceleratorR.width);
        acceleratorR.y = viewR.y + (viewR.height / 2) - (acceleratorR.height / 2);

        if (!isTopLevelMenu) {
            //    if ( GetSysDirection() < 0 ) hierRect.right = hierRect.left + w + 4;
            //    else hierRect.left = hierRect.right - w - 4;
            arrowIconR.x = (viewR.width - arrowIconR.width) + 1;
            arrowIconR.y = viewR.y + (labelR.height / 2) - (arrowIconR.height / 2) + 1;

            checkIconR.y = viewR.y + (labelR.height / 2) - (checkIconR.height / 2);
            checkIconR.x = 5;

            textR.width += 8;
        }

        if (false) {
            AquaUtils.logDebug("Layout: " +horizontalAlignment+ " v=" +viewR+"  c="+checkIconR+" i="+
                    iconR+" t="+textR+" acc="+acceleratorR+" a="+arrowIconR);
        }

        if (!AquaUtils.isLeftToRight(menuItem)) {
            // Flip the rectangles so that instead of [check][icon][text][accel/arrow] it's [accel/arrow][text][icon][check]
            int w = viewR.width;
            checkIconR.x = w - (checkIconR.x + checkIconR.width);
            iconR.x = w - (iconR.x + iconR.width);
            textR.x = w - (textR.x + textR.width);
            acceleratorR.x = w - (acceleratorR.x + acceleratorR.width);
            arrowIconR.x = w - (arrowIconR.x + arrowIconR.width);
        }
        textR.x += menuItemGap;
        iconR.x += menuItemGap;

        return text;
    }
}
