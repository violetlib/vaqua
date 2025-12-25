/*
 * Changes Copyright (c) 2015-2025 Alan Snyder.
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

import org.jetbrains.annotations.*;
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
            appearance = AppearanceManager.getAppearance(b);
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

    public void paintMenuItem(@NotNull Graphics2D g,
                              @NotNull JMenuItem b,
                              @NotNull AppearanceContext context,
                              @NotNull MenuDescription md
    ) {
        ButtonModel model = b.getModel();
        Rectangle viewRect = new Rectangle(0, 0, b.getWidth(), b.getHeight());

        Font f = md.labelFont;
        g.setFont(f);
        FontMetrics fm = md.layoutInfo.labelFontMetrics;

        // get Accelerator text
        KeyStroke accelerator = b.getAccelerator();
        String modifiersString = "";
        String keyString = "";
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
        Rectangle contentRect = new Rectangle();

        // layout the text and icon
        String text = layoutMenuItem(b, b.getText(), keyString, modifiersString, md.layoutInfo,
          viewRect, iconRect, textRect, acceleratorRect, checkIconRect, arrowIconRect, contentRect
        );

        Color foreground = md.colors.getForeground(context);
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

        Icon selectionIcon = getSelectionIcon(b, md);
        if (selectionIcon != null) {
            paintCheck(g, b, foreground, selectionIcon, checkIconRect);
        }

        // Draw the accelerator first in case the HTML renderer changes the color
        if (!keyString.isEmpty()) {
            int yAccel = acceleratorRect.y + fm.getAscent();
            if (modifiersString.isEmpty()) {
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
                    g.setFont(md.acceleratorFont);
                    drawString(g, b, modifiersString, underlinedChar, acceleratorRect.x, yAccel, isEnabled, isSelected);
                    g.setFont(f);
                    JavaSupport.drawString(b, g, keyString, acceleratorRect.x + acceleratorRect.width - emWidth, yAccel);
                } else {
                    int xAccel = acceleratorRect.x + emWidth;
                    g.setFont(md.acceleratorFont);
                    drawString(g, b, modifiersString, underlinedChar, xAccel, yAccel, isEnabled, isSelected);
                    g.setFont(f);
                    JavaSupport.drawString(b, g, keyString, xAccel - fm.stringWidth(keyString), yAccel);
                }
            }
        }

        // Draw the Text
        if (text != null && !text.isEmpty()) {
            View v = (View)b.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, textRect);
            } else {
                int mnemonic = (AquaMnemonicHandler.isMnemonicHidden() ? -1 : model.getMnemonic());
                drawString(g, b, text, mnemonic, textRect.x, textRect.y + fm.getAscent(), isEnabled, isSelected);
            }
        }

        // Paint the Arrow
        if (arrowIconRect.width > 0 && b instanceof JMenu) {
            paintArrow(g, b, model, foreground, md.arrowIcon, arrowIconRect);
        }
    }

    private @Nullable Icon getSelectionIcon(@NotNull JMenuItem b, @NotNull MenuDescription md) {
        Object value = b.getClientProperty(AquaMenuItemUI.SELECTION_STATE_PROPERTY);
        if ("indeterminate".equals(value)) {
            return md.indeterminateIcon;
        }
        if (b.isSelected()) {
            if (b instanceof JRadioButtonMenuItem) {
                return md.radioIcon;
            }
            return  md.checkIcon;
        }
        return null;
    }

    private void paintCheck(Graphics g, JMenuItem item, Color color, Icon checkIcon, Rectangle checkIconRect) {
        if (isTopLevelMenu(item) || !item.isSelected()) {
            return;
        }
        Icon ic = AquaImageFactory.getProcessedImage(checkIcon, color);
        ic.paintIcon(item, g, checkIconRect.x, checkIconRect.y);
    }

    private void paintArrow(Graphics g, JMenuItem c, ButtonModel model, Color color, Icon arrowIcon, Rectangle arrowIconRect) {
        if (isTopLevelMenu(c)) {
            return;
        }
        Icon ic = AquaImageFactory.getProcessedImage(arrowIcon, color);
        ic.paintIcon(c, g, arrowIconRect.x, arrowIconRect.y);
    }

    private void paintIcon(Graphics g, JMenuItem c, Rectangle localIconRect, boolean isEnabled) {
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
     *  not case-sensitive.
     */
    private void drawString(Graphics g, JComponent c, String text, int underlinedChar, int x, int y, boolean isEnabled, boolean isSelected) {
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
     * Returns true if the menu item is a JMenu and it is a top level menu (on the menu bar).
     */
    private static boolean isTopLevelMenu(JMenuItem menuItem) {
        return (menuItem instanceof JMenu) && (((JMenu)menuItem).isTopLevelMenu());
    }

    public @NotNull Dimension getPreferredMenuItemSize(@NotNull JMenuItem b, @NotNull MenuLayoutInfo info) {
        Icon icon = b.getIcon();
        String text = b.getText();
        KeyStroke accelerator = b.getAccelerator();
        String keyString = "";
        String modifiersString = "";

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

        Rectangle iconRect = new Rectangle();
        Rectangle textRect = new Rectangle();
        Rectangle acceleratorRect = new Rectangle();
        Rectangle checkIconRect = new Rectangle();
        Rectangle arrowIconRect = new Rectangle();
        Rectangle viewRect = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);
        Rectangle contentRect = new Rectangle();

        layoutMenuItem(b, text, keyString, modifiersString, info,
          viewRect, iconRect, textRect, acceleratorRect, checkIconRect, arrowIconRect, contentRect);
        return AquaUtils.extend(contentRect.getSize(), info.insets);
    }

    private @Nullable String layoutMenuItem(@NotNull JMenuItem menuItem,
                                            @NotNull String text,
                                            @Nullable String keyString,
                                            @Nullable String modifiersString,
                                            @NotNull MenuLayoutInfo ml,
                                            @NotNull Rectangle viewR,  // available space
                                            @NotNull Rectangle iconR,  // output
                                            @NotNull Rectangle textR,  // output
                                            @NotNull Rectangle acceleratorR,  // output
                                            @NotNull Rectangle selectionIconR,  // output
                                            @NotNull Rectangle arrowIconR,  // output
                                            @NotNull Rectangle contentRect  // output
    ) {
        // Do the initial arrangement LTR, then flip if needed for RTL.

        // Determine the size of the accelerator.
        boolean acceleratorTextIsEmpty = keyString == null || keyString.isEmpty();
        if (acceleratorTextIsEmpty) {
            acceleratorR.width = acceleratorR.height = 0;
        } else {
            FontMetrics fm = ml.acceleratorFontMetrics;
            acceleratorR.width = SwingUtilities.computeStringWidth(fm, modifiersString);
            // The keyStrings should all line up, so always adjust the width by the same amount
            // (if they're multi-char, they won't line up but at least they won't be cut off)
            acceleratorR.width += Math.max(fm.charWidth('M'), SwingUtilities.computeStringWidth(fm, keyString));
            acceleratorR.height = fm.getHeight();
        }
        int acceleratorExtraWidth = acceleratorR.width > 0 ? acceleratorR.width + ml.arrowIconSeparation : 0;

        // Determine the arrangement of the icon and label (zero based)
        Dimension availableSpaceBasic
          = computeAvailableSpaceForIconAndLabel(menuItem, viewR.getSize(), acceleratorExtraWidth, ml);
        ButtonLayoutInfo info = computeBasicLayout(menuItem, availableSpaceBasic);
        if (info == null) {
            return null;
        }

        Insets s = ml.insets;
        contentRect.setBounds(offset(info.contentBounds, s));
        if (info.labelBounds != null) {
            textR.setBounds(offset(info.labelBounds, s));
        } else {
            textR.setBounds(0, 0, 0, 0);
        }
        if (info.iconBounds != null) {
            iconR.setBounds(offset(info.iconBounds, s));
        } else {
            iconR.setBounds(0, 0, 0, 0);
        }
        if (info.substitutedLabel != null) {
            text = info.substitutedLabel;
        }

        boolean isTopLevelMenu = isTopLevelMenu(menuItem);
        if (!isTopLevelMenu) {
            Dimension selectionIconSize =  ml.selectionIconSize;
            if (selectionIconSize.width > 0) {
                selectionIconR.width = selectionIconSize.width;
                selectionIconR.height = selectionIconSize.height;
            } else {
                selectionIconR.width = selectionIconR.height = 0;
            }
            Dimension arrowSize = ml.arrowSize;
            if (arrowSize.width > 0) {
                arrowIconR.width = arrowSize.width;
                arrowIconR.height = arrowSize.height;
            } else {
                arrowIconR.width = arrowIconR.height = 0;
            }
        }

        // Adjust the overall size
        contentRect.width += acceleratorExtraWidth;

        int left = s.left;
        int extraLeft = 0;

        if (!isTopLevelMenu(menuItem)) {
            // Add in the checkIcon
            if (selectionIconR.width > 0) {
                contentRect.width += selectionIconR.width;
                contentRect.width += ml.selectionIconSeparation;
                extraLeft = selectionIconR.width + ml.selectionIconSeparation;
                left += extraLeft;
            }

            // Add in the arrowIcon space
            if (arrowIconR.width > 0) {
                contentRect.width += ml.arrowIconSeparation;
                contentRect.width += arrowIconR.width;
            }
        }

        // Adjust the positions

        iconR.x += extraLeft;
        textR.x += extraLeft;

        // Position the Accelerator text rect
        // Menu shortcut text *ought* to have the letters left-justified - look at a menu with an "M" in it
        acceleratorR.x += (viewR.width - arrowIconR.width - acceleratorR.width - s.right);
        acceleratorR.y = viewR.y + (viewR.height / 2) - (acceleratorR.height / 2);

        if (!isTopLevelMenu) {
            arrowIconR.x = viewR.width - (arrowIconR.width + s.right);
            arrowIconR.y = viewR.y + (contentRect.height / 2) - (arrowIconR.height / 2) + 1;
            selectionIconR.y = viewR.y + (contentRect.height / 2) - (selectionIconR.height / 2);
            selectionIconR.x = s.left;
        }

        if (false) {
            Utils.logDebug("Layout: v=" +viewR+"  c="+ selectionIconR +" i="+
              iconR+" t="+textR+" acc="+acceleratorR+" a="+arrowIconR);
        }

        if (!AquaUtils.isLeftToRight(menuItem)) {
            // Flip the rectangles so that instead of [check][icon][text][accel/arrow] it's [accel/arrow][text][icon][check]
            int w = viewR.width;
            selectionIconR.x = w - (selectionIconR.x + selectionIconR.width);
            iconR.x = w - (iconR.x + iconR.width);
            textR.x = w - (textR.x + textR.width);
            acceleratorR.x = w - (acceleratorR.x + acceleratorR.width);
            arrowIconR.x = w - (arrowIconR.x + arrowIconR.width);
        }

        return text;
    }

    private static @NotNull Rectangle offset(@NotNull Rectangle r, @NotNull Insets s) {
        return new Rectangle(r.x + s.left, r.y + s.top, r.width, r.height);
    }

    /**
     * Compute the available space for the menu icon and label. If computing a preferred size, no adjustment is needed.
     */
    private @NotNull Dimension computeAvailableSpaceForIconAndLabel(@NotNull JMenuItem menuItem,
                                                                    @NotNull Dimension controlAvailableSpace,
                                                                    int acceleratorWidth,
                                                                    @NotNull MenuLayoutInfo info) {
        if (controlAvailableSpace.width >= Short.MAX_VALUE) {
            return controlAvailableSpace;
        }
        int width = controlAvailableSpace.width;
        int height = controlAvailableSpace.height;
        if (info.selectionIconSize.width > 0) {
            width -= (info.selectionIconSize.width + info.selectionIconSeparation);
        }
        if (menuItem instanceof JMenu && info.arrowSize.width > 0) {
            width -= (info.arrowSize.width + info.arrowIconSeparation);
        }
        width -= acceleratorWidth;
        width -= (info.insets.left + info.insets.right);
        height -= (info.insets.top + info.insets.bottom);
        return new Dimension(Math.max(0, width), Math.max(0, height));
    }

    /**
     * Compute a basic layout of the menu item label and icon.
     * @param menuItem The menu item.
     * @param availableSize The available size for the label and icon. The size can be very large when computing the
     *                      preferred size.
     * @return a description of the label and icon layout. The description is zero based.
     */
    private @Nullable ButtonLayoutInfo computeBasicLayout(@NotNull JMenuItem menuItem,
                                                          @NotNull Dimension availableSize) {
        Icon icon = menuItem.getIcon();
        Dimension iconSize = icon != null ? new Dimension(icon.getIconWidth(), icon.getIconHeight()) : null;
        CompoundLabelAlignment alignment = CompoundLabelAlignment.HORIZONTAL_RIGHT_TEXT;
        CompoundLabelLayoutEngine engine
          = AquaButtonSupport.createCompoundLayoutEngine(menuItem, iconSize, null, null, alignment);
        if (engine == null) {
            return null;
        }
        return engine.getLayoutInfo(availableSize.width, availableSize.height, new Insets(0, 0, 0, 0)).toLeftAligned();
    }
}
