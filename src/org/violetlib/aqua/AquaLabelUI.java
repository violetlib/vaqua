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
import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.plaf.basic.BasicLabelUI;
import javax.swing.text.View;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

public class AquaLabelUI extends BasicLabelUI implements AquaComponentUI {

    public static ComponentUI createUI(JComponent c) {
        return new AquaLabelUI();
    }

    private static AquaCellEditorPolicy cellEditorPolicy = AquaCellEditorPolicy.getInstance();

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    private Rectangle paintIconR = new Rectangle();
    private Rectangle paintTextR = new Rectangle();

    public AquaLabelUI() {
        colors = AquaColors.CLEAR_CONTROL_COLORS;
    }

    @Override
    protected void installDefaults(JLabel c) {
        super.installDefaults(c);
        configureAppearanceContext(null, c);
    }

    @Override
    protected void installListeners(JLabel c) {
        super.installListeners(c);
        AquaUtilControlSize.addSizePropertyListener(c);
        AppearanceManager.installListener(c);
    }

    @Override
    protected void uninstallListeners(JLabel c) {
        AppearanceManager.uninstallListener(c);
        AquaUtilControlSize.removeSizePropertyListener(c);
        super.uninstallListeners(c);
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance, (JLabel)c);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        // JLabel is not active state sensitive
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance, @NotNull JLabel label) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(label);
        }
        AquaUIPainter.State state = AquaUIPainter.State.ACTIVE;
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        // If the label is being used as a cell renderer component, it is up to the cell renderer to configure
        // its colors.
        if (cellEditorPolicy.getCellStatus(label) == null) {
            AquaColors.installColors(label, appearanceContext, colors);
        }
        label.repaint();
    }

    @Override
    public void update(Graphics g, JComponent c) {
        // If a label is used for the title of a titled border, it has no parent and its appearance property may be
        // stale.
        if (c.getParent() != null) {
            AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
            super.update(g, c);
            AppearanceManager.restoreCurrentAppearance(appearance);
        } else {
            super.update(g, c);
        }
    }

    @Override
    public void paint(Graphics g, JComponent c)
    {
        JLabel label = (JLabel)c;
        String text = label.getText();
        Icon icon = (label.isEnabled()) ? label.getIcon() : label.getDisabledIcon();

        if ((icon == null) && (text == null)) {
            return;
        }

        FontMetrics fm = label.getFontMetrics(g.getFont());
        String clippedText = layout(label, fm, c.getWidth(), c.getHeight());

        if (icon != null) {
            if (icon instanceof ImageIcon) {
                ImageIcon ii = (ImageIcon) icon;
                Image im = ii.getImage();
                if (AquaImageFactory.isTemplateImage(im)) {
                    Color foreground = label.getForeground();
                    Image coloredImage = AquaImageFactory.getProcessedImage(im, foreground);
                    icon = new ImageIcon(coloredImage);
                }
            }
            icon.paintIcon(c, g, paintIconR.x, paintIconR.y);
        }

        if (text != null) {
            View v = (View) c.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, paintTextR);
            } else {
                int textX = paintTextR.x;
                int textY = paintTextR.y + fm.getAscent();

                if (label.isEnabled()) {
                    paintEnabledText(label, g, clippedText, textX, textY);
                }
                else {
                    paintDisabledText(label, g, clippedText, textX, textY);
                }
            }
        }
    }

    private @NotNull String layout(JLabel label, FontMetrics fm, int width, int height) {
        Insets insets = label.getInsets(null);
        String text = label.getText();
        Icon icon = (label.isEnabled()) ? label.getIcon() : label.getDisabledIcon();
        Rectangle paintViewR = new Rectangle();
        paintViewR.x = insets.left;
        paintViewR.y = insets.top;
        paintViewR.width = width - (insets.left + insets.right);
        paintViewR.height = height - (insets.top + insets.bottom);
        paintIconR.x = paintIconR.y = paintIconR.width = paintIconR.height = 0;
        paintTextR.x = paintTextR.y = paintTextR.width = paintTextR.height = 0;
        return layoutCL(label, fm, text, icon, paintViewR, paintIconR, paintTextR);
    }

    @Override
    protected void paintEnabledText(JLabel l, Graphics g, String s, int textX, int textY) {
        paintText(l, g, s, textX, textY);
    }

    @Override
    protected void paintDisabledText(JLabel l, Graphics g, String s, int textX, int textY) {
        paintText(l, g, s, textX, textY);
    }

    private void paintText(@NotNull JLabel l, @NotNull Graphics g, @NotNull String s, int textX, int textY) {
        int mnemIndex = l.getDisplayedMnemonicIndex();
        if (AquaMnemonicHandler.isMnemonicHidden()) {
            mnemIndex = -1;
        }

        Color foreground = l.getForeground();

        // Special case for TitledBorder, where the label is not in the hierarchy and has no back link
        if (l.getParent() == null) {
            Color defaultColor = UIManager.getColor("TitledBorder.titleColor");
            if (foreground == defaultColor) {
                AquaAppearance appearance = AppearanceManager.getCurrentAppearance();
                foreground = appearance.getColor("controlText");
                //System.err.println("Using default color for titled border: " + AquaColors.toString(foreground));
            }
        }

        g.setColor(foreground);
        JavaSupport.drawStringUnderlineCharAt(l, (Graphics2D) g, s, mnemIndex, textX, textY);
    }
}
