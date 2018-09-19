/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;

import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.Configuration;
import org.violetlib.jnr.aqua.GroupBoxConfiguration;

/**
 * Paints the background and title for a group box.
 */
public class AquaBoxPainter implements BackgroundPainter {

    // space allocated around the content area for the box visual border
    private static final int topInset = 10;
    private static final int sideInset = 15;

    // border insets - defines the default content area
    private static final int topBorderInset = topInset + 5;
    private static final int sideBorderInset = sideInset + 8;

    private static final int titleIndent = 10;

    private static Border titledBorder = new BorderUIResource.EmptyBorderUIResource(topBorderInset+11, sideBorderInset, topBorderInset, sideBorderInset);
    private static Border titlelessBorder = new BorderUIResource.EmptyBorderUIResource(topBorderInset, sideBorderInset, topBorderInset, sideBorderInset);

    protected final AquaUIPainter painter = AquaPainting.create();

    private static final AquaUtils.RecyclableSingletonFromDefaultConstructor<AquaBoxPainter> instance
      = new AquaUtils.RecyclableSingletonFromDefaultConstructor<>(AquaBoxPainter.class);

    public static AquaBoxPainter getInstance() {
        return instance.get();
    }

    public AquaBoxPainter() {
    }

    public static Border getBorderForTitledBox() {
        return titledBorder;
    }

    public static Border getTitlelessBorder() {
        return titlelessBorder;
    }

    @Override
    public void paintBackground(JComponent c, Graphics g, int x, int y, int width, int height) {

        // Paint the box background to surround the content area, which is defined by the insets.

        Insets s = c.getInsets();
        int boxLeft = Math.max(0, s.left - sideInset);
        int boxTop = Math.max(0, s.top - topInset);
        int boxWidth = Math.max(0, width - boxLeft - Math.max(0, s.right - sideInset));
        int boxHeight = Math.max(0, height - boxTop - Math.max(0, s.bottom - topInset));
        AquaUtils.configure(painter, c, boxWidth, boxHeight);
        Configuration bg = getConfiguration();
        painter.getPainter(bg).paint(g, boxLeft, boxTop);

        Object o = c.getClientProperty(AquaPanelUI.GROUP_BOX_TITLE_KEY);
        if (o instanceof String) {
            String title = (String) o;
            JLabel titleLabel = new JLabel(title);
            titleLabel.setFont(AquaFonts.getControlTextSmallFont());
            Dimension size = titleLabel.getPreferredSize();
            int labelW = size.width;
            int labelH = size.height;
            if (boxTop >= labelH) {
                int labelX = boxLeft + titleIndent;
                int labelY = boxTop - labelH;
                g.translate(labelX, labelY);
                titleLabel.setSize(labelW, labelH);
                titleLabel.paint(g);
                g.translate(-labelX, -labelY);
            }
        }
    }

    protected Configuration getConfiguration() {
        return new GroupBoxConfiguration(AquaUIPainter.State.ACTIVE, false);
    }
}
