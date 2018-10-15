/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.InlineView;
import javax.swing.text.html.ParagraphView;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AppearanceManager;

/**
 * Much hairy code to support non-word-based line wrap and centering in the same component. Ugh.
 */
public class NameView extends JTextPane {

    private final Font font;

    public NameView() {
        font = UIManager.getFont("FileChooser.previewNameFont");
        setEditorKit(new MyEditorKit());
        setEditable(false);
        addPropertyChangeListener(AppearanceManager.AQUA_APPEARANCE_KEY, ev -> updateAttributes());
    }

    public void setText(@NotNull String text) {
        super.setText(text);
        updateAttributes();
    }

    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    private void updateAttributes() {
        StyledDocument doc = getStyledDocument();
        SimpleAttributeSet attributes = new SimpleAttributeSet();
        StyleConstants.setAlignment(attributes, StyleConstants.ALIGN_CENTER);
        StyleConstants.setFontFamily(attributes, font.getFamily());
        StyleConstants.setFontSize(attributes, font.getSize());
        StyleConstants.setForeground(attributes, getForeground());
        doc.setParagraphAttributes(0, doc.getLength(), attributes, false);
    }
}

class MyEditorKit extends HTMLEditorKit {
    @Override
    public ViewFactory getViewFactory() {
        return new MyViewFactory();
    }
}

class MyViewFactory extends HTMLEditorKit.HTMLFactory {
    @Override
    public View create(Element e) {
        View v = super.create(e);
        if (v instanceof InlineView) {
            return new MyInlineView(e);
        }
        else if (v instanceof ParagraphView) {
            return new MyParagraphView(e);
        }
        return v;
    }
}

class MyInlineView extends InlineView {

    public MyInlineView(Element e) {
        super(e);
    }

    @Override
    public int getBreakWeight(int axis, float pos, float len) {
        return GoodBreakWeight;
    }

    @Override
    public View breakView(int axis, int offset, float pos, float len) {
        if (axis == View.X_AXIS) {
            checkPainter();
            int p1 = getGlyphPainter().getBoundedPosition(this, offset, pos, len);
            if (offset == getStartOffset() && p1 == getEndOffset()) {
                return this;
            }
            return createFragment(offset, p1);
        }
        return this;
    }
}

class MyParagraphView extends ParagraphView {

    public MyParagraphView(Element e) {
        super(e);
        setJustification(StyleConstants.ALIGN_CENTER);
    }

    protected SizeRequirements calculateMinorAxisRequirements(int axis, SizeRequirements r) {
        if (r == null) {
            r = new SizeRequirements();
        }
        int pref = (int) Math.ceil(layoutPool.getPreferredSpan(axis));
        int min = (int) Math.ceil(layoutPool.getMinimumSpan(axis));
        r.minimum = min;
        r.preferred = Math.max(r.minimum, pref);
        r.maximum = Integer.MAX_VALUE;
        r.alignment = 0.5f;
        return r;
    }
}
