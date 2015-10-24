/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.text.Element;
import javax.swing.text.FieldView;
import javax.swing.text.ViewFactory;

/**
 * A version of FieldView without scrolling support. Scrolling support has been moved to AquaMarginView.
 */
public class AquaFieldView extends FieldView {

    public AquaFieldView(Element elem) {
        super(elem);
    }

     protected Shape adjustAllocation(Shape a) {
        // like superclass method but does not implement scrolling
        if (a != null) {
            Rectangle bounds = a.getBounds();
            int vspan = (int) getPreferredSpan(Y_AXIS);
            int hspan = (int) getPreferredSpan(X_AXIS);
            if (bounds.height != vspan) {
                int slop = bounds.height - vspan;
                bounds.y += (slop + 1) / 2;     // give preference to the top, it looks better
                bounds.height -= slop;
            }

            // horizontal adjustments
            Component c = getContainer();
            if (c instanceof JTextField) {
                JTextField field = (JTextField) c;
                if (hspan < bounds.width) {
                    // horizontally align the interior
                    int slop = bounds.width - 1 - hspan;

                    int align = field.getHorizontalAlignment();
                    if(AquaUtils.isLeftToRight(field)) {
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
                }
            }
            return bounds;
        }
        return null;
    }

    public void insertUpdate(DocumentEvent changes, Shape a, ViewFactory f) {
        // like superclass method but does not update visibility (scrolling region)
        // the following is what PlainView actually does
        updateDamage(changes, adjustAllocation(a), f);
    }

    public void removeUpdate(DocumentEvent changes, Shape a, ViewFactory f) {
        // like superclass method but does not update visibility (scrolling region)
        // the following is what PlainView actually does
        updateDamage(changes, adjustAllocation(a), f);
    }
}
