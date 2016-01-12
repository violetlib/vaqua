/*
 * Changes Copyright (c) 2015-2016 Alan Snyder.
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

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicPanelUI;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class AquaPanelUI extends BasicPanelUI {

    public static final String PANEL_STYLE_KEY = "JPanel.style";
    public static final String GROUP_BOX_STYLE = "groupBox";
    public static final String GROUP_BOX_TITLE_KEY = "Aqua.groupBoxTitle";

    static RecyclableSingleton<AquaPanelUI> instance = new RecyclableSingletonFromDefaultConstructor<AquaPanelUI>(AquaPanelUI.class);

    public PropertyChangeListener propertyChangeListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            AquaPanelUI.this.propertyChange(evt);
        }
    };

    public static ComponentUI createUI(final JComponent c) {
        return instance.get();
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        AquaVibrantSupport.updateVibrantStyle(c);
        c.addPropertyChangeListener(propertyChangeListener);
        updateStyle(c);
    }

    @Override
    public void uninstallUI(JComponent c) {
        c.removePropertyChangeListener(propertyChangeListener);
        AquaVibrantSupport.uninstallVibrantStyle(c);

        Border b = c.getBorder();
        if (b instanceof UIResource) {
            c.setBorder(null);
        }

        super.uninstallUI(c);
    }

    @Override
    protected void installDefaults(JPanel p) {
        super.installDefaults(p);
        LookAndFeel.installProperty(p, "opaque", Boolean.FALSE);
	}

	@Override
    public void paint(Graphics g, JComponent c) {
        BackgroundPainter p = getBackgroundPainter(c);
        if (p != null) {
            p.paintBackground(c, g, 0, 0, c.getWidth(), c.getHeight());
        }
    }

    @Override
    public final void update(final Graphics g, final JComponent c) {
        if (c.isOpaque() || AquaVibrantSupport.isVibrant(c)) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_TEXTURED | AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    protected void propertyChange(PropertyChangeEvent evt) {
        if (AquaVibrantSupport.processVibrantStyleChange(evt)) {
            return;
        } else {
            String prop = evt.getPropertyName();
            if (PANEL_STYLE_KEY.equals(prop) || GROUP_BOX_TITLE_KEY.equals(prop)) {
                JComponent c = (JComponent) evt.getSource();
                updateStyle(c);
            }
        }
    }

    protected void updateStyle(JComponent c) {
        c.repaint();

        Border b = c.getBorder();
        if (b == null || b instanceof UIResource) {
            c.setBorder(getDefaultBorder(c));
        }
    }

    protected Border getDefaultBorder(JComponent c) {
        String style = getStyle(c);
        if (GROUP_BOX_STYLE.equals(style)) {
            String title = getGroupBoxTitle(c);
            if (title != null) {
                return AquaBoxPainter.getBorderForTitledBox();
            } else {
                return AquaBoxPainter.getTitlelessBorder();
            }
        }
        return null;
    }

    protected String getStyle(JComponent c) {
        Object o = c.getClientProperty(PANEL_STYLE_KEY);
        if (o instanceof String) {
            return (String) o;
        } else {
            return null;
        }
    }

    protected String getGroupBoxTitle(JComponent c) {
        Object o = c.getClientProperty(GROUP_BOX_TITLE_KEY);
        if (o instanceof String) {
            return (String) o;
        } else {
            return null;
        }
    }

    protected BackgroundPainter getBackgroundPainter(JComponent c) {
        Object o = c.getClientProperty(PANEL_STYLE_KEY);
        if (GROUP_BOX_STYLE.equals(o)) {
            return AquaBoxPainter.getInstance();
        }
        return null;
    }
}
