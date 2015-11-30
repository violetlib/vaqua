/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicViewportUI;

/**
 * Support vibrant background.
 */
public class AquaViewportUI extends BasicViewportUI {
    static AquaUtils.RecyclableSingleton<AquaViewportUI> instance = new AquaUtils.RecyclableSingletonFromDefaultConstructor<AquaViewportUI>(AquaViewportUI.class);

    public PropertyChangeListener propertyChangeListener = new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            AquaViewportUI.this.propertyChange(evt);
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
    }

    @Override
    public void uninstallUI(JComponent c) {
        c.removePropertyChangeListener(propertyChangeListener);
        AquaVibrantSupport.uninstallVibrantStyle(c);
        super.uninstallUI(c);
    }

    @Override
    public final void update(final Graphics g, final JComponent c) {
        if (c.isOpaque() || AquaVibrantSupport.isVibrant(c)) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    protected void propertyChange(PropertyChangeEvent evt) {
        if (AquaVibrantSupport.processVibrantStyleChange(evt)) {
            return;
        }
    }
}
