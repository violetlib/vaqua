/*
 * Copyright (c) 2018-2026 Alan Snyder.
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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuItemUI;

import org.jetbrains.annotations.*;

public class AquaMenuItemUI extends BasicMenuItemUI implements AquaComponentUI {

    static final int kPlain = 0, kCheckBox = 1, kRadioButton = 2;
    static final String sPropertyPrefixes[] = { "MenuItem", "CheckBoxMenuItem", "RadioButtonMenuItem" };

    boolean fIsIndeterminate = false;
    int fType;

    protected @NotNull BasicContextualColors colors;

    public AquaMenuItemUI(int type) {
        fType = type;
        colors = AquaColors.getMenuColors();
    }

    public static ComponentUI createUI(JComponent c) {
        int type = kPlain;
        if (c instanceof JCheckBoxMenuItem) type = kCheckBox;
        if (c instanceof JRadioButtonMenuItem) type = kRadioButton;
        return new AquaMenuItemUI(type);
    }

    // The only real difference between the three is which property prefix it returns
    // and therefore which icons!
    protected String getPropertyPrefix() {
        return sPropertyPrefixes[fType];
    }

    protected void installDefaults() {
        super.installDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        SelectionStateListener.install(menuItem);
        AppearanceManager.install(menuItem);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstall(menuItem);
        SelectionStateListener.uninstall(menuItem);
        super.uninstallListeners();
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceManager.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {
        assert menuItem != null;
        AppearanceContext appearanceContext = AquaMenuSupport.instance().getAppearanceContext(menuItem, pc.appearance);
        AquaColors.installColors(menuItem, appearanceContext, colors);

        MenuDescription md = getMenuDescription();
        if (md != null) {
            if (AquaLookAndFeel.ENABLE_VIBRANT_MENU) {
                Object o = getParent().getClientProperty(AquaPopupMenuUI.POP_UP_TRACKER);
                if (o instanceof MenuSelectionBoundsTracker) {
                    MenuSelectionBoundsTracker tracker = (MenuSelectionBoundsTracker) o;
                    tracker.paintingItem((JMenuItem) c, appearanceContext);
                }
            } else {
                Color background = md.colors.getBackground(appearanceContext);
                g.setColor(background);
                if (OSXSystemProperties.useInsetViewStyle()) {
                    AquaUtils.paintInsetMenuItemSelection(g, 0, 0, menuItem.getWidth(), menuItem.getHeight(),
                      getMenuSelectionDescription());
                } else {
                    g.fillRect(0, 0, menuItem.getWidth(), menuItem.getHeight());
                }
            }
            AquaMenuSupport.instance().paintMenuItem(g, menuItem, appearanceContext, md);
        }
    }

    protected @NotNull SelectionHighlightDescription getMenuSelectionDescription()
    {
        return AquaUtils.getMenuSelectionDescription();
    }

    private @Nullable MenuDescription getMenuDescription() {
        AquaPopupMenuUI ui = AquaUtils.getUI(menuItem.getParent(), AquaPopupMenuUI.class);
        return ui != null ? ui.getMenuDescription() : null;
    }

    private @NotNull JPopupMenu getParent() {
        Object parent = menuItem.getParent();
        if (parent instanceof JPopupMenu) {
            return (JPopupMenu) parent;
        }
        throw new AssertionError("JMenuItem does not have a JPopupMenu parent");
    }

    protected Dimension getPreferredMenuItemSize(JComponent c,
                                                 Icon ignoreCheckIcon,
                                                 Icon ingoreArrowIcon,
                                                 int ignoreTextIconGap) {
        MenuDescription md = getMenuDescription();
        if (md != null) {
            return AquaMenuSupport.instance().getPreferredMenuItemSize(menuItem, md.layoutInfo);
        } else {
            return new Dimension(0, 0);
        }
    }

    protected void doClick(MenuSelectionManager msm) {
        Dimension size = menuItem.getSize();
        AquaUtils.blinkMenu(new AquaUtils.Selectable() {
            public void paintSelected(boolean selected) {
                menuItem.setArmed(selected);
                menuItem.paintImmediately(0, 0, size.width, size.height);
            }
        });
        super.doClick(msm);
    }

    static final String SELECTION_STATE_PROPERTY = "JMenuItem.selectedState";
    static final SelectionStateListener SELECTION_STATE_LISTENER = new SelectionStateListener();
    static class SelectionStateListener implements PropertyChangeListener {

        static void install(JMenuItem menuItem) {
            menuItem.addPropertyChangeListener(SELECTION_STATE_PROPERTY, SELECTION_STATE_LISTENER);
        }

        static void uninstall(JMenuItem menuItem) {
            menuItem.removePropertyChangeListener(SELECTION_STATE_PROPERTY, SELECTION_STATE_LISTENER);
        }

        public void propertyChange(PropertyChangeEvent evt) {
            String key = evt.getPropertyName();
            if (!SELECTION_STATE_PROPERTY.equalsIgnoreCase(key)) {
                return;
            }

            Object source = evt.getSource();
            if (!(source instanceof JMenuItem)) {
                return;
            }

            JMenuItem c = (JMenuItem)source;
            c.revalidate();
            c.repaint();
        }
    }

    public boolean isSelectable() {
        return fType == kCheckBox || fType == kRadioButton;
    }
}
