/*
 * Copyright (c) 2018-2020 Alan Snyder.
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
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuItemUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class AquaMenuItemUI extends BasicMenuItemUI implements AquaComponentUI {

    static final int kPlain = 0, kCheckBox = 1, kRadioButton = 2;
    static final String sPropertyPrefixes[] = { "MenuItem", "CheckBoxMenuItem", "RadioButtonMenuItem" };

    boolean fIsIndeterminate = false;
    int fType;

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

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
        configureAppearanceContext(null);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        IndeterminateListener.install(menuItem);
        AppearanceManager.installListener(menuItem);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListener(menuItem);
        IndeterminateListener.uninstall(menuItem);
        super.uninstallListeners();
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        configureAppearanceContext(null);
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(menuItem);
        }
        appearanceContext = AquaMenuSupport.instance().getAppearanceContext(menuItem, appearance);
        AquaColors.installColors(menuItem, appearanceContext, colors);
        menuItem.repaint();
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        appearanceContext = AquaMenuSupport.instance().getAppearanceContext(menuItem, null);
        if (c.isOpaque()) {
            Color background = colors.getBackground(appearanceContext);
            g.setColor(background);
            if (OSXSystemProperties.useInsetViewStyle()) {
                AquaUtils.paintInsetMenuItemSelection((Graphics2D) g, 0, 0, c.getWidth(), c.getHeight());
            } else {
                g.fillRect(0, 0, c.getWidth(), c.getHeight());
            }
        }
        AquaMenuSupport.instance().paintMenuItem((Graphics2D) g, menuItem, checkIcon, arrowIcon,
                appearanceContext, colors, defaultTextIconGap, acceleratorFont);
    }

    protected Dimension getPreferredMenuItemSize(JComponent c, Icon localCheckIcon, Icon localArrowIcon, int localDefaultTextIconGap) {
        return AquaMenuSupport.instance().getPreferredMenuItemSize(c, localCheckIcon, localArrowIcon, localDefaultTextIconGap, acceleratorFont);
    }

    public void paintBackground(Graphics g, JComponent c, int menuWidth, int menuHeight) {
        assert appearanceContext != null;
        ButtonModel model = ((JMenuItem)c).getModel();
        if (model.isArmed() || model.isSelected()) {
            Color color = colors.getBackground(appearanceContext.withSelected(true));
            g.setColor(color);
            g.fillRect(0, 0, menuWidth, menuHeight);
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

    static final String CLIENT_PROPERTY_KEY = "JMenuItem.selectedState";

    static final IndeterminateListener INDETERMINATE_LISTENER = new IndeterminateListener();
    static class IndeterminateListener implements PropertyChangeListener {

        static void install(JMenuItem menuItem) {
            menuItem.addPropertyChangeListener(CLIENT_PROPERTY_KEY, INDETERMINATE_LISTENER);
            apply(menuItem, menuItem.getClientProperty(CLIENT_PROPERTY_KEY));
        }

        static void uninstall(JMenuItem menuItem) {
            menuItem.removePropertyChangeListener(CLIENT_PROPERTY_KEY, INDETERMINATE_LISTENER);
        }

        public void propertyChange(PropertyChangeEvent evt) {
            String key = evt.getPropertyName();
            if (!CLIENT_PROPERTY_KEY.equalsIgnoreCase(key)) {
                return;
            }

            Object source = evt.getSource();
            if (!(source instanceof JMenuItem)) {
                return;
            }

            JMenuItem c = (JMenuItem)source;
            apply(c, evt.getNewValue());
        }

        static void apply(JMenuItem menuItem, Object value) {
            ButtonUI ui = menuItem.getUI();
            if (!(ui instanceof AquaMenuItemUI)) {
                return;
            }

            AquaMenuItemUI aquaUI = (AquaMenuItemUI)ui;

            if (aquaUI.fIsIndeterminate = "indeterminate".equals(value)) {
                aquaUI.checkIcon = UIManager.getIcon(aquaUI.getPropertyPrefix() + ".dashIcon");
            } else {
                aquaUI.checkIcon = UIManager.getIcon(aquaUI.getPropertyPrefix() + ".checkIcon");
            }
        }

        public static boolean isIndeterminate(JMenuItem menuItem) {
            return "indeterminate".equals(menuItem.getClientProperty(CLIENT_PROPERTY_KEY));
        }
    }
}
