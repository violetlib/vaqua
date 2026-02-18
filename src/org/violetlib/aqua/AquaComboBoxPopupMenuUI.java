/*
 * Copyright (c) 2015-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static org.violetlib.aqua.OSXSystemProperties.macOS26;

/**
 * The UI for a combo box popup menu. It installs the intended menu border. Needed because a JPopupMenu uninstalls and
 * reinstalls the UI when the popup menu is shown.
 */
public class AquaComboBoxPopupMenuUI extends AquaPopupMenuUI {

    public static ComponentUI createUI(JComponent x) {
        return new AquaComboBoxPopupMenuUI();
    }

    protected @Nullable ContainerContextualColors colorsForList;

    public AquaComboBoxPopupMenuUI() {
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        // When a popup is shown, the UI is uninstalled and then reinstalled, which removes our border.
        if (c instanceof AquaComboBoxPopup) {
            AquaComboBoxPopup p = (AquaComboBoxPopup) c;
            p.updateContents(false);
        }
    }

    @Override
    protected @NotNull BasicContextualColors getMenuColors(Component c) {
        if (c instanceof JComboBox) {
            JComboBox cb = (JComboBox) c;
            if (cb.isEditable()) {
                return AquaColors.getComboBoxMenuColors();
            }
        }
        return super.getMenuColors(c);
    }

    @Override
    protected boolean isVibrantSelectionSupportNeeded(Component owner) {
        return false;
    }

    @Override
    protected int getContextualMenuStyle(Component owner) {
        if (owner instanceof JComboBox) {
            JComboBox cb = (JComboBox) owner;
            if (cb.isEditable()) {
                int version = AquaPainting.getVersion();
                if (version < macOS26) {
                    return version >= 1014 ? SIMPLE_CONTEXTUAL_MENU_STYLE : ORDINARY_CONTEXTUAL_MENU_STYLE;
                }
            }
        }
        return super.getContextualMenuStyle(owner);
    }

    public void configure(@NotNull JComboBox<?> cb, @NotNull JList<?> list) {
        if (colorsForList == null) {
            colorsForList = new DelegatedContainerContextualColors(getMenuColors(cb));
        }
        AquaListUI ui = AquaUtils.getUI(list, AquaListUI.class);
        if (ui != null) {
            AquaUIPainter.Size size = AquaUtilControlSize.getUserSizeFrom(cb, AquaUIPainter.Size.REGULAR);
            AquaComboBoxType t = getComboBoxType(cb);
            boolean isVibrant = t != AquaComboBoxType.EDITABLE_COMBO_BOX;
            ui.configureAsMenu(t, size, isVibrant);
            ui.setColors(colorsForList);
            list.putClientProperty(AquaListUI.LIST_VIEW_STYLE_KEY, "inset");
        }
    }

    private @NotNull AquaComboBoxType getComboBoxType(@NotNull JComboBox<?> cb) {
        AquaComboBoxUI ui = AquaUtils.getUI(cb, AquaComboBoxUI.class);
        if (ui != null) {
            return ui.getComboBoxType();
        }
        if (cb.isEditable()) {
            return AquaComboBoxType.EDITABLE_COMBO_BOX;
        }
        return AquaComboBoxType.POP_UP_MENU_BUTTON;
    }
}
