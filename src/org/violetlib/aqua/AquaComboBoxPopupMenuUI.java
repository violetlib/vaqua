/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

/**
 * The UI for a combo box popup menu. It installs the intended menu border. Needed because a JPopupMenu uninstalls and
 * reinstalls the UI when the popup menu is shown.
 */
public class AquaComboBoxPopupMenuUI extends AquaPopupMenuUI {

    public static ComponentUI createUI(final JComponent x) {
        return new AquaComboBoxPopupMenuUI();
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        // When a pop up is shown, the UI is uninstalled and then reinstalled, which removes our border.
        if (c instanceof AquaComboBoxPopup) {
            AquaComboBoxPopup p = (AquaComboBoxPopup) c;
            p.updateContents(false);
        }
    }

    @Override
    protected boolean isContextualMenuStyle(Component c) {
        if (c instanceof JComboBox) {
            JComboBox cb = (JComboBox) c;
            return !cb.isEditable();
        }
        return super.isContextualMenuStyle(c);
    }
}
