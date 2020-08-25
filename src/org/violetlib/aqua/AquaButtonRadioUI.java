/*
 * Changes copyright (c) 2015-2018 Alan Snyder.
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

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaButtonRadioUI extends AquaButtonLabeledUI {

    protected static final RecyclableSingleton<AquaButtonRadioUI> instance = new RecyclableSingletonFromDefaultConstructor<AquaButtonRadioUI>(AquaButtonRadioUI.class);

    public static ComponentUI createUI(JComponent b) {
        return instance.get();
    }

    public AquaButtonRadioUI() {
        super(new AquaRadioButtonBorder());
    }

    protected String getPropertyPrefix() {
        return "RadioButton" + ".";
    }

    // There is a very special case in the handling of focusable radio buttons. Normally, a mouse press on a focusable
    // radio button does not request focus. However, if the current focus owner is another radio button in the same
    // group, then a focus transfer is requested.

    @Override
    protected Object willHandleButtonPress(AbstractButton b) {
        if (!b.isRequestFocusEnabled() && shouldRequestFocus(b)) {
            b.setRequestFocusEnabled(true);
            return true;
        } else {
            return null;
        }
    }

    @Override
    protected void didHandleButtonPress(AbstractButton b, Object data) {
        if (Boolean.TRUE.equals(data)) {
            b.setRequestFocusEnabled(false);
        }
    }

    protected boolean shouldRequestFocus(AbstractButton b) {
        ButtonModel model = b.getModel();
        if (model instanceof DefaultButtonModel) {
            DefaultButtonModel dm = (DefaultButtonModel) model;
            ButtonGroup group = dm.getGroup();
            if (group != null) {
                Component fc = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                if (fc != null && fc != b && fc instanceof AbstractButton) {
                    AbstractButton fb = (AbstractButton) fc;
                    ButtonModel fm = fb.getModel();
                    // Could enumerate the members of the button group, but that would make the behavior intransitive.
                    if (fm instanceof DefaultButtonModel) {
                        DefaultButtonModel fdm = (DefaultButtonModel) fm;
                        ButtonGroup fg = fdm.getGroup();
                        return fg == group;
                    }
                }
            }
        }
        return false;
    }
}
