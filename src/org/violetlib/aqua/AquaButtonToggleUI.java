/*
 * Changes copyright (c) 2018-2020 Alan Snyder.
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
import javax.swing.plaf.ComponentUI;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaButtonToggleUI extends AquaButtonUI {

    static final RecyclableSingleton<AquaButtonToggleUI> aquaToggleButtonUI = new RecyclableSingletonFromDefaultConstructor<AquaButtonToggleUI>(AquaButtonToggleUI.class);

    public static ComponentUI createUI(JComponent b) {
        return aquaToggleButtonUI.get();
    }

    protected String getPropertyPrefix() {
        return "ToggleButton" + ".";
    }

    // The layout parameters and rendering of a toggle button may be impacted by whether or not the button is a member
    // of a group. Unfortunately, there are no notifications when group membership changes. To reduce overhead, we
    // limit group membership checking to those cases where it might make a difference.

    @Override
    public boolean isPotentiallyGroupSensitive(@NotNull AbstractButton b) {
        // Group membership sensitivity was introduced in macOS 11 for rounded segmented buttons.
        if (OSXSystemProperties.OSVersion < 1016) {
            return false;
        }
        String buttonType = AquaButtonExtendedTypes.getBasicButtonType(b, false);
        return "segmented".equals(buttonType);
    }
}
