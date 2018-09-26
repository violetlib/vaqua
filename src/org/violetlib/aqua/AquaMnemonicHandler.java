/*
 * Changes copyright (c) 2018 Alan Snyder.
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
import java.awt.event.KeyEvent;
import javax.swing.*;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaMnemonicHandler {
    static final RecyclableSingleton<AltProcessor> altProcessor = new RecyclableSingletonFromDefaultConstructor<AltProcessor>(AltProcessor.class);
    public static KeyEventPostProcessor getInstance() {
        return altProcessor.get();
    }

    protected static boolean isMnemonicHidden = true; // true by default

    public static void setMnemonicHidden(boolean hide) {
        if (UIManager.getBoolean("Button.showMnemonics")) {
            // Do not hide mnemonics if the UI defaults do not support this
            isMnemonicHidden = false;
        } else {
            isMnemonicHidden = hide;
        }
    }

    /**
     * Gets the state of the hide mnemonic flag. This only has meaning if this feature is supported by the underlying OS.
     *
     * @return true if mnemonics are hidden, otherwise, false
     * @see #setMnemonicHidden
     * @since 1.4
     */
    public static boolean isMnemonicHidden() {
        if (UIManager.getBoolean("Button.showMnemonics")) {
            // Do not hide mnemonics if the UI defaults do not support this
            isMnemonicHidden = false;
        }
        return isMnemonicHidden;
    }

    static class AltProcessor implements KeyEventPostProcessor {
        public boolean postProcessKeyEvent(KeyEvent ev) {
            if (ev.getKeyCode() != KeyEvent.VK_ALT) {
                return false;
            }

            JRootPane root = SwingUtilities.getRootPane(ev.getComponent());
            Window winAncestor = (root == null ? null : SwingUtilities.getWindowAncestor(root));

            switch(ev.getID()) {
                case KeyEvent.KEY_PRESSED:
                    setMnemonicHidden(false);
                    break;
                case KeyEvent.KEY_RELEASED:
                    setMnemonicHidden(true);
                    break;
            }

            repaintMnemonicsInWindow(winAncestor);

            return false;
        }
    }

    /*
     * Repaints all the components with the mnemonics in the given window and all its owned windows.
     */
    static void repaintMnemonicsInWindow(Window w) {
        if (w == null || !w.isShowing()) {
            return;
        }

        Window[] ownedWindows = w.getOwnedWindows();
        for (Window element : ownedWindows) {
            repaintMnemonicsInWindow(element);
        }

        repaintMnemonicsInContainer(w);
    }

    /*
     * Repaints all the components with the mnemonics in container.
     * Recursively searches for all the subcomponents.
     */
    static void repaintMnemonicsInContainer(Container cont) {
        for (int i = 0; i < cont.getComponentCount(); i++) {
            Component c = cont.getComponent(i);
            if (c == null || !c.isVisible()) {
                continue;
            }

            if (c instanceof AbstractButton && ((AbstractButton)c).getMnemonic() != '\0') {
                c.repaint();
                continue;
            }

            if (c instanceof JLabel && ((JLabel)c).getDisplayedMnemonic() != '\0') {
                c.repaint();
                continue;
            }

            if (c instanceof Container) {
                repaintMnemonicsInContainer((Container)c);
            }
        }
    }
}
