/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

// Based on ColorChooserDialog in javax.swing.JColorChooser

/*
 * Copyright (c) 1998, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Locale;

/**
 * The dialog used for LAF supplied color chooser dialogs. OK and Cancel buttons are not needed because any color
 * selection will be immediately applied and the window close button can be used to dismiss the dialog.
 */
@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaColorChooserDialog extends JDialog {
    private Color initialColor;
    private JColorChooser chooserPane;

    public AquaColorChooserDialog(Dialog owner, String title, boolean modal, Component c, JColorChooser chooserPane)
        throws HeadlessException {
        super(owner, title, modal);
        initColorChooserDialog(c, chooserPane);
    }

    public AquaColorChooserDialog(Frame owner, String title, boolean modal, Component c, JColorChooser chooserPane)
        throws HeadlessException {
        super(owner, title, modal);
        initColorChooserDialog(c, chooserPane);
    }

    protected void initColorChooserDialog(Component c, JColorChooser chooserPane) {
        setResizable(false);

        this.chooserPane = chooserPane;

        Locale locale = getLocale();
        String resetString = UIManager.getString("ColorChooser.resetText", locale);

        Container contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(chooserPane, BorderLayout.CENTER);

        /*
         * Create Lower button panel
         */
        JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new FlowLayout(FlowLayout.CENTER));

        // The following few lines are used to register esc to close the dialog
        @SuppressWarnings("serial") // anonymous class
        Action hideAction = new AbstractAction() {
            public void actionPerformed(ActionEvent e) {
                setVisible(false);
            }
        };

        KeyStroke cancelKeyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        InputMap inputMap = chooserPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        ActionMap actionMap = chooserPane.getActionMap();
        if (inputMap != null && actionMap != null) {
            inputMap.put(cancelKeyStroke, "cancel");
            actionMap.put("cancel", hideAction);
        }
        // end esc handling

        JButton resetButton = new JButton(resetString);
        resetButton.getAccessibleContext().setAccessibleDescription(resetString);
        resetButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                reset();
            }
        });
        int mnemonic = getUIDefaultsInt("ColorChooser.resetMnemonic", locale, -1);
        if (mnemonic != -1) {
            resetButton.setMnemonic(mnemonic);
        }
        buttonPane.add(resetButton);
        contentPane.add(buttonPane, BorderLayout.SOUTH);

        JRootPane jRootPane = getRootPane();
        jRootPane.putClientProperty("Window.style", "small");
        applyComponentOrientation(((c == null) ? jRootPane : c).getComponentOrientation());

        pack();
        setLocationRelativeTo(c);
    }

    @SuppressWarnings("deprecation")
    public void show() {
        initialColor = chooserPane.getColor();
        super.show();
    }

    public void reset() {
        chooserPane.setColor(initialColor);
    }

    private static int getUIDefaultsInt(Object key, Locale l, int defaultValue) {
         Object value = UIManager.get(key, l);

         if (value instanceof Integer) {
             return ((Integer)value).intValue();
         }
         if (value instanceof String) {
             try {
                 return Integer.parseInt((String)value);
             } catch (NumberFormatException nfe) {}
         }
         return defaultValue;
     }
}
