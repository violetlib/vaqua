/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import org.violetlib.aqua.AquaListUI;
import org.violetlib.aqua.JListModel;

import javax.swing.*;
import javax.swing.event.MouseInputListener;

/**
 * A UI for the lists in a file chooser browser.
 */
public class AquaFileChooserBrowserListUI extends AquaListUI implements AbstractFileChooserBrowserListUI {

    private JFileChooser fc;
    private AquaFileChooserListMouseBehavior.FileSelectionHandler fileSelectionHandler;
    private AquaFileChooserListMouseBehavior mouseBehavior;

    public AquaFileChooserBrowserListUI(JFileChooser fc) {
        this.fc = fc;
    }

    public void setFileSelectionHandler(AquaFileChooserListMouseBehavior.FileSelectionHandler h) {
        fileSelectionHandler = h;
        if (mouseBehavior != null) {
            mouseBehavior.setFileSelectionHandler(h);
        }
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();

        // Avoid conflict with Cmd-Shift-A in the file chooser
        InputMap map = list.getInputMap(JComponent.WHEN_FOCUSED).getParent();
        KeyStroke ks = KeyStroke.getKeyStroke("shift meta A");
        Object v = map.get(ks);
        if (v != null && v.equals("clearSelection")) {
            InputMap newMap = new InputMap();
            newMap.setParent(map);
            newMap.put(ks, "selectApplicationsFolder"); // dummy name for now
            SwingUtilities.replaceUIInputMap(list, JComponent.WHEN_FOCUSED, newMap);
        }
    }

    @Override
    protected MouseInputListener createMouseInputListener() {
        if (mouseBehavior == null) {
            mouseBehavior = new AquaFileChooserListMouseBehavior(fc, new JListModel(list));
            mouseBehavior.setFileSelectionHandler(fileSelectionHandler);
        }
        return mouseBehavior;
    }
}
