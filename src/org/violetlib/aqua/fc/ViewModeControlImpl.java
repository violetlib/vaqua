/*
 * Copyright (c) 2014-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement. For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import javax.swing.*;

import org.violetlib.aqua.OSXSystemProperties;

import static org.violetlib.aqua.AquaButtonUI.BUTTON_TYPE;
import static org.violetlib.aqua.AquaButtonUI.SEGMENTED_BUTTON_POSITION;

/**
 * Control to select list or column view in the file chooser.
 */
public class ViewModeControlImpl extends ViewModeControl {

    private static Image listViewImage;
    private static Image columnViewImage;

    private ButtonGroup group;
    private JToggleButton listViewButton;
    private JToggleButton columnViewButton;

    private int selectedViewMode = -1;  // an invalid value to trigger initialization

    public ViewModeControlImpl() {
        initComponents();
        setFocusable(false);
        setOpaque(false);
        setSelectedViewMode(COLUMN_VIEW);
    }

    @Override
    public synchronized void addKeyListener(KeyListener l) {
        listViewButton.addKeyListener(l);
        columnViewButton.addKeyListener(l);
    }

    @Override
    public void setSelectedViewMode(int mode) {
        if (mode != selectedViewMode && (mode == LIST_VIEW || mode == COLUMN_VIEW)) {
            selectedViewMode = mode;
            int count = getComponentCount();
            String action = "" + mode;
            for (int i = 0; i < count; i++) {
                Component c = getComponent(i);
                if (c instanceof JToggleButton) {
                    JToggleButton b = (JToggleButton) c;
                    if (action.equals(b.getActionCommand())) {
                        b.setSelected(true);
                    }
                }
            }
            selectedViewModeChanged();
        }
    }

    @Override
    public int getSelectedViewMode() {
        return selectedViewMode;
    }

    private void initComponents() {

        if (listViewImage == null) {
            listViewImage = Toolkit.getDefaultToolkit().getImage("NSImage://NSListViewTemplate");
        }

        if (columnViewImage == null) {
            columnViewImage = Toolkit.getDefaultToolkit().getImage("NSImage://NSColumnViewTemplate");
        }

        group = new ButtonGroup();

        ImageIcon listViewIcon = new ImageIcon(listViewImage);
        ImageIcon columnViewIcon = new ImageIcon(columnViewImage);
        listViewButton = createButton(listViewIcon, "first", LIST_VIEW);
        columnViewButton = createButton(columnViewIcon, "last", COLUMN_VIEW);

        add(listViewButton);
        add(columnViewButton);

        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
    }

    protected JToggleButton createButton(ImageIcon ic, String position, int viewMode) {
        JToggleButton b = new JToggleButton(ic);
        b.putClientProperty(BUTTON_TYPE, OSXSystemProperties.OSVersion < 1016 ? "segmentedTextured" : "segmented");
        b.putClientProperty(SEGMENTED_BUTTON_POSITION, position);

        if (OSXSystemProperties.OSVersion >= 1011) {
            b.setMargin(new Insets(5, 5, 5, 5));
        } else {
            b.setMargin(new Insets(6, 5, 6, 5));
        }

        b.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setSelectedViewMode(viewMode);
            }
        });

        b.setActionCommand("" + viewMode);

        group.add(b);
        return b;
    }
}
