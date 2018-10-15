/*
 * Copyright (c) 2014-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import javax.swing.plaf.ListUI;
import java.awt.*;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.io.File;

import org.jetbrains.annotations.Nullable;

/**
 * The browser in a file chooser. Implements special behavior for clicking on an ordinary file in a Save panel.
 */
public class AquaFileChooserBrowser extends JBrowser {
    protected final JFileChooser fc;
    protected final ColumnScrollPaneMouseWheelListener columnScrollPaneMouseWheelListener;

    public AquaFileChooserBrowser(JFileChooser fc) {
        this.fc = fc;
        columnScrollPaneMouseWheelListener = new ColumnScrollPaneMouseWheelListener();
    }

    @Override
    protected ListUI getColumnListUI(ListUI basicUI) {

        AbstractFileChooserBrowserListUI ui = new AquaFileChooserBrowserListUI(fc);

        ui.setFileSelectionHandler(new AquaFileChooserListMouseBehavior.FileSelectionHandler() {
            @Override
            public void fileSelected(File f) {
                AquaFileChooserBrowser.this.fileSelectedInSavePanel(f);
            }
        });

        return (ListUI) ui;
    }

    @Override
    protected JScrollPane createScrollPane(@Nullable JComponent c) {
        JScrollPane sp = super.createScrollPane(c);
        sp.addMouseWheelListener(columnScrollPaneMouseWheelListener);
        return sp;
    }

    protected void fileSelectedInSavePanel(File f) {
    }

    /**
     * Support horizontal mouse wheel scrolling of the entire browser when the mouse is over a column scroll pane.
     */
    protected class ColumnScrollPaneMouseWheelListener implements MouseWheelListener {
        @Override
        public void mouseWheelMoved(MouseWheelEvent e) {
            if (e.isShiftDown()) {
                JScrollPane browserScrollPane = getBrowserScrollPane(e.getComponent());
                if (browserScrollPane != null) {
                    MouseWheelEvent ce = (MouseWheelEvent) SwingUtilities.convertMouseEvent(e.getComponent(), e, browserScrollPane);
                    browserScrollPane.dispatchEvent(ce);
                }
            }
        }

        protected JScrollPane getBrowserScrollPane(Component c) {
            for (;;) {
                Container p = c.getParent();
                if (p == null || p instanceof JFileChooser) {
                    return null;
                }
                if (p instanceof JScrollPane) {
                    return (JScrollPane) p;
                }
                c = p;
            }
        }
    }
}
