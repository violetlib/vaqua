/*
 * @(#)DefaultColumnCellRenderer.java
 *
 * Copyright (c) 2003-2013 Werner Randelshofer, Switzerland.
 * http://www.randelshofer.ch
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import org.violetlib.aqua.AquaFocusHandler;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * DefaultColumnCellRenderer.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public class DefaultColumnCellRenderer extends JPanel implements ListCellRenderer {

    private JLabel textLabel;
    private JLabel arrowLabel;
    private JBrowser browser;
    protected Icon expandedIcon = null;
    protected Icon selectedExpandedIcon = null;
    protected Icon focusedSelectedExpandedIcon = null;
    private static final Border DEFAULT_NO_FOCUS_BORDER = new EmptyBorder(1, 1, 1, 1);
    private static final Color TRANSPARENT_COLOR = new Color(0, true);

    public DefaultColumnCellRenderer(JBrowser browser) {
        this.browser = browser;

        expandedIcon = UIManager.getIcon("Browser.expandedIcon");
        selectedExpandedIcon = UIManager.getIcon("Browser.selectedExpandedIcon");
        focusedSelectedExpandedIcon = UIManager.getIcon("Browser.focusedSelectedExpandedIcon");

        setLayout(new BorderLayout());

        textLabel = new LabelRenderer();
        arrowLabel = new LabelRenderer();
        setOpaque(true);

        textLabel.putClientProperty("Quaqua.Component.visualMargin", new Insets(0, 0, 0, 0));
        textLabel.setOpaque(false);
        arrowLabel.putClientProperty("Quaqua.Component.visualMargin", new Insets(0, 0, 0, 0));
        arrowLabel.setOpaque(false);

        add(textLabel, BorderLayout.CENTER);
        arrowLabel.setIcon(expandedIcon);
        add(arrowLabel, BorderLayout.EAST);
    }

    public Component getListCellRendererComponent(JList list, Object value,
            int index, boolean isSelected,
            boolean cellHasFocus) {
        //setComponentOrientation(list.getComponentOrientation());
        boolean isFocused = AquaFocusHandler.hasFocus(list);

        if (isSelected) {
            setBackground(list.getSelectionBackground());
            Color foreground = (!isFocused && UIManager.getColor("List.inactiveSelectionForeground") != null) ? UIManager.getColor("List.inactiveSelectionForeground") : list.getSelectionForeground();
            textLabel.setForeground(foreground);
            arrowLabel.setForeground(foreground);
            arrowLabel.setIcon(isFocused ? focusedSelectedExpandedIcon : selectedExpandedIcon);
        } else {
            setBackground(TRANSPARENT_COLOR);
            Color foreground = list.getForeground();
            textLabel.setForeground(foreground);
            arrowLabel.setForeground(foreground);
            arrowLabel.setIcon(expandedIcon);
        }

        textLabel.setText((value == null) ? "null" : value.toString());
        //textLabel.setIcon(getFileChooser().getIcon(file));

        arrowLabel.setVisible(!browser.getModel().isLeaf(value));


        textLabel.setEnabled(list.isEnabled());
        textLabel.setFont(list.getFont());

        // Get border. Handle Look and feels which don't specify a border.
        Border border = UIManager.getBorder((cellHasFocus) ? "List.focusCellHighlightBorder" : "List.cellNoFocusBorder");
        if (border == null) {
            border = DEFAULT_NO_FOCUS_BORDER;
        }
        setBorder(border);

        return this;
    }

    // Overridden for performance reasons.
    //public void validate() {}
    @Override
    public void revalidate() {
    }

    @Override
    public void repaint(long tm, int x, int y, int width, int height) {
    }

    @Override
    public void repaint(Rectangle r) {
    }

    @Override
    protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, short oldValue, short newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, int oldValue, int newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, long oldValue, long newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, float oldValue, float newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, double oldValue, double newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {
    }

    //
    // Inner classes
    //
    public static class UIResource extends DefaultColumnCellRenderer implements javax.swing.plaf.UIResource {

        public UIResource(JBrowser browser) {
            super(browser);
        }
    }

    private static class LabelRenderer extends JLabel {
        // Overridden for performance reasons.

        @Override
        public void validate() {
        }

        @Override
        public void revalidate() {
        }

        @Override
        public void repaint(
                long tm, int x, int y, int width, int height) {
        }

        @Override
        public void repaint(Rectangle r) {
        }

        @Override
        protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
            if (propertyName != null && propertyName.equals("text")) {
                super.firePropertyChange(propertyName, oldValue, newValue);
            }
        }

        @Override
        public void firePropertyChange(String propertyName, short oldValue, short newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, int oldValue, int newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, long oldValue, long newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, float oldValue, float newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, double oldValue, double newValue) {
        }

        @Override
        public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {
        }
    };
}
