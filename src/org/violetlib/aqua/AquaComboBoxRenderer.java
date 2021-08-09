/*
 * Copyright (c) 2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.PopupButtonLayoutConfiguration;

import static org.violetlib.jnr.aqua.AquaUIPainter.Size;
import static org.violetlib.aqua.OSXSystemProperties.*;

/**
 * The list cell renderer used by AquaComboBoxUI for the combo box button content and the cells in the list menu.
 *
 * The goals of this renderer are (1) to add left and right margins to the cell rendering and (2) to display a
 * checkmark, as needed.
 */


@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaComboBoxRenderer implements ListCellRenderer<Object>, UIResource {

    protected static int menuLabelLeftInset = 25;
    protected static int editableMenuLabelLeftInset = OSVersion >= 1015 ? 16 : 5;
    protected static int menuLabelRightInset = OSVersion >= 1015 ? 19 : 5;
    protected static int menuLabelTopInset = 2;
    protected static int menuLabelBottomInset = 3;
    protected static int miniMenuLabelTopInset = 1;
    protected static int miniMenuLabelBottomInset = 0;
    protected static int checkMarkLeftInset = OSVersion >= 1016 ? 10 : OSVersion >= 1015 ? 7 : 5;
    protected static int checkMarkTopInset = 3;

    protected final @NotNull JComboBox<?> comboBox;
    protected final boolean isList;  // true to render a list cell, false to render the button content
    protected @NotNull Size size;
    protected @Nullable ListCellRenderer<Object> customRenderer;
    protected @Nullable Wrapper wrapper;
    protected @Nullable MyDefaultComponent defaultComponent;

    protected boolean shouldDisplayCheckMark;

    public AquaComboBoxRenderer(@NotNull JComboBox<?> comboBox, boolean isList, @NotNull Size size) {
        this.comboBox = comboBox;
        this.isList = isList;
        this.size = size;
    }

    public void setSize(@NotNull Size size) {
        this.size = size;
    }

    public void setCustomRenderer(@Nullable ListCellRenderer<Object> renderer)
    {
        this.customRenderer = renderer;
    }

    @Override
    public @NotNull Component getListCellRendererComponent(JList<?> list,
                                                           Object value,
                                                           int index,
                                                           boolean isSelected,
                                                           boolean cellHasFocus) {
        shouldDisplayCheckMark = !comboBox.isEditable() && shouldDisplayCheckMark(index);
        JComponent c;
        if (customRenderer != null) {
            Component basicComponent = customRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            if (wrapper == null) {
                wrapper = new Wrapper();
            }
            wrapper.setComponent(basicComponent);
            c = wrapper;
        } else {
            if (defaultComponent == null) {
                defaultComponent = new MyDefaultComponent();
            }
            if (value instanceof Icon) {
                defaultComponent.setIcon((Icon) value);
                defaultComponent.setText("");
            } else {
                String text = value != null ? value.toString() : "";
                defaultComponent.setText(text);
                defaultComponent.setIcon(null);
            }
            c = defaultComponent;
        }
        c.setFont(list.getFont());
        c.setForeground(isSelected ? list.getSelectionForeground() : list.getForeground());
        // Set a border that has the appropriate insets based on context.
        int top = 0;
        int left = 0;
        int bottom = 0;
        int right = 0;
        if (isList) {
            top = size == Size.MINI ? miniMenuLabelTopInset : menuLabelTopInset;
            bottom = size == Size.MINI ? miniMenuLabelBottomInset : menuLabelBottomInset;
            left = comboBox.isEditable() ? editableMenuLabelLeftInset : menuLabelLeftInset;
            right = menuLabelRightInset;
        }
        c.setBorder(new EmptyBorder(top, left, bottom, right));
        return c;
    }

    protected void paintMark(@NotNull Graphics g, @NotNull JComponent c) {
        if (shouldDisplayCheckMark) {
            Icon checkMark = AquaImageFactory.getPopupMenuItemCheckIcon(size);
            Color color = c.getForeground();
            Image im = AquaImageFactory.getProcessedImage(checkMark, color);
            int height = c.getHeight();
            int left = checkMarkLeftInset;
            int top = Math.max(checkMarkTopInset, (height - checkMark.getIconHeight() - 1) / 2);
            g.drawImage(im, left, top, null);
        }
    }

    private boolean shouldDisplayCheckMark(int index) {
        if (index >= 0 && isList) {
            Object item = comboBox.getItemAt(index);
            return item != null && item.equals(comboBox.getSelectedItem()) && !isPullDown();
        } else {
            return false;
        }
    }

    protected boolean isPullDown() {
        if (!comboBox.isEditable()) {
            AquaComboBoxUI ui = AquaUtils.getUI(comboBox, AquaComboBoxUI.class);
            if (ui != null) {
                PopupButtonLayoutConfiguration g = (PopupButtonLayoutConfiguration) ui.getLayoutConfiguration();
                return g != null && !g.isPopUp();
            }
        }
        return false;
    }

    private class Wrapper
            extends JComponent {
        public Wrapper() {
            setLayout(new BorderLayout());
        }

        public void setComponent(@NotNull Component c) {
            removeAll();
            add(c);
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            paintMark(g, this);
        }
    }

    private class MyDefaultComponent
            extends JLabel {
        @Override
        public @NotNull Dimension getPreferredSize() {
            Dimension size;
            String text = getText();
            if (text == null || text.isEmpty()) {
                setText(" ");
                size = super.getPreferredSize();
                setText("");
            } else {
                size = super.getPreferredSize();
            }
            return size;
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            paintMark(g, this);
        }
    }
}
