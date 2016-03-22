/*
 * Changes Copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.plaf.UIResource;

import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.ComboBoxWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.PopupButtonWidget;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

import static org.violetlib.jnr.aqua.AquaUIPainter.ComboBoxWidget.*;
import static org.violetlib.jnr.aqua.AquaUIPainter.PopupButtonWidget.*;

@SuppressWarnings("serial") // Superclass is not serializable across versions
class AquaComboBoxButton extends JButton {

    public static final String BUTTON_TYPE = "JButton.buttonType";

    public static final String LAYOUT_CONFIGURATION_PROPERTY = "Aqua.ComboBox.LayoutConfiguration";

    private static AquaCellEditorPolicy cellEditorPolicy = AquaCellEditorPolicy.getInstance();
    private static AquaComboBoxHierarchyListener hierarchyListener = new AquaComboBoxHierarchyListener();

    protected final JComboBox<Object> comboBox;
    protected final JList<?> list;
    protected final CellRendererPane rendererPane;
    protected final AquaComboBoxUI ui;
    protected final AquaButtonExtendedTypes.ColorDefaults colorDefaults;

    protected final AquaUIPainter painter = AquaPainting.create();
    protected boolean isPopDown;
    protected String style;
    protected Size sizeVariant;
    protected boolean isRollover;

    protected ImageIcon lastTestedIcon;
    protected boolean lastTestedIconIsTemplate;

    @SuppressWarnings("serial") // anonymous class
    protected AquaComboBoxButton(final AquaComboBoxUI ui,
                                 final JComboBox<Object> comboBox,
                                 final CellRendererPane rendererPane,
                                 final JList<?> list) {
        super("");

        putClientProperty(BUTTON_TYPE, "comboboxInternal");

        this.ui = ui;
        this.comboBox = comboBox;
        this.rendererPane = rendererPane;
        this.list = list;

        colorDefaults = new AquaButtonExtendedTypes.ColorDefaults();
        colorDefaults.disabledTextColor = UIManager.getColor("ComboBox.disabledForeground");
        colorDefaults.enabledTextColor = UIManager.getColor("ComboBox.foreground");

        setModel(new DefaultButtonModel() {
            public void setArmed(final boolean armed) {
                super.setArmed(isPressed() ? true : armed);
            }
        });

        setEnabled(comboBox.isEnabled());

        addMouseListener(new RolloverMouseListener());
        addHierarchyListener(hierarchyListener);
    }

    public boolean isEnabled() {
        return comboBox == null ? true : comboBox.isEnabled();
    }

    @SuppressWarnings("deprecation")
    public boolean isFocusTraversable() {
        return false;
    }

    /**
     * Style related configuration affecting layout
     *
     * @param size Optional new size variant to install
     */
    public void configure(Size size) {
        {
            Object o = comboBox.getClientProperty(AquaComboBoxUI.POPDOWN_CLIENT_PROPERTY_KEY);
            isPopDown = Boolean.TRUE.equals(o);
        }

        {
            String style = null;

            Object o = comboBox.getClientProperty(AquaComboBoxUI.STYLE_CLIENT_PROPERTY_KEY);
            if (o instanceof String) {
                style = (String) o;
            } else {
                o = comboBox.getClientProperty(AquaComboBoxUI.ISSQUARE_CLIENT_PROPERTY_KEY);
                if (Boolean.TRUE.equals(o)) {
                    // old client property gets the old square style
                    style = "old_square";
                }
            }

            if ("textured".equals(style) && isOnToolbar(comboBox)) {
                style = "textured-onToolbar";
            }

            this.style = style;
        }

        if (size != null) {
            sizeVariant = size;
        }

        if (sizeVariant == null) {
            sizeVariant = Size.REGULAR;
        }

        AbstractComboBoxLayoutConfiguration layoutConfiguration;
        boolean isEditable = comboBox.isEditable();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(comboBox);

        if (isEditable) {
            ComboBoxWidget widget = getComboBoxWidget();
            layoutConfiguration = new ComboBoxLayoutConfiguration(widget, sizeVariant, ld);
            ComboBoxEditor editor = comboBox.getEditor();
            if (editor instanceof AquaComboBoxUI.AquaComboBoxEditor) {
                AquaComboBoxUI.AquaComboBoxEditor e = (AquaComboBoxUI.AquaComboBoxEditor) editor;
                e.configure(widget);
            }
        } else {
            PopupButtonWidget widget = getPopupButtonWidget();
            sizeVariant = canonicalize(sizeVariant, widget);
            layoutConfiguration = new PopupButtonLayoutConfiguration(widget, sizeVariant, ld);
        }

        comboBox.putClientProperty(LAYOUT_CONFIGURATION_PROPERTY, layoutConfiguration);

        if (AquaUtilControlSize.isOKToInstallDefaultFont(comboBox)) {
            Font df = getDefaultFont(layoutConfiguration);
            AquaUtilControlSize.installDefaultFont(comboBox, df);
        }

        comboBox.revalidate();
        comboBox.repaint();
    }

    public AbstractComboBoxLayoutConfiguration getLayoutConfiguration() {
        return (AbstractComboBoxLayoutConfiguration) comboBox.getClientProperty(LAYOUT_CONFIGURATION_PROPERTY);
    }

    /**
     * Convert size to the standard size for those styles with only one size.
     * This affects the default font.
     */
    protected Size canonicalize(Size size, PopupButtonWidget widget) {
        switch (widget) {
            case BUTTON_POP_UP_BEVEL:
            case BUTTON_POP_DOWN_BEVEL:
            case BUTTON_POP_UP_SQUARE:
            case BUTTON_POP_DOWN_SQUARE:
            case BUTTON_POP_UP_GRADIENT:
            case BUTTON_POP_DOWN_GRADIENT:
                return Size.REGULAR;
        }

        return size;
    }

    protected Font getDefaultFont(LayoutConfiguration g) {
        Font font = comboBox.getFont();

        Object widget = getWidget(g);
        if (widget != null) {
            return AquaButtonExtendedTypes.getFont(font, widget, sizeVariant);
        }

        return font;
    }

    protected Object getWidget(LayoutConfiguration g) {
        if (g instanceof ComboBoxLayoutConfiguration) {
            ComboBoxLayoutConfiguration bg = (ComboBoxLayoutConfiguration) g;
            return bg.getWidget();
        } else if (g instanceof PopupButtonLayoutConfiguration) {
            PopupButtonLayoutConfiguration bg = (PopupButtonLayoutConfiguration) g;
            return bg.getPopupButtonWidget();
        } else {
            return null;
        }
    }

    protected ComboBoxWidget getComboBoxWidget() {
        if (cellEditorPolicy.isCellEditor(comboBox)) {
            return BUTTON_COMBO_BOX_CELL;
        }

        if (style != null) {
            switch (style) {
                case "tableHeader":
                case "cell":
                case "borderless":
                    return BUTTON_COMBO_BOX_CELL;
                case "textured":
                    return BUTTON_COMBO_BOX_TEXTURED;
                case "textured-onToolbar":
                    return BUTTON_COMBO_BOX_TEXTURED_TOOLBAR;
            }
        }

        if (isOnToolbar(comboBox)) {
            return BUTTON_COMBO_BOX_TEXTURED_TOOLBAR;
        }

        return BUTTON_COMBO_BOX;
    }

    protected PopupButtonWidget getPopupButtonWidget() {
        if (cellEditorPolicy.isCellEditor(comboBox)) {
            return isPopDown ? BUTTON_POP_DOWN_CELL : BUTTON_POP_UP_CELL;
        }

        if (style != null) {
            switch (style) {
                case "tableHeader":
                case "cell":
                case "borderless":
                    return isPopDown ? BUTTON_POP_DOWN_CELL : BUTTON_POP_UP_CELL;
                case "square":
                    // Gradient is the new Square
                    return isPopDown ? BUTTON_POP_DOWN_GRADIENT : BUTTON_POP_UP_GRADIENT;
                case "old_square":
                    // Old API gets the old style (if available)
                    return isPopDown ? BUTTON_POP_DOWN_SQUARE : BUTTON_POP_UP_SQUARE;
                case "bevel":
                    return isPopDown ? BUTTON_POP_DOWN_BEVEL : BUTTON_POP_UP_BEVEL;
                case "roundRect":
                    return isPopDown ? BUTTON_POP_DOWN_ROUND_RECT : BUTTON_POP_UP_ROUND_RECT;
                case "recessed":
                    return isPopDown ? BUTTON_POP_DOWN_RECESSED : BUTTON_POP_UP_RECESSED;
                case "textured":
                    return isPopDown ? BUTTON_POP_DOWN_TEXTURED : BUTTON_POP_UP_TEXTURED;
                case "textured-onToolbar":
                    return isPopDown ? BUTTON_POP_DOWN_TEXTURED_TOOLBAR : BUTTON_POP_UP_TEXTURED_TOOLBAR;
                case "gradient":
                    return isPopDown ? BUTTON_POP_DOWN_GRADIENT : BUTTON_POP_UP_GRADIENT;
            }
        }

        if (isOnToolbar(comboBox)) {
            return isPopDown ? BUTTON_POP_DOWN_TEXTURED_TOOLBAR : BUTTON_POP_UP_TEXTURED_TOOLBAR;
        }

        return isPopDown ? BUTTON_POP_DOWN : BUTTON_POP_UP;
    }

    public static boolean isOnToolbar(JComboBox b) {
        Component parent = b.getParent();
        while (parent != null) {
            if (parent instanceof JToolBar) {
                return true;
            }
            parent = parent.getParent();
        }
        return false;
    }

    public Color getForeground() {
        return getForeground(false);
    }

    protected Color getForeground(boolean isIcon) {
        if (comboBox == null) {
            return super.getForeground();
        }

        Color existingColor = comboBox.getForeground();
        if (existingColor == null || existingColor instanceof UIResource) {
            Object widget = getWidget(getLayoutConfiguration());
            AquaButtonExtendedTypes.WidgetInfo info = AquaButtonExtendedTypes.getWidgetInfo(widget);
            State state = getState();
            Color c = info.getForeground(state, AquaUIPainter.ButtonState.STATELESS, colorDefaults, false, isIcon);
            if (c != null) {
                return c;
            }
        }

        return existingColor;
    }

    /**
     * Modify the title icon if necessary based on the combo box (button) style and state.
     * @param icon The supplied icon.
     * @return the icon to use.
     */
    public Icon getIcon(Icon icon) {
        State st = getState();

        if (icon instanceof ImageIcon) {
            ImageIcon ii = (ImageIcon) icon;
            if (isTemplateIconEnabled(ii)) {
                Color color = getForeground(true);
                if (color != null) {
                    Image im = ii.getImage();
                    im = AquaImageFactory.createImageFromTemplate(im, color);
                    if (im != null) {
                        return new ImageIconUIResource(im);
                    }
                }
            }
        }

        if (st == State.PRESSED) {
            return AquaIcon.createPressedDarkIcon(icon);
        }
        if (st == State.DISABLED || st == State.DISABLED_INACTIVE) {
            return AquaIcon.createDisabledLightIcon(icon);
        }

        return icon;
    }

    private boolean isTemplateIconEnabled(ImageIcon ii) {
        if (ii == lastTestedIcon) {
            return lastTestedIconIsTemplate;
        }
        lastTestedIcon = ii;
        lastTestedIconIsTemplate = AquaImageFactory.isTemplateImage(ii.getImage());
        return lastTestedIconIsTemplate;
    }

    protected State getState() {
        boolean isActive = AquaFocusHandler.isActive(comboBox);

        if (!comboBox.isEnabled()) {
            return isActive ? State.DISABLED : State.DISABLED_INACTIVE;
        }

        if (!isActive) {
            return State.INACTIVE;
        }

        ButtonModel model = getModel();
        if (model.isArmed() && model.isPressed()) {
            return State.PRESSED;
        }

        if (isRollover) {
            return State.ROLLOVER;
        }

        return State.ACTIVE;
    }

    @Override
    public void paintComponent(final Graphics g) {

        // If this is a non-editable or a cell combo box, we paint the border here.
        // Otherwise, we let the combo box do the painting.

        boolean isCell = AquaCellEditorPolicy.getInstance().isCellEditor(comboBox);
        boolean isEditable = comboBox.isEditable();

        if (isEditable && !isCell) {
            return;
        }

        int width = getWidth();
        int height = getHeight();

        if (height <= 0 || width <= 0) {
            return;
        }

        if (comboBox.isOpaque() || isCell && !isEditable && !(comboBox.getParent() instanceof CellRendererPane)) {
            // If the combo box is a cell editor but it is not editable, then it should paint the background because
            // there is no combo box editor to do so. Without the background, the text will probably look wrong, because
            // cell editors are not configured with colors by the container the way cell renderers are. I think it is a
            // bug in BasicTableUI that makes this situation an issue. Normally, when a combo box cell editor is opened,
            // it will bring up a menu and the editor will be closed when the user dismisses the menu. However, if the
            // user clicks in the cell separation, the cell editor will be opened but the mouse event is not delivered,
            // so the combo box editor is left in place.
            g.setColor(getBackground());
            g.fillRect(0, 0, width, height);
        }

        Configuration bg = getConfiguration();
        if (bg != null) {
            painter.configure(width, height);
            Painter p = painter.getPainter(bg);
            p.paint(g, 0, 0);
            if (!isEditable) {
                ui.paintValue(g);
            }
        }
    }

    public Configuration getConfiguration() {

        State state = getState();

        LayoutConfiguration g = getLayoutConfiguration();
        if (g instanceof PopupButtonLayoutConfiguration) {
            PopupButtonLayoutConfiguration bg = (PopupButtonLayoutConfiguration) g;
            return new PopupButtonConfiguration(bg, state);

        } else if (g instanceof ComboBoxLayoutConfiguration) {
            ComboBoxLayoutConfiguration bg = (ComboBoxLayoutConfiguration) g;
            boolean isFocused = false; // comboBox.hasFocus() || comboBox.getEditor().getEditorComponent().hasFocus();
            return new ComboBoxConfiguration(bg, state, isFocused);
        } else {
            return null;
        }
    }

    static class AquaComboBoxHierarchyListener implements HierarchyListener {
        // A hierarchy change may indicate a change in the combo box style (cell or not or toolbar or not)
        public void hierarchyChanged(HierarchyEvent ev) {
            if ((ev.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0) {
                Object o = ev.getSource();
                if (o instanceof AquaComboBoxButton) {
                    AquaComboBoxButton b = (AquaComboBoxButton) o;
                    b.configure(null);
                }
            }
        }
    }

    private class RolloverMouseListener extends MouseAdapter {
        @Override
        public void mouseEntered(MouseEvent e) {
            isRollover = true;
            comboBox.repaint();
        }

        @Override
        public void mouseExited(MouseEvent e) {
            isRollover = false;
            comboBox.repaint();
        }
    }
 }
