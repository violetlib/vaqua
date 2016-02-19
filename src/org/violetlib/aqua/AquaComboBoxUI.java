/*
 * Copyright (c) 2015-2016 Alan Snyder.
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
import java.awt.event.*;
import java.awt.geom.RoundRectangle2D;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleState;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicComboBoxEditor;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.ComboPopup;
import javax.swing.text.JTextComponent;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.ClientPropertyApplicator.Property;
import org.violetlib.geom.ExpandableOutline;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;

import static org.violetlib.jnr.aqua.AquaUIPainter.PopupButtonWidget.*;

// Inspired by MetalComboBoxUI, which also has a combined text-and-arrow button for noneditables
public class AquaComboBoxUI extends BasicComboBoxUI implements AquaUtilControlSize.Sizeable, FocusRingOutlineProvider {
    public static ComponentUI createUI(final JComponent c) {
        return new AquaComboBoxUI();
    }

    // A JComboBox maps to two different kinds of views depending upon whether the JComboBox is editable or not. This
    // code is complex because the editable attribute can be changed on the fly. A non-editable JComboBox maps to a
    // popup button, which shares many characteristics with ordinary buttons.

    public static final String POPDOWN_CLIENT_PROPERTY_KEY = "JComboBox.isPopDown"; // legacy from Aqua LAF
    public static final String ISSQUARE_CLIENT_PROPERTY_KEY = "JComboBox.isSquare"; // legacy from Aqua LAF
    public static final String STYLE_CLIENT_PROPERTY_KEY = "JComboBox.style";
    public static final String TITLE_CLIENT_PROPERTY_KEY = "JComboBox.title";

    private static AquaUIPainter painter = AquaPainting.create();
    private static AquaCellEditorPolicy cellEditorPolicy = AquaCellEditorPolicy.getInstance();
    private static OptionallyFocusableComponentHandler focusHandler = new MyOptionalFocusHandler();

    private int oldMaximumRowCount;
    protected Size sizeVariant;
    protected Dimension cachedPreferredSize = new Dimension( 0, 0 );
    protected AquaComboBoxButton arrowButton;

    public void installUI(final JComponent c) {
        super.installUI(c);

        LookAndFeel.installProperty(c, "opaque", false);
        oldMaximumRowCount = comboBox.getMaximumRowCount();
        int maximumRows = UIManager.getInt("ComboBox.maximumRowCount");
        if (maximumRows > 0) {
            comboBox.setMaximumRowCount(maximumRows);
        }
        comboBox.setRequestFocusEnabled(false);
        //comboBox.putClientProperty(DEFAULT_FONT_PROPERTY, comboBox.getFont());
        configure(null);
    }

    public void uninstallUI(final JComponent c) {
        comboBox.setMaximumRowCount(oldMaximumRowCount);
        super.uninstallUI(c);
    }

    protected void installListeners() {
        super.installListeners();
        AquaUtilControlSize.addSizePropertyListener(comboBox);

        // An editable combo box is normally focusable.
        // A non-editable combo box (a pull down or pop menu button) is focusable only if Full Keyboard Access
        // is enabled.
        // Because editability is a dynamic attribute, we install the change listener unconditionally, but make its
        // effect conditional on the editability of the combo box.

        comboBox.putClientProperty(AquaFullKeyboardFocusableHandler.OPTIONAL_FOCUSABILITY_HANDLER_KEY, focusHandler);
        AquaFullKeyboardFocusableHandler.addListener(comboBox);
    }

    protected void uninstallListeners() {
        AquaUtilControlSize.removeSizePropertyListener(comboBox);
        AquaFullKeyboardFocusableHandler.removeListener(comboBox);
        super.uninstallListeners();
    }

    protected void installComponents() {
        super.installComponents();

        // client properties must be applied after the components have been installed,
        // because isSquare and isPopdown are applied to the installed button
        getApplicator().attachAndApplyClientProperties(comboBox);
    }

    protected void uninstallComponents() {
        getApplicator().removeFrom(comboBox);
        super.uninstallComponents();
    }

    private static class MyOptionalFocusHandler implements OptionallyFocusableComponentHandler {
        @Override
        public void updateFocusability(JComponent c, boolean isFocusable) {
            JComboBox cb = (JComboBox) c;
            if (!cb.isEditable()) {
                cb.setFocusable(isFocusable);
            }
        }
    }

    protected ItemListener createItemListener() {
        return new ItemListener() {
            long lastBlink = 0L;
            public void itemStateChanged(final ItemEvent e) {
                if (e.getStateChange() != ItemEvent.SELECTED) return;
                if (!popup.isVisible()) return;

                // sometimes, multiple selection changes can occur while the popup is up,
                // and blinking more than "once" (in a second) is not desirable
                final long now = System.currentTimeMillis();
                if (now - 1000 < lastBlink) return;
                lastBlink = now;

                final JList<Object> itemList = popup.getList();
                final ListUI listUI = itemList.getUI();
                if (!(listUI instanceof AquaListUI)) return;
                final AquaListUI aquaListUI = (AquaListUI)listUI;

                final int selectedIndex = comboBox.getSelectedIndex();
                final ListModel<Object> dataModel = itemList.getModel();
                if (dataModel == null) return;

                final Object value = dataModel.getElementAt(selectedIndex);
                AquaUtils.blinkMenu(new AquaUtils.Selectable() {
                    public void paintSelected(final boolean selected) {
                        aquaListUI.repaintCell(value, selectedIndex, selected);
                    }
                });
            }
        };
    }

    @Override
    public void addEditor() {
        super.addEditor();
        isMinimumSizeDirty = true;  // workaround for bug in BasicComboBoxUI, editable change does not set this flag
    }

    @Override
    public void removeEditor() {
        super.removeEditor();
        isMinimumSizeDirty = true;  // workaround for bug in BasicComboBoxUI, editable change does not set this flag
    }

    public void paint(final Graphics g, final JComponent c) {
        // If this is an editable combo box with a border, we paint the component background here.
        // Otherwise, we let the button do the painting.

        final boolean editable = comboBox.isEditable();
        final boolean isCell = cellEditorPolicy.isCellEditor(comboBox);

        if (isCell) {
            // Our foreground color may have been configured for a particular cell.
            // Transfer that color to the text field so that it paints the text using that color.
            Component ed = comboBox.getEditor().getEditorComponent();
            ed.setForeground(c.getForeground());
        } else if (editable && arrowButton != null) {
            Configuration bg = arrowButton.getConfiguration();
            painter.configure(c.getWidth(), c.getHeight());
            painter.getPainter(bg).paint(g, 0, 0);
        }
    }

    protected AbstractComboBoxLayoutConfiguration getLayoutConfiguration() {
        return arrowButton != null ? arrowButton.getLayoutConfiguration() : null;
    }

    @Override
    public Shape getFocusRingOutline(JComponent c) {
        LayoutConfiguration g = getLayoutConfiguration();
        if (g != null) {
            painter.configure(c.getWidth(), c.getHeight());
            return painter.getOutline(g);
        } else {
            return null;
        }
    }

//    /**
//     * Return the default font for a combo box independent of the style and configuration.
//     */
//    public static Font getGenericDefaultFont(JComboBox b) {
//        Font f = (Font) b.getClientProperty(DEFAULT_FONT_PROPERTY);
//        if (f != null) {
//            return f;
//        }
//        return b.getFont();
//    }

    public void paintValue(final Graphics g) {
        final ListCellRenderer<Object> renderer = comboBox.getRenderer();

        Object displayedItem = null;

        AquaComboBoxType type = getComboBoxType(comboBox);
        if (type == AquaComboBoxType.PULL_DOWN_MENU_BUTTON) {
            Object value = comboBox.getClientProperty(TITLE_CLIENT_PROPERTY_KEY);
            if (value != null) {
                displayedItem = value;
            }
        } else {
            displayedItem = comboBox.getSelectedItem();
        }

        // fake it out! not renderPressed
        final Component c = renderer.getListCellRendererComponent(listBox, displayedItem, -1, false, false);
        // System.err.println("Renderer: " + renderer);

        int top = 0;
        int left = 0;
        int width = comboBox.getWidth();
        int height = comboBox.getHeight();

        LayoutConfiguration bg = getLayoutConfiguration();
        if (bg != null) {
            Rectangle bounds = getContentBounds(bg);
            if (bounds != null) {
                left = bounds.x;
                top = bounds.y;
                width = bounds.width;
                height = bounds.height;
            }
        }

        c.setFont(currentValuePane.getFont());

        Color foreground = arrowButton.getForeground();

        c.setForeground(foreground);

        // Sun Fix for 4238829: should lay out the JPanel.
        boolean shouldValidate = false;
        if (c instanceof JPanel) {
            shouldValidate = true;
        }

//        final int iconWidth = 0;
//        final int cWidth = width - (insets.right + iconWidth);
//
//        // fix for 3156483 we need to crop images that are too big.
//        // if (height > 18)
//        // always crop.
//        {
//            top = height / 2 - 8;
//            height = 19;
//        }

        // It doesn't need to draw its background, we handled it
        final Color background = c.getBackground();
            c.setBackground(new Color(0, 0, 0, 0));

        if (padding != null) {
            left += padding.left;
            top += padding.top;
            width -= padding.left;  // do not use right padding here, if we need the room we should use it
            height -= padding.top;  // do not use bottom padding here, if we need the room we should use it
        }

        currentValuePane.paintComponent(g, c, comboBox, left, top, width, height, shouldValidate);

        c.setBackground(background);
    }

    protected Rectangle getContentBounds(LayoutConfiguration g) {
        painter.configure(comboBox.getWidth(), comboBox.getHeight());
        if (g instanceof ComboBoxLayoutConfiguration) {
            return AquaUtils.toMinimumRectangle(painter.getComboBoxEditorBounds((ComboBoxLayoutConfiguration) g));
        } else if (g instanceof PopupButtonLayoutConfiguration) {
            return AquaUtils.toMinimumRectangle(painter.getPopupButtonContentBounds((PopupButtonLayoutConfiguration) g));
        } else {
            return null;
        }
    }

    protected Insetter getContentInsets(LayoutConfiguration g) {
        if (g instanceof ComboBoxLayoutConfiguration) {
            return painter.getLayoutInfo().getComboBoxEditorInsets((ComboBoxLayoutConfiguration) g);
        } else if (g instanceof PopupButtonLayoutConfiguration) {
            return painter.getLayoutInfo().getPopupButtonContentInsets((PopupButtonLayoutConfiguration) g);
        } else {
            return null;
        }
    }

    protected ListCellRenderer<Object> createRenderer() {
        return new AquaComboBoxRenderer(comboBox);
    }

    protected ComboPopup createPopup() {
        return new AquaComboBoxPopup(comboBox);
    }

    protected JButton createArrowButton() {
        return arrowButton = new AquaComboBoxButton(this, comboBox, currentValuePane, listBox);
    }

    protected ComboBoxEditor createEditor() {
        return new AquaComboBoxEditor();
    }

    @Override
    protected void configureEditor() {
        super.configureEditor();

        // We now know the combo box is editable.

        comboBox.setFocusable(true);

        if (editor instanceof JTextField && !(editor instanceof AquaCustomComboTextField)) {
            JTextField tf = (JTextField) editor;
            tf.setUI(new AquaComboBoxEditorUI());
            tf.setBorder(null);
        }
    }

    @Override
    protected void unconfigureEditor() {
        super.unconfigureEditor();

        // Either we now know that the combo box is not editable, or the UI is being uninstalled.

        if (!comboBox.isEditable()) {
            comboBox.setFocusable(OSXSystemProperties.isFullKeyboardAccessEnabled());
        }
    }

    public boolean updateListSelectionFromEditor() {
        if (editor instanceof JTextField) {
            JTextField tf = (JTextField) editor;
            updateListSelectionFromEditor(tf);
            return true;
        }
        return false;
    }

    protected void updateListSelectionFromEditor(JTextField editor) {
        String text = editor.getText();
        ListModel<Object> model = listBox.getModel();
        int items = model.getSize();
        for (int i = 0; i < items; i++) {
            Object element = model.getElementAt(i);
            if (element == null) continue;

            String asString = element.toString();
            if (asString == null || !asString.equals(text)) continue;

            popup.getList().setSelectedIndex(i);
            return;
        }

        popup.getList().clearSelection();
    }

    final class AquaComboBoxEditorUI extends AquaTextFieldUI implements FocusRingOutlineProvider {

        @Override
        protected void installDefaults() {
            super.installDefaults();
            JTextComponent c = getComponent();
            LookAndFeel.installProperty(c, "opaque", true);
            Border b = c.getBorder();
            if (b == null || b instanceof UIDefaults) {
                c.setBorder(null);
            }
        }

        @Override
        protected void paintBackgroundSafely(Graphics g) {
            if (comboBox.getParent() instanceof CellRendererPane) {
                return;
            }

            if (arrowButton != null) {
                ComboBoxConfiguration bg = (ComboBoxConfiguration) arrowButton.getConfiguration();
                if (bg.getState() == AquaUIPainter.State.INACTIVE) {
                    AquaUIPainter.ComboBoxWidget w = bg.getWidget();
                    if (w == AquaUIPainter.ComboBoxWidget.BUTTON_COMBO_BOX_TEXTURED || w == AquaUIPainter.ComboBoxWidget.BUTTON_COMBO_BOX_TEXTURED_TOOLBAR) {
                        // Textured combo boxes change background color when inactive
                        int width = editor.getWidth();
                        int height = editor.getHeight();
                        Color c = new Color(245, 245, 245);
                        g.setColor(c);
                        g.fillRect(0, 0, width, height);
                        return;
                    }
                }
            }

            super.paintBackgroundSafely(g);
        }

        @Override
        public Shape getFocusRingOutline(JComponent c) {
            Component parent = c.getParent();
            if (parent instanceof JComboBox) {
                JComboBox cb = (JComboBox) parent;

                // The focus ring for a combo box goes around the entire combo box, not the text field.

                AquaComboBoxUI ui = AquaUtils.getUI(cb, AquaComboBoxUI.class);
                if (ui != null) {
                    // Translate the shape into the text field coordinate space.
                    // Can't use AffineTransform.createTransformedShape() because that probably returns a Path,
                    // losing useful information.
                    Shape s = ui.getFocusRingOutline(cb);
                    Rectangle textFieldBounds = c.getBounds();
                    return ExpandableOutline.createTranslatedShape(s, -textFieldBounds.x, -textFieldBounds.y);
                }

                int width = cb.getWidth();
                int height = cb.getHeight();
                Rectangle textFieldBounds = c.getBounds();
                int x = -textFieldBounds.x;
                int y = -textFieldBounds.y;
                return new RoundRectangle2D.Double(x+AquaButtonUI.OUTLINE_OFFSET, y+AquaButtonUI.OUTLINE_OFFSET, width-2*AquaButtonUI.OUTLINE_OFFSET, height-2*AquaButtonUI.OUTLINE_OFFSET,
                        AquaButtonUI.OUTLINE_CORNER, AquaButtonUI.OUTLINE_CORNER);
            }

            return null;
        }
    }

    final class AquaComboBoxEditor extends BasicComboBoxEditor implements UIResource, DocumentListener {

        AquaComboBoxEditor() {
            editor.addFocusListener(this);
            editor.getDocument().addDocumentListener(this);
        }

        @Override
        protected JTextField createEditorComponent() {
            return new AquaCustomComboTextField();
        }

        @Override
        public void focusGained(final FocusEvent e) {
            if (arrowButton != null) {
                arrowButton.repaint();
            }
        }

        @Override
        public void focusLost(final FocusEvent e) {
            if (arrowButton != null) {
                arrowButton.repaint();
            }
        }

        @Override
        public void changedUpdate(final DocumentEvent e) {
            editorTextChanged();
        }

        @Override
        public void insertUpdate(final DocumentEvent e) {
            editorTextChanged();
        }

        @Override
        public void removeUpdate(final DocumentEvent e) {
            editorTextChanged();
        }

        private void editorTextChanged() {
            if (popup.isVisible()) {
                updateListSelectionFromEditor(editor);
            }
        }
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    class AquaCustomComboTextField extends JTextField {
        @SuppressWarnings("serial") // anonymous class
        public AquaCustomComboTextField() {

            setUI(new AquaComboBoxEditorUI());

            setBorder(null);

            // TBD: there should be some space at the left and right ends of the text, but JTextField does not support that

            //setBackground(new Color(255, 200, 0, 128)); // debug

            final InputMap inputMap = getInputMap();
            inputMap.put(KeyStroke.getKeyStroke("DOWN"), highlightNextAction);
            inputMap.put(KeyStroke.getKeyStroke("KP_DOWN"), highlightNextAction);
            inputMap.put(KeyStroke.getKeyStroke("UP"), highlightPreviousAction);
            inputMap.put(KeyStroke.getKeyStroke("KP_UP"), highlightPreviousAction);

            inputMap.put(KeyStroke.getKeyStroke("HOME"), highlightFirstAction);
            inputMap.put(KeyStroke.getKeyStroke("END"), highlightLastAction);
            inputMap.put(KeyStroke.getKeyStroke("PAGE_UP"), highlightPageUpAction);
            inputMap.put(KeyStroke.getKeyStroke("PAGE_DOWN"), highlightPageDownAction);

            final Action action = getActionMap().get(JTextField.notifyAction);
            inputMap.put(KeyStroke.getKeyStroke("ENTER"), new AbstractAction() {
                public void actionPerformed(final ActionEvent e) {
                    if (popup.isVisible()) {
                        triggerSelectionEvent(comboBox, e);

                        if (editor instanceof AquaCustomComboTextField) {
                            ((AquaCustomComboTextField)editor).selectAll();
                        }
                    } else {
                        action.actionPerformed(e);
                    }
                }
            });
        }

        // workaround for 4530952
        public void setText(final String s) {
            if (getText().equals(s)) {
                return;
            }
            super.setText(s);
        }
    }

    /**
     * This listener hides the popup when the focus is lost.  It also repaints
     * when focus is gained or lost.
     *
     * This override is necessary because the Basic L&F for the combo box is working
     * around a Solaris-only bug that we don't have on Mac OS X.  So, remove the lightweight
     * popup check here. rdar://Problem/3518582
     */
    protected FocusListener createFocusListener() {
        return new BasicComboBoxUI.FocusHandler() {
            public void focusLost(final FocusEvent e) {
                hasFocus = false;
                if (!e.isTemporary()) {
                    setPopupVisible(comboBox, false);
                }
                comboBox.repaint();

                // Notify assistive technologies that the combo box lost focus
                final AccessibleContext ac = ((Accessible)comboBox).getAccessibleContext();
                if (ac != null) {
                    ac.firePropertyChange(AccessibleContext.ACCESSIBLE_STATE_PROPERTY, AccessibleState.FOCUSED, null);
                }
            }
        };
    }

    protected void installKeyboardActions() {
        super.installKeyboardActions();

        ActionMap actionMap = new ActionMapUIResource();

        actionMap.put("aquaSelectNext", highlightNextAction);
        actionMap.put("aquaSelectPrevious", highlightPreviousAction);
        actionMap.put("enterPressed", triggerSelectionAction);
        actionMap.put("aquaSpacePressed", toggleSelectionAction);

        actionMap.put("aquaSelectHome", highlightFirstAction);
        actionMap.put("aquaSelectEnd", highlightLastAction);
        actionMap.put("aquaSelectPageUp", highlightPageUpAction);
        actionMap.put("aquaSelectPageDown", highlightPageDownAction);

        actionMap.put("aquaHidePopup", hideAction);

        SwingUtilities.replaceUIActionMap(comboBox, actionMap);
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    private abstract class ComboBoxAction extends AbstractAction {
        public void actionPerformed(final ActionEvent e) {
            if (!comboBox.isEnabled() || !comboBox.isShowing()) {
                return;
            }

            if (comboBox.isPopupVisible()) {
                final AquaComboBoxUI ui = (AquaComboBoxUI)comboBox.getUI();
                performComboBoxAction(ui);
            } else {
                comboBox.setPopupVisible(true);
            }
        }

        abstract void performComboBoxAction(final AquaComboBoxUI ui);
    }

    /**
     * Hilight _but do not select_ the next item in the list.
     */
    @SuppressWarnings("serial") // anonymous class
    private Action highlightNextAction = new ComboBoxAction() {
        @Override
        public void performComboBoxAction(AquaComboBoxUI ui) {
            final int si = listBox.getSelectedIndex();

            if (si < comboBox.getModel().getSize() - 1) {
                listBox.setSelectedIndex(si + 1);
                listBox.ensureIndexIsVisible(si + 1);
            }
            comboBox.repaint();
        }
    };

    /**
     * Hilight _but do not select_ the previous item in the list.
     */
    @SuppressWarnings("serial") // anonymous class
    private Action highlightPreviousAction = new ComboBoxAction() {
        @Override
        void performComboBoxAction(final AquaComboBoxUI ui) {
            final int si = listBox.getSelectedIndex();
            if (si > 0) {
                listBox.setSelectedIndex(si - 1);
                listBox.ensureIndexIsVisible(si - 1);
            }
            comboBox.repaint();
        }
    };

    @SuppressWarnings("serial") // anonymous class
    private Action highlightFirstAction = new ComboBoxAction() {
        @Override
        void performComboBoxAction(final AquaComboBoxUI ui) {
            listBox.setSelectedIndex(0);
            listBox.ensureIndexIsVisible(0);
        }
    };

    @SuppressWarnings("serial") // anonymous class
    private Action highlightLastAction = new ComboBoxAction() {
        @Override
        void performComboBoxAction(final AquaComboBoxUI ui) {
            final int size = listBox.getModel().getSize();
            listBox.setSelectedIndex(size - 1);
            listBox.ensureIndexIsVisible(size - 1);
        }
    };

    @SuppressWarnings("serial") // anonymous class
    private Action highlightPageUpAction = new ComboBoxAction() {
        @Override
        void performComboBoxAction(final AquaComboBoxUI ui) {
            final int current = listBox.getSelectedIndex();
            final int first = listBox.getFirstVisibleIndex();

            if (current != first) {
                listBox.setSelectedIndex(first);
                return;
            }

            final int page = listBox.getVisibleRect().height / listBox.getCellBounds(0, 0).height;
            int target = first - page;
            if (target < 0) target = 0;

            listBox.ensureIndexIsVisible(target);
            listBox.setSelectedIndex(target);
        }
    };

    @SuppressWarnings("serial") // anonymous class
    private Action highlightPageDownAction = new ComboBoxAction() {
        @Override
        void performComboBoxAction(final AquaComboBoxUI ui) {
            final int current = listBox.getSelectedIndex();
            final int last = listBox.getLastVisibleIndex();

            if (current != last) {
                listBox.setSelectedIndex(last);
                return;
            }

            final int page = listBox.getVisibleRect().height / listBox.getCellBounds(0, 0).height;
            final int end = listBox.getModel().getSize() - 1;
            int target = last + page;
            if (target > end) target = end;

            listBox.ensureIndexIsVisible(target);
            listBox.setSelectedIndex(target);
        }
    };

    // For <rdar://problem/3759984> Java 1.4.2_5: Serializing Swing components not working
    // Inner classes were using a this reference and then trying to serialize the AquaComboBoxUI
    // We shouldn't do that. But we need to be able to get the popup from other classes, so we need
    // a public accessor.
    public ComboPopup getPopup() {
        return popup;
    }

    protected LayoutManager createLayoutManager() {
        return new AquaComboBoxLayoutManager();
    }

    class AquaComboBoxLayoutManager extends BasicComboBoxUI.ComboBoxLayoutManager {

        public void layoutContainer(final Container parent) {
            final int width = comboBox.getWidth();
            final int height = comboBox.getHeight();

            if (comboBox.isEditable()) {
                ComboBoxConfiguration g = (ComboBoxConfiguration) arrowButton.getConfiguration();
                painter.configure(width, height);
                if (editor != null) {
                    Rectangle editorBounds = AquaUtils.toMinimumRectangle(painter.getComboBoxEditorBounds(g));
                    editor.setBounds(editorBounds);
                }
                if (arrowButton != null) {
                    Rectangle arrowBounds = AquaUtils.toMinimumRectangle(painter.getComboBoxIndicatorBounds(g));
                    arrowButton.setBounds(arrowBounds);
                }
            } else {
                arrowButton.setBounds(0, 0, width, height);
            }
        }
    }

    public static AquaComboBoxType getComboBoxType(final JComboBox<?> c) {
        if (c.isEditable()) {
            return AquaComboBoxType.EDITABLE_COMBO_BOX;
        } else if (Boolean.TRUE.equals(c.getClientProperty(AquaComboBoxUI.POPDOWN_CLIENT_PROPERTY_KEY))) {
            return AquaComboBoxType.PULL_DOWN_MENU_BUTTON;
        } else {
            return AquaComboBoxType.POP_UP_MENU_BUTTON;
        }
    }

    protected static void triggerSelectionEvent(final JComboBox<?> comboBox, final ActionEvent e) {
        if (!comboBox.isEnabled()) return;

        final AquaComboBoxUI aquaUi = (AquaComboBoxUI)comboBox.getUI();

        if (aquaUi.getPopup().getList().getSelectedIndex() < 0) {
            comboBox.setPopupVisible(false);
        }

        if (cellEditorPolicy.isCellEditor(comboBox)) {
            // The original code below from AquaComboBoxUI has the effect of setting the text to the empty string. Seems
            // like a bug. Instead, do what BasicComboBoxUI does:

            comboBox.setSelectedItem(comboBox.getSelectedItem());

//            // Forces the selection of the list item if the combo box is in a JTable
//            comboBox.setSelectedIndex(aquaUi.getPopup().getList().getSelectedIndex());

            return;
        }

        if (comboBox.isPopupVisible()) {
            comboBox.setSelectedIndex(aquaUi.getPopup().getList().getSelectedIndex());
            comboBox.setPopupVisible(false);
            return;
        }

        // Call the default button binding.
        // This is a pretty messy way of passing an event through to the root pane
        final JRootPane root = SwingUtilities.getRootPane(comboBox);
        if (root == null) return;

        final InputMap im = root.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        final ActionMap am = root.getActionMap();
        if (im == null || am == null) return;

        final Object obj = im.get(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0));
        if (obj == null) return;

        final Action action = am.get(obj);
        if (action == null) return;

        action.actionPerformed(new ActionEvent(root, e.getID(), e.getActionCommand(), e.getWhen(), e.getModifiers()));
    }

    // This is somewhat messy.  The difference here from BasicComboBoxUI.EnterAction is that
    // arrow up or down does not automatically select the
    @SuppressWarnings("serial") // anonymous class
    private final Action triggerSelectionAction = new AbstractAction() {
        public void actionPerformed(final ActionEvent e) {
            triggerSelectionEvent((JComboBox)e.getSource(), e);
        }

        @Override
        public boolean isEnabled() {
            return comboBox.isPopupVisible() && super.isEnabled();
        }
    };

    @SuppressWarnings("serial") // anonymous class
    private static final Action toggleSelectionAction = new AbstractAction() {
        public void actionPerformed(final ActionEvent e) {
            final JComboBox<?> comboBox = (JComboBox<?>) e.getSource();
            if (!comboBox.isEnabled()) return;
            if (comboBox.isEditable()) return;

            final AquaComboBoxUI aquaUi = (AquaComboBoxUI)comboBox.getUI();

            if (comboBox.isPopupVisible()) {
                comboBox.setSelectedIndex(aquaUi.getPopup().getList().getSelectedIndex());
                comboBox.setPopupVisible(false);
                return;
            }

            comboBox.setPopupVisible(true);
        }
    };

    @SuppressWarnings("serial") // anonymous class
    private final Action hideAction = new AbstractAction() {
        @Override
        public void actionPerformed(final ActionEvent e) {
            final JComboBox<?> comboBox = (JComboBox<?>) e.getSource();
            comboBox.firePopupMenuCanceled();
            comboBox.setPopupVisible(false);
        }

        @Override
        public boolean isEnabled() {
            return comboBox.isPopupVisible() && super.isEnabled();
        }
    };

    public void applySizeFor(JComponent c, Size size, boolean isDefaultSize) {
        sizeVariant = size;
        if (isDefaultSize) {
            size = determineDefaultSize(size);
        }

        configure(size);
    }

    /**
     * Return the effective size variant. The effective size variant may differ from the specified size variant if the
     * selected style does not support the specified size variant.
     */
    public Size getSizeVariant() {
        if (sizeVariant == null) {
            return Size.REGULAR;
        }

        if (sizeVariant != Size.REGULAR) {
            AbstractComboBoxLayoutConfiguration g = getLayoutConfiguration();
            if (g != null) {
                // TBD: ideally, should get this information from the LayoutInfo
                Size size = g.getSize();
                if (size == Size.MINI) {
                    AquaUIPainter.PopupButtonWidget w = getPopupButtonWidget();
                    if (w == BUTTON_POP_UP_SQUARE || w == BUTTON_POP_DOWN_SQUARE || w == BUTTON_POP_UP_CELL || w == BUTTON_POP_DOWN_CELL) {
                        size = Size.SMALL;
                    }
                }
                return size;
            }
        }

        return sizeVariant;
    }

    protected Size determineDefaultSize(Size size) {
        if (size == Size.REGULAR && cellEditorPolicy.isCellEditor(comboBox) && comboBox.getHeight() < 16) {
            return Size.SMALL;
        }
        return size;
    }

    public Dimension getMinimumSize(final JComponent c) {
        if (isMinimumSizeDirty) {
            calculateLayoutSizes();
        }
        return new Dimension(cachedMinimumSize);
    }

    public Dimension getPreferredSize(final JComponent c) {
        if (isMinimumSizeDirty) {
            calculateLayoutSizes();
        }
        return new Dimension(cachedPreferredSize);
    }

    protected void calculateLayoutSizes() {
        AbstractComboBoxLayoutConfiguration g = getLayoutConfiguration();
        LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
        int fixedRenderingHeight = (int) layoutInfo.getFixedVisualHeight();
        // The fixed rendering height is a minimum height.
        // It is the preferred height for all combo boxes except cell styles, where we add some extra space.
        // The native renderer will vertically center the cell arrows in the requested space.

        int minimumHeight = fixedRenderingHeight;
        int preferredHeight = fixedRenderingHeight;

        Dimension size = null;

        if (arrowButton != null) {
            if (g.isCell()) {
                preferredHeight += 6;
            }

            Insetter s = getContentInsets(g);
            if (s != null) {
                Dimension displaySize = getDisplaySize();
                size = s.expand(displaySize);
            }
        }

        if (size == null) {
            boolean editable = comboBox.isEditable();
            if (editable && arrowButton != null && editor != null) {
                size = super.getMinimumSize(comboBox);
                final Insets margin = arrowButton.getMargin();
                size.height += margin.top + margin.bottom;
            } else {
                size = super.getMinimumSize(comboBox);
            }
        }

        if (size.height > minimumHeight) {
            minimumHeight = size.height;
        }

        if (size.height > preferredHeight) {
            preferredHeight = size.height;
        }

        cachedMinimumSize.setSize(size.width, minimumHeight);
        cachedPreferredSize.setSize(size.width, preferredHeight);
        isMinimumSizeDirty = false;
    }

    // Overridden to use the proper renderer
    @Override
    protected Dimension getDefaultSize() {
        ListCellRenderer r = comboBox.getRenderer();
        if (r == null)  {
            r = new DefaultListCellRenderer();
        }
        Dimension d = getSizeForComponent(r.getListCellRendererComponent(listBox, " ", -1, false, false));
        return new Dimension(d.width, d.height);
    }

    /**
     * Style related configuration affecting layout
     *
     * @param size Optional new size variant to install
     */
    protected void configure(Size size) {
        if (arrowButton != null) {
            arrowButton.configure(size);

        } else if (comboBox != null) {
            comboBox.repaint();
        }
    }

    /**
     * Return the offset needed to align a popup menu item label with the combo box button label.
     * @return the offset, or null if none.
     */
    public Point getPopupButtonLabelOffset() {
        // For a pop up menu, the goal is for the menu item label to exactly overlay the combo box button label, at
        // least in the case where our default renderer is used. The correction factors are based on a number of
        // parameters, many of which are not currently accessible. We can get a good approximation with the following
        // values.

        // TBD: calculate exactly based on layout information

        int labelXOffset = 0;
        int labelYOffset = 0;

        AquaComboBoxType type = getComboBoxType(comboBox);
        if (type == AquaComboBoxType.POP_UP_MENU_BUTTON) {
            labelXOffset -= 8;
            labelYOffset = 1;

            AquaUIPainter.PopupButtonWidget w = getPopupButtonWidget();
            if (w != AquaUIPainter.PopupButtonWidget.BUTTON_POP_UP) {
                labelXOffset -= 2;
                labelYOffset = 2;
            }
        }

        return labelXOffset != 0 || labelYOffset != 0 ? new Point(labelXOffset, labelYOffset) : null;
    }

    public AquaUIPainter.PopupButtonWidget getPopupButtonWidget() {
        AbstractComboBoxLayoutConfiguration g = getLayoutConfiguration();
        if (g instanceof PopupButtonLayoutConfiguration) {
            PopupButtonLayoutConfiguration pg = (PopupButtonLayoutConfiguration) g;
            return pg.getPopupButtonWidget();
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    static final RecyclableSingleton<ClientPropertyApplicator<JComboBox<?>, AquaComboBoxUI>> APPLICATOR = new
            RecyclableSingleton<ClientPropertyApplicator<JComboBox<?>, AquaComboBoxUI>>() {
        @Override
        protected ClientPropertyApplicator<JComboBox<?>, AquaComboBoxUI> getInstance() {
            return new ClientPropertyApplicator<JComboBox<?>, AquaComboBoxUI>(
                new Property<AquaComboBoxUI>(AquaFocusHandler.FRAME_ACTIVE_PROPERTY) {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        if (Boolean.FALSE.equals(value)) {
                            if (target.comboBox != null) target.comboBox.hidePopup();
                        }
                        if (target.listBox != null) target.listBox.repaint();
                        if (target.comboBox != null) {
                            target.comboBox.repaint();
                        }
                    }
                },
                new Property<AquaComboBoxUI>("editable") {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        target.configure(null);
                    }
                },
                new Property<AquaComboBoxUI>("background") {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        final Color color = (Color)value;
                        if (target.arrowButton != null) target.arrowButton.setBackground(color);
                        if (target.listBox != null) target.listBox.setBackground(color);
                    }
                },
                new Property<AquaComboBoxUI>("foreground") {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        final Color color = (Color)value;
                        if (target.arrowButton != null) target.arrowButton.setForeground(color);
                        if (target.listBox != null) target.listBox.setForeground(color);
                    }
                },
                new Property<AquaComboBoxUI>(POPDOWN_CLIENT_PROPERTY_KEY) {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        target.configure(null);
                    }
                },
                new Property<AquaComboBoxUI>(ISSQUARE_CLIENT_PROPERTY_KEY) {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        target.configure(null);
                    }
                },
                new Property<AquaComboBoxUI>(STYLE_CLIENT_PROPERTY_KEY) {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        target.configure(null);
                    }
                },
                new Property<AquaComboBoxUI>(TITLE_CLIENT_PROPERTY_KEY) {
                    public void applyProperty(final AquaComboBoxUI target, final Object value) {
                        if (target.comboBox != null) {
                            target.comboBox.setPrototypeDisplayValue(value);
                            target.comboBox.repaint();
                        }
                    }
                }
            ) {
                public AquaComboBoxUI convertJComponentToTarget(final JComboBox<?> combo) {
                    final ComboBoxUI comboUI = combo.getUI();
                    if (comboUI instanceof AquaComboBoxUI) return (AquaComboBoxUI)comboUI;
                    return null;
                }
            };
        }
    };
    static ClientPropertyApplicator<JComboBox<?>, AquaComboBoxUI> getApplicator() {
        return APPLICATOR.get();
    }
}
