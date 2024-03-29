/*
 * Changes Copyright (c) 2015-2023 Alan Snyder.
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
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget;
import org.violetlib.jnr.aqua.TextFieldLayoutConfiguration;

import static org.violetlib.aqua.AquaLabelUI.AQUA_LABEL_ROLE_PROPERTY;
import static org.violetlib.aqua.AquaLabelUI.AQUA_SEARCH_FIELD_PROMPT_ROLE_VALUE;
import static org.violetlib.aqua.OSXSystemProperties.OSVersion;

public class AquaTextFieldSearch {

    private static final String FIND_POPUP_KEY = "JTextField.Search.FindPopup";
    private static final String FIND_ACTION_KEY = "JTextField.Search.FindAction";
    private static final String CANCEL_ACTION_KEY = "JTextField.Search.CancelAction";
    private static final String PROMPT_KEY = "JTextField.Search.Prompt";

    protected static boolean hasPopupMenu(@NotNull JTextComponent c) {
        return (c.getClientProperty(FIND_POPUP_KEY) instanceof JPopupMenu);
    }

    protected static void installSearchField(@NotNull JTextComponent c) {

        uninstallSearchField(c);

        SearchFieldBorder border = new AquaTextFieldSearch.SearchFieldBorder(c);
        c.setBorder(border);
        c.setLayout(border.getCustomLayout());
        c.add(getFindButton(c), BorderLayout.WEST);
        c.add(getCancelButton(c), BorderLayout.EAST);
        c.add(getPromptLabel(c), BorderLayout.CENTER);
    }

    protected static void uninstallSearchField(JTextComponent c) {
        if (c.getBorder() instanceof SearchFieldBorder) {
            c.setBorder(UIManager.getBorder("TextField.border"));
            c.removeAll();
        }
    }

    // The "magnifying glass" icon that sometimes has a downward pointing triangle next to it
    // if a popup has been assigned to it. It does not appear to have a pressed state.
//    protected static DynamicallySizingJRSUIIcon getFindIcon(JTextComponent text) {
//        return (text.getClientProperty(FIND_POPUP_KEY) == null) ?
//            new DynamicallySizingJRSUIIcon(new SizeDescriptor(new SizeVariant(25, 22).alterMargins(0, 4, 0, -5))) {
//                public void initJRSUIState() {
//                    painter.state.set(Widget.BUTTON_SEARCH_FIELD_FIND);
//                }
//            }
//        :
//            new DynamicallySizingJRSUIIcon(new SizeDescriptor(new SizeVariant(25, 22).alterMargins(0, 4, 0, 2))) {
//                public void initJRSUIState() {
//                    painter.state.set(Widget.BUTTON_SEARCH_FIELD_FIND);
//                }
//            }
//        ;
//    }

    // The "X in a circle" that only shows up when there is text in the search field.
//    protected static DynamicallySizingJRSUIIcon getCancelIcon() {
//        return new DynamicallySizingJRSUIIcon(new SizeDescriptor(new SizeVariant(22, 22).alterMargins(0, 0, 0, 4))) {
//            public void initJRSUIState() {
//                painter.state.set(Widget.BUTTON_SEARCH_FIELD_CANCEL);
//            }
//        };
//    }

    protected static State getState(JButton b) {
        if (!AquaFocusHandler.isActive(b)) return State.INACTIVE;
        if (b.getModel().isPressed()) return State.PRESSED;
        return State.ACTIVE;
    }

    protected static JButton createButton(JTextComponent c, Object /* DynamicallySizingJRSUIIcon */ icon) {
        JButton b = new JButton()
//        {
//            public void paint(Graphics g) {
//                super.paint(g);
//
//                g.setColor(Color.green);
//                g.drawRect(0, 0, getWidth() - 1, getHeight() - 1);
//            }
//        }
                ;

        //Insets i = icon.sizeVariant.margins;
        //b.setBorder(BorderFactory.createEmptyBorder(i.top, i.left, i.bottom, i.right));

        //b.setIcon(icon);
        b.setBorderPainted(false);
        b.setFocusable(false);
        b.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
//        b.addChangeListener(new ChangeListener() {
//            public void stateChanged(ChangeEvent e) {
//                icon.painter.state.set(getState(b));
//            }
//        });
        b.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                JavaSupport.requestFocusInWindowFromMouseEvent(c);
            }
        });

        return b;
    }

    protected static JButton getFindButton(JTextComponent c) {
        //DynamicallySizingJRSUIIcon findIcon = getFindIcon(c);
        JButton b = createButton(c, null);
        b.setName("find");

        Object findPopup = c.getClientProperty(FIND_POPUP_KEY);
        if (findPopup instanceof JPopupMenu) {
            // if we have a popup, indicate that in the icon
            //findIcon.painter.state.set(Variant.MENU_GLYPH);

            b.addMouseListener(new MouseAdapter() {
                public void mousePressed(MouseEvent e) {
                    ((JPopupMenu)findPopup).show(b, 8, b.getHeight() - 2);
                    JavaSupport.requestFocusInWindowFromMouseEvent(c);
                    c.repaint();
                }
            });
        }

        b.addActionListener(new FindAction(c));

        return b;
    }

    private static class FindAction implements ActionListener {
        private JTextComponent tc;

        public FindAction(JTextComponent tc) {
            this.tc = tc;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Object findAction = tc.getClientProperty(FIND_ACTION_KEY);
            if (findAction instanceof ActionListener) {
                ActionListener al = (ActionListener) findAction;
                al.actionPerformed(e);
            } else if (tc instanceof JTextField) {
                JTextField tf = (JTextField) tc;
                Action a = tf.getAction();
                if (a != null) {
                    a.actionPerformed(e);
                }
            }
        }
    }

    private static Component getPromptLabel(JTextComponent c) {
        JLabel label = new JLabel();
        label.putClientProperty(AQUA_LABEL_ROLE_PROPERTY, AQUA_SEARCH_FIELD_PROMPT_ROLE_VALUE);
        label.setFont(null);  // use the same font as the text field

        c.getDocument().addDocumentListener(new DocumentListener() {
            public void changedUpdate(DocumentEvent e) { updatePromptLabel(label, c); }
            public void insertUpdate(DocumentEvent e) { updatePromptLabel(label, c); }
            public void removeUpdate(DocumentEvent e) { updatePromptLabel(label, c); }
        });
        c.addFocusListener(new FocusAdapter() {
            public void focusGained(FocusEvent e) { updatePromptLabel(label, c); }
            public void focusLost(FocusEvent e) { updatePromptLabel(label, c); }
        });
        updatePromptLabel(label, c);

        return label;
    }

    static void updatePromptLabel(JLabel label, JTextComponent text) {
        if (SwingUtilities.isEventDispatchThread()) {
            updatePromptLabelOnEDT(label, text);
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() { updatePromptLabelOnEDT(label, text); }
            });
        }
    }

    static void updatePromptLabelOnEDT(JLabel label, JTextComponent text) {
        String promptText = " ";
        if ("".equals(text.getText())) {
            Object prompt = text.getClientProperty(PROMPT_KEY);
            if (prompt != null) promptText = prompt.toString();
        }
        label.setText(promptText);
    }

    @SuppressWarnings("serial") // anonymous class inside
    protected static JButton getCancelButton(JTextComponent c) {
        JButton b = createButton(c, null);
        b.setName("cancel");
        b.addActionListener(new CancelAction(c));

        c.getDocument().addDocumentListener(new DocumentListener() {
            public void changedUpdate(DocumentEvent e) { updateCancelIcon(b, c); }
            public void insertUpdate(DocumentEvent e) { updateCancelIcon(b, c); }
            public void removeUpdate(DocumentEvent e) { updateCancelIcon(b, c); }
        });

        updateCancelIcon(b, c);
        return b;
    }

    private static class CancelAction implements ActionListener {
        private JTextComponent tc;

        public CancelAction(JTextComponent tc) {
            this.tc = tc;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Object cancelAction = tc.getClientProperty(CANCEL_ACTION_KEY);
            if (cancelAction instanceof ActionListener) {
                ActionListener al = (ActionListener) cancelAction;
                al.actionPerformed(e);
            }
            tc.setText("");
        }
    }

    // <rdar://problem/6444328> JTextField.variant=search: not thread-safe
    static void updateCancelIcon(JButton button, JTextComponent text) {
        if (SwingUtilities.isEventDispatchThread()) {
            updateCancelIconOnEDT(button, text);
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() { updateCancelIconOnEDT(button, text); }
            });
        }
    }

    static void updateCancelIconOnEDT(JButton button, JTextComponent text) {
        button.setVisible(!"".equals(text.getText()));
    }

    // subclass of normal text border, because we still want all the normal text field behaviors
    static class SearchFieldBorder extends AquaTextComponentBorder {
        protected boolean reallyPaintBorder;

        // called via reflection
        public SearchFieldBorder(@NotNull JTextComponent tc) {
            super(tc);
        }

        @Override
        protected @Nullable AquaUIPainter.Size getSpecialDefaultSize() {
            if (OSVersion >= 1016) {
                boolean isToolbar = AquaUtils.isOnToolbar(tc);
                return isToolbar ? AquaUIPainter.Size.LARGE : null;
            }
            return null;
        }

        public void paint(JComponent c, Graphics g, int x, int y, int w, int h) {
            reallyPaintBorder = true;
            paintBorder(c, g, x, y, w, h);
            reallyPaintBorder = false;
        }

        // apparently without adjusting for odd height pixels, the search field "wobbles" relative to it's contents

        // TBD: reducing the height while painting causes clipping
        // Does the wobble problem still occur?

//        @Override
//        public void paintBackground(JComponent c, Graphics g, int x, int y, int width, int height) {
//            super.paintBackground(c, g, x, y - (height % 2), width, height);
//        }

        @Override
        protected @NotNull TextFieldWidget getWidget() {
            boolean hasFocus = AquaFocusHandler.hasFocus(tc);
            String text = tc.getText();
            boolean hasMenu = hasPopupMenu(tc) && (hasFocus || OSVersion >= 1014);
            boolean hasCancelButton = !text.isEmpty();

            if (!hasCancelButton && OSVersion < 1011) {
                // In Yosemite, the cancel button is shown when focused even if there is no text.
                hasCancelButton = hasFocus;
            }

            boolean isToolbar = AquaUtils.isOnToolbar(tc);

            return !hasCancelButton ?
                    hasMenu ?
                            isToolbar ? TextFieldWidget.TEXT_FIELD_SEARCH_WITH_MENU_TOOLBAR : TextFieldWidget.TEXT_FIELD_SEARCH_WITH_MENU
                            :
                            isToolbar ? TextFieldWidget.TEXT_FIELD_SEARCH_TOOLBAR: TextFieldWidget.TEXT_FIELD_SEARCH
                    :
                    hasMenu ?
                            isToolbar ? TextFieldWidget.TEXT_FIELD_SEARCH_WITH_MENU_AND_CANCEL_TOOLBAR : TextFieldWidget.TEXT_FIELD_SEARCH_WITH_MENU_AND_CANCEL
                            :
                            isToolbar ? TextFieldWidget.TEXT_FIELD_SEARCH_WITH_CANCEL_TOOLBAR : TextFieldWidget.TEXT_FIELD_SEARCH_WITH_CANCEL;
        }

        public @NotNull Insets getBorderInsets(@NotNull Component c) {
            if (doingLayout) {
                return new Insets(0, 0, 0, 0);
            }

            return super.getBorderInsets(c);
        }

        @Override
        public int getTextMargin() {
            // No extra margin needed
            return 0;
        }

        protected boolean doingLayout;
        @SuppressWarnings("serial") // anonymous class inside
        protected LayoutManager getCustomLayout() {
            return new SearchFieldLayoutManager();
        }

        class SearchFieldLayoutManager implements LayoutManager {
            private JTextComponent tc;
            private Component findButton;
            private Component cancelButton;
            private Component promptLabel;

            @Override
            public void addLayoutComponent(String name, Component comp) {
                String n = comp.getName();
                if ("find".equals(n)) {
                    findButton = comp;
                    tc = (JTextComponent) comp.getParent();
                } else if ("cancel".equals(n)) {
                    cancelButton = comp;
                    tc = (JTextComponent) comp.getParent();
                } else if (comp instanceof JLabel) {
                    tc = (JTextComponent) comp.getParent();
                    promptLabel = comp;
                }
            }

            @Override
            public void removeLayoutComponent(Component comp) {
            }

            @Override
            public Dimension preferredLayoutSize(Container parent) {
                return null;
            }

            @Override
            public Dimension minimumLayoutSize(Container parent) {
                return null;
            }

            @Override
            public void layoutContainer(Container parent) {
                if (tc != null) {
                    Rectangle bounds = new Rectangle(0, 0, tc.getWidth(), tc.getHeight());
                    TextFieldWidget widget = getWidget();
                    AquaUIPainter.Size sz = AquaUtilControlSize.getUserSizeFrom(tc);
                    AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tc);
                    TextFieldLayoutConfiguration g = new TextFieldLayoutConfiguration(widget, sz, ld);

                    if (findButton != null) {
                        Insetter s = painter.getLayoutInfo().getSearchButtonInsets(g);
                        if (s != null) {
                            Rectangle buttonBounds = s.applyToBounds(bounds);
                            findButton.setBounds(buttonBounds);
                        } else {
                            findButton.setBounds(0, 0, 0, 0);
                        }
                    }

                    if (cancelButton != null) {
                        Insetter s = painter.getLayoutInfo().getCancelButtonInsets(g);
                        if (s != null) {
                            Rectangle buttonBounds = s.applyToBounds(bounds);
                            cancelButton.setBounds(buttonBounds);
                        } else {
                            cancelButton.setBounds(0, 0, 0, 0);
                        }
                    }

                    if (promptLabel != null) {
                        Insetter s = painter.getLayoutInfo().getTextFieldTextInsets(g);
                        Rectangle textBounds = s.applyToBounds(bounds);
                        promptLabel.setBounds(textBounds);
                    }
                }
            }
        }
    }
}
