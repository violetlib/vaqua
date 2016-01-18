/*
 * Changes Copyright (c) 2015 Alan Snyder.
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

import sun.swing.DefaultLookup;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicOptionPaneUI;
import javax.swing.text.Document;

public class AquaOptionPaneUI extends BasicOptionPaneUI {
    private static final int kOKCancelButtonWidth = 79;
    private static final int kButtonHeight = 23;

    private static final int kDialogSmallPadding = 4;
    private static final int kDialogLargePadding = 23;

    public static final String TEXT_FIELD_DOCUMENT_KEY = "JOptionPane.textFieldDocument";

    private static String newline;

    static {
        newline = System.lineSeparator();
        if (newline == null) {
            newline = "\n";
        }
    }

    /**
     * Creates a new BasicOptionPaneUI instance.
     */
    public static ComponentUI createUI(final JComponent x) {
        return new AquaOptionPaneUI();
    }

    @Override
    public final void update(final Graphics g, final JComponent c) {
        if (c.isOpaque()) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    /**
     * Creates and returns a Container containin the buttons. The buttons
     * are created by calling <code>getButtons</code>.
     */
    protected Container createButtonArea() {
        final Container bottom = super.createButtonArea();
        // Now replace the Layout
        bottom.setLayout(new AquaButtonAreaLayout(true, kDialogSmallPadding));
        return bottom;
    }

    /**
     * Messaged from installComponents to create a Container containing the
     * body of the message.
     * The icon and body should be aligned on their top edges
     */
    protected Container createMessageArea() {
        JPanel top = new JPanel();
        top.setBorder(UIManager.getBorder("OptionPane.messageAreaBorder"));
        top.setLayout(new BorderLayout());

        JComponent iconPanel = createIconPanel();
        JComponent body = createBody();

        body.setName("OptionPane.body");

        if (iconPanel != null) {
            JComponent realBody = new JPanel(new BorderLayout());
            realBody.setName("OptionPane.realBody");
            realBody.add(Box.createHorizontalStrut(kDialogLargePadding), BorderLayout.BEFORE_LINE_BEGINS);
            realBody.add(body, BorderLayout.CENTER);
            top.add(realBody, BorderLayout.CENTER);
            top.add(iconPanel, BorderLayout.BEFORE_LINE_BEGINS);
        } else {
            top.add(body, BorderLayout.CENTER);
        }
        return top;
    }

    protected JComponent createIconPanel() {
        Icon sideIcon = getIcon();
        if (sideIcon != null) {
            JLabel iconLabel = new JLabel(sideIcon);
            iconLabel.setVerticalAlignment(SwingConstants.TOP);
            JPanel iconPanel = new JPanel();
            iconPanel.add(iconLabel);
            return iconPanel;
        }
        return null;
    }

    protected JComponent createBody() {
        JPanel body = new JPanel();
        body.setLayout(new GridBagLayout());
        GridBagConstraints cons = new GridBagConstraints();
        cons.gridx = cons.gridy = 0;
        cons.gridwidth = GridBagConstraints.REMAINDER;
        cons.gridheight = 1;
        cons.anchor = GridBagConstraints.WEST;
        cons.insets = new Insets(0, 0, 3, 0);
        addMessageComponents(body, cons, getMessage(), getMaxCharactersPerLineCount(), false);
        return body;
    }

    @Override
    protected int getMaxCharactersPerLineCount() {
        return optionPane.getMaxCharactersPerLineCount();
    }

    @Override
    protected Object getMessage() {
        Object o = super.getMessage();

        if (o != null && inputComponent instanceof JTextField) {
            Object p = optionPane.getClientProperty(TEXT_FIELD_DOCUMENT_KEY);
            if (p instanceof Document) {
                Document d = (Document) p;
                JTextField tf = (JTextField) inputComponent;
                tf.setDocument(d);
            }
        }

        return o;
    }

    /**
     * AquaButtonAreaLayout lays out all
     *   components according to the HI Guidelines:
     * The most important button is always on the far right
     * The group of buttons is on the right for left-to-right,
     *         left for right-to-left
     * The widths of each component will be set to the largest preferred size width.
     *
     *
     * This inner class is marked &quot;public&quot; due to a compiler bug.
     * This class should be treated as a &quot;protected&quot; inner class.
     * Instantiate it only within subclasses of BasicOptionPaneUI.
     *
     * BasicOptionPaneUI expects that its buttons are layed out with
     * a subclass of ButtonAreaLayout
     */
    public static class AquaButtonAreaLayout extends ButtonAreaLayout {
        public AquaButtonAreaLayout(final boolean syncAllWidths, final int padding) {
            super(true, padding);
        }

        public void layoutContainer(final Container container) {
            final Component[] children = container.getComponents();
            if (children == null || 0 >= children.length) return;

            final int numChildren = children.length;
            final int yLocation = container.getInsets().top;

            // Always syncAllWidths - and heights!
            final Dimension maxSize = new Dimension(kOKCancelButtonWidth, kButtonHeight);
            for (int i = 0; i < numChildren; i++) {
                final Dimension sizes = children[i].getPreferredSize();
                maxSize.width = Math.max(maxSize.width, sizes.width);
                maxSize.height = Math.max(maxSize.height, sizes.height);
            }

            // ignore getCentersChildren, because we don't
            int xLocation = container.getSize().width - (maxSize.width * numChildren + (numChildren - 1) * padding);
            final int xOffset = maxSize.width + padding;

            // most important button (button zero) on far right
            for (int i = numChildren - 1; i >= 0; i--) {
                children[i].setBounds(xLocation, yLocation, maxSize.width, maxSize.height);
                xLocation += xOffset;
            }
        }
    }

    protected void addMessageComponents(Container container,
                                     GridBagConstraints cons,
                                     Object msg, int maxll,
                                     boolean internallyCreated) {
        if (msg == null) {
            return;
        }
        if (msg instanceof Component) {
            // To workaround problem where Gridbag will set child
            // to its minimum size if its preferred size will not fit
            // within allocated cells
            if (msg instanceof JScrollPane || msg instanceof JPanel) {
                cons.fill = GridBagConstraints.BOTH;
                cons.weighty = 1;
            } else {
                cons.fill = GridBagConstraints.HORIZONTAL;
            }
            cons.weightx = 1;

            container.add((Component) msg, cons);
            cons.weightx = 0;
            cons.weighty = 0;
            cons.fill = GridBagConstraints.NONE;
            cons.gridy++;
            if (!internallyCreated) {
                hasCustomComponents = true;
            }

        } else if (msg instanceof Object[]) {
            Object [] msgs = (Object[]) msg;
            for (Object o : msgs) {
                addMessageComponents(container, cons, o, maxll, false);
            }

        } else if (msg instanceof Icon) {
            JLabel label = new JLabel( (Icon)msg, SwingConstants.CENTER );
            configureMessageLabel(label);
            addMessageComponents(container, cons, label, maxll, true);

        } else {
            String s = msg.toString();
            int len = s.length();
            if (len <= 0) {
                return;
            }

            if (s.startsWith("<html>")) {
                JComponent c = createHTMLTextComponent(s, maxll);
                c.setName("OptionPane.label");
                configureMessageLabel(c);
                addMessageComponents(container, cons, c, maxll, true);
                return;
            }

            int nl;
            int nll = 0;

            if ((nl = s.indexOf(newline)) >= 0) {
                nll = newline.length();
            } else if ((nl = s.indexOf("\r\n")) >= 0) {
                nll = 2;
            } else if ((nl = s.indexOf('\n')) >= 0) {
                nll = 1;
            }
            if (nl >= 0) {
                // break up newlines
                if (nl == 0) {
                    @SuppressWarnings("serial") // anonymous class
                    JPanel breakPanel = new JPanel() {
                        public Dimension getPreferredSize() {
                            Font f = getFont();

                            if (f != null) {
                                return new Dimension(1, f.getSize() + 2);
                            }
                            return new Dimension(0, 0);
                        }
                    };
                    breakPanel.setName("OptionPane.break");
                    addMessageComponents(container, cons, breakPanel, maxll, true);
                } else {
                    addMessageComponents(container, cons, s.substring(0, nl), maxll, false);
                }
                addMessageComponents(container, cons, s.substring(nl + nll), maxll, false);

            } else {
                JComponent c = createTextComponent(s, maxll);
                c.setName("OptionPane.label");
                configureMessageLabel(c);
                addMessageComponents(container, cons, c, maxll, true);
            }
        }
    }

    protected JComponent createHTMLTextComponent(String text, int maxll) {
        if (maxll > 100000) {
            maxll = 80;
        }
        int width = 15 * Math.max(maxll, 100);
        JLabel label = new JLabel(text, JLabel.LEADING);
        label.setMaximumSize(new Dimension(width, 10000));
        return label;
    }

    protected JComponent createTextComponent(String text, int maxll) {
        if (maxll > 100000) {
            maxll = 80;
        }

        if (text.length() < maxll) {
            return new JLabel(text, JLabel.LEADING);
        } else {
            JTextArea textArea = new JTextArea();
            textArea.setEditable(false);
            textArea.setBackground(UIManager.getColor("Label.background"));
            textArea.setFont(UIManager.getFont("Label.font"));
            textArea.setWrapStyleWord(true);
            textArea.setLineWrap(true);
            textArea.setText(text);
            // The following is a pitiful attempt to get JTextArea to compute a plausible
            // preferred size. It is better than nothing. Do not set columns, it just increases the
            // preferred width.
            int columnWidth = textArea.getFontMetrics(textArea.getFont()).charWidth('n');
            textArea.setSize(maxll * columnWidth, 10000);
            return textArea;
        }
    }

    /**
     * Configures any necessary colors/fonts for the specified label
     * used representing the message.
     */
    protected void configureMessageLabel(JComponent label) {
        Color color = (Color) DefaultLookup.get(optionPane, this,
          "OptionPane.messageForeground");
        if (color != null) {
            label.setForeground(color);
        }
        Font messageFont = (Font)DefaultLookup.get(optionPane, this,
                                                   "OptionPane.messageFont");
        if (messageFont != null) {
            label.setFont(messageFont);
        }
    }
}
