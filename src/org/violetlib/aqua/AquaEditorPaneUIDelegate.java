/*
 * Copyright (c) 2018 Alan Snyder.
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
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTextUI;
import javax.swing.text.*;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.StyleSheet;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Supports UI behavior specific to JEditorPane. This code copied from BasicEditorPaneUI.
 */
public class AquaEditorPaneUIDelegate implements AquaTextComponentUIDelegate {

    @Override
    public @NotNull String getPropertyPrefix() {
        return "EditorPane";
    }

    @Override
    public void install(@NotNull JTextComponent c) {
        updateDisplayProperties(c, c.getFont(), c.getForeground());
    }

    @Override
    public void uninstall(@NotNull JTextComponent c) {
        cleanDisplayProperties(c);
    }

    @Override
    public boolean propertyChange(@NotNull PropertyChangeEvent evt) {
        JTextComponent c = (JTextComponent) evt.getSource();
        String name = evt.getPropertyName();
        if ("editorKit".equals(name)) {
            ActionMap map = SwingUtilities.getUIActionMap(c);
            if (map != null) {
                Object oldValue = evt.getOldValue();
                if (oldValue instanceof EditorKit) {
                    Action[] actions = ((EditorKit)oldValue).getActions();
                    if (actions != null) {
                        removeActions(map, actions);
                    }
                }
                Object newValue = evt.getNewValue();
                if (newValue instanceof EditorKit) {
                    Action[] actions = ((EditorKit)newValue).getActions();
                    if (actions != null) {
                        addActions(map, actions);
                    }
                }
            }
            AquaTextComponentSupport.updateFocusTraversalKeys(c, getEditorKit(c));
        } else if ("editable".equals(name)) {
            AquaTextComponentSupport.updateFocusTraversalKeys(c, getEditorKit(c));
        } else if ("foreground".equals(name)
                   || "font".equals(name)
                   || "document".equals(name)
                   || JEditorPane.W3C_LENGTH_UNITS.equals(name)
                   || JEditorPane.HONOR_DISPLAY_PROPERTIES.equals(name)
                   ) {
            updateDisplayProperties(c, c.getFont(), c.getForeground());
            if ("foreground".equals(name)) {
                Object honorDisplayPropertiesObject = c.getClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES);
                boolean honorDisplayProperties = false;
                if (honorDisplayPropertiesObject instanceof Boolean) {
                    honorDisplayProperties = (Boolean) honorDisplayPropertiesObject;
                }
                if (honorDisplayProperties) {
                    return true;
                }
            }
            if (JEditorPane.W3C_LENGTH_UNITS.equals(name) || JEditorPane.HONOR_DISPLAY_PROPERTIES.equals(name)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public @NotNull EditorKit getEditorKit(@NotNull JTextComponent tc) {
        JEditorPane pane = (JEditorPane) tc;
        return pane.getEditorKit();
    }

    @Override
    public @Nullable View create(@NotNull JTextComponent c, Element elem) {
        return null;
    }

    @Override
    public int getBaseline(@NotNull JTextComponent c, int width, int height) {
        return -1;
    }

    @Override
    public @NotNull Component.BaselineResizeBehavior getBaselineResizeBehavior(JTextComponent c) {
        return Component.BaselineResizeBehavior.OTHER;
    }

    @Override
    public @Nullable ActionMap getActionMap(@NotNull JTextComponent c) {
        ActionMap am = new ActionMapUIResource();
        am.put("requestFocus", new FocusAction(c));
        EditorKit editorKit = getEditorKit(c);
        Action[] actions = editorKit.getActions();
        if (actions != null) {
            addActions(am, actions);
        }
        am.put(TransferHandler.getCutAction().getValue(Action.NAME), TransferHandler.getCutAction());
        am.put(TransferHandler.getCopyAction().getValue(Action.NAME), TransferHandler.getCopyAction());
        am.put(TransferHandler.getPasteAction().getValue(Action.NAME), TransferHandler.getPasteAction());
        return am;
    }

    private static class FocusAction extends AbstractAction {

        private final @NotNull JTextComponent editor;

        public FocusAction(@NotNull JTextComponent editor) {
            this.editor = editor;
        }

        public void actionPerformed(ActionEvent e) {
            editor.requestFocus();
        }

        public boolean isEnabled() {
            return editor.isEditable();
        }
    }

    private void removeActions(ActionMap map, Action[] actions) {
        int n = actions.length;
        for (int i = 0; i < n; i++) {
            Action a = actions[i];
            map.remove(a.getValue(Action.NAME));
        }
    }

    private void addActions(ActionMap map, Action[] actions) {
        int n = actions.length;
        for (int i = 0; i < n; i++) {
            Action a = actions[i];
            map.put(a.getValue(Action.NAME), a);
        }
    }

    private void updateDisplayProperties(@NotNull JTextComponent c, Font font, Color fg) {
        Object honorDisplayPropertiesObject = c.getClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES);
        boolean honorDisplayProperties = false;
        Object w3cLengthUnitsObject = c.getClientProperty(JEditorPane.W3C_LENGTH_UNITS);
        boolean w3cLengthUnits = false;
        if (honorDisplayPropertiesObject instanceof Boolean) {
            honorDisplayProperties = (Boolean) honorDisplayPropertiesObject;
        }
        if (w3cLengthUnitsObject instanceof Boolean) {
            w3cLengthUnits = (Boolean) w3cLengthUnitsObject;
        }
        if (this instanceof AquaTextPaneUIDelegate || honorDisplayProperties) {
             //using equals because can not use UIResource for Boolean
            Document doc = c.getDocument();
            if (doc instanceof StyledDocument) {
                if (doc instanceof HTMLDocument && honorDisplayProperties) {
                    updateCSS(c, font, fg);
                } else {
                    updateStyle(c, font, fg);
                }
            }
        } else {
            cleanDisplayProperties(c);
        }
        if ( w3cLengthUnits ) {
            Document doc = c.getDocument();
            if (doc instanceof HTMLDocument) {
                StyleSheet documentStyleSheet = ((HTMLDocument)doc).getStyleSheet();
                documentStyleSheet.addRule("W3C_LENGTH_UNITS_ENABLE");
            }
        } else {
            Document doc = c.getDocument();
            if (doc instanceof HTMLDocument) {
                StyleSheet documentStyleSheet = ((HTMLDocument)doc).getStyleSheet();
                documentStyleSheet.addRule("W3C_LENGTH_UNITS_DISABLE");
            }
        }
    }

    private static final String FONT_ATTRIBUTE_KEY = "FONT_ATTRIBUTE_KEY";

    private void cleanDisplayProperties(@NotNull JTextComponent c) {
        Document document = c.getDocument();
        if (document instanceof HTMLDocument) {
            StyleSheet documentStyleSheet =
                ((HTMLDocument)document).getStyleSheet();
            StyleSheet[] styleSheets = documentStyleSheet.getStyleSheets();
            if (styleSheets != null) {
                for (StyleSheet s : styleSheets) {
                    if (s instanceof StyleSheetUIResource) {
                        documentStyleSheet.removeStyleSheet(s);
                        documentStyleSheet.addRule("BASE_SIZE_DISABLE");
                        break;
                    }
                }
            }
            Style style = ((StyledDocument) document).getStyle(StyleContext.DEFAULT_STYLE);
            if (style.getAttribute(FONT_ATTRIBUTE_KEY) != null) {
                style.removeAttribute(FONT_ATTRIBUTE_KEY);
            }
        }
    }

    private static class StyleSheetUIResource extends StyleSheet implements UIResource {
    }

    private void updateCSS(@NotNull JTextComponent c, Font font, Color fg) {
        Document document = c.getDocument();
        if (document instanceof HTMLDocument) {
            StyleSheet styleSheet = new StyleSheetUIResource();
            StyleSheet documentStyleSheet =
                ((HTMLDocument)document).getStyleSheet();
            StyleSheet[] styleSheets = documentStyleSheet.getStyleSheets();
            if (styleSheets != null) {
                for (StyleSheet s : styleSheets) {
                    if (s instanceof StyleSheetUIResource) {
                        documentStyleSheet.removeStyleSheet(s);
                    }
                }
            }
            String cssRule = sun.swing.
                SwingUtilities2.displayPropertiesToCSS(font, fg);
            styleSheet.addRule(cssRule);
            documentStyleSheet.addStyleSheet(styleSheet);
            documentStyleSheet.addRule("BASE_SIZE " + c.getFont().getSize());
            Style style = ((StyledDocument) document).getStyle(StyleContext.DEFAULT_STYLE);
            if (! font.equals(style.getAttribute(FONT_ATTRIBUTE_KEY))) {
                style.addAttribute(FONT_ATTRIBUTE_KEY, font);
            }
        }
    }

    private void updateStyle(@NotNull JTextComponent c, Font font, Color fg) {
        updateFont(c, font);
        updateForeground(c, fg);
    }

    private void updateForeground(@NotNull JTextComponent c, Color color) {
        StyledDocument doc = (StyledDocument)c.getDocument();
        Style style = doc.getStyle(StyleContext.DEFAULT_STYLE);

        if (style == null) {
            return;
        }

        if (color == null) {
            if (style.getAttribute(StyleConstants.Foreground) != null) {
                style.removeAttribute(StyleConstants.Foreground);
            }
        } else {
            if (! color.equals(StyleConstants.getForeground(style))) {
                StyleConstants.setForeground(style, color);
            }
        }
    }

    private void updateFont(@NotNull JTextComponent c, Font font) {
        StyledDocument doc = (StyledDocument)c.getDocument();
        Style style = doc.getStyle(StyleContext.DEFAULT_STYLE);

        if (style == null) {
            return;
        }

        String fontFamily = (String) style.getAttribute(StyleConstants.FontFamily);
        Integer fontSize = (Integer) style.getAttribute(StyleConstants.FontSize);
        Boolean isBold = (Boolean) style.getAttribute(StyleConstants.Bold);
        Boolean isItalic = (Boolean) style.getAttribute(StyleConstants.Italic);
        Font  fontAttribute = (Font) style.getAttribute(FONT_ATTRIBUTE_KEY);
        if (font == null) {
            if (fontFamily != null) {
                style.removeAttribute(StyleConstants.FontFamily);
            }
            if (fontSize != null) {
                style.removeAttribute(StyleConstants.FontSize);
            }
            if (isBold != null) {
                style.removeAttribute(StyleConstants.Bold);
            }
            if (isItalic != null) {
                style.removeAttribute(StyleConstants.Italic);
            }
            if (fontAttribute != null) {
                style.removeAttribute(FONT_ATTRIBUTE_KEY);
           }
        } else {
            if (! font.getName().equals(fontFamily)) {
                StyleConstants.setFontFamily(style, font.getName());
            }
            if (fontSize == null
                  || fontSize.intValue() != font.getSize()) {
                StyleConstants.setFontSize(style, font.getSize());
            }
            if (isBold == null
                  || isBold.booleanValue() != font.isBold()) {
                StyleConstants.setBold(style, font.isBold());
            }
            if (isItalic == null
                  || isItalic.booleanValue() != font.isItalic()) {
                StyleConstants.setItalic(style, font.isItalic());
            }
            if (! font.equals(fontAttribute)) {
                style.addAttribute(FONT_ATTRIBUTE_KEY, font);
            }
        }
    }
}
