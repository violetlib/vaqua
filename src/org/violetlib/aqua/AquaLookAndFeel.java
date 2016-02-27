/*
 * Changes copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2013, Oracle and/or its affiliates. All rights reserved.
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

import org.violetlib.aqua.fc.AquaBrowserSizeHandleIcon;
import org.violetlib.aqua.fc.OSXFile;
import org.violetlib.jnr.aqua.AquaNativeRendering;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.util.function.Consumer;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicBorders;
import javax.swing.plaf.basic.BasicLookAndFeel;

import static javax.swing.UIDefaults.LazyValue;

@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaLookAndFeel extends BasicLookAndFeel {
    // for lazy initializers. Following the pattern from metal.
    private static final String PKG_PREFIX = "org.violetlib.aqua.";

    private AquaFocusRingManager focusRingManager;
    private PropertyChangeListener uiChangeListener;
    private AquaPopupFactory popupFactory;

    public static boolean suppressCreationOfDisabledButtonIcons;

    public String getName() {
        return "VAqua";
    }

    public String getID() {
        return "VAqua";
    }

    public String getDescription() {
        return "VAqua Look and Feel for Mac OS X";
    }

    public boolean getSupportsWindowDecorations() {
        return false;
    }

    public boolean isNativeLookAndFeel() {
        return true;
    }

    public boolean isSupportedLookAndFeel() {
        return AquaNativeSupport.load();
    }

    public void initialize() {
        super.initialize();

        // Popups must be heavy to use the vibrant background
        if (popupFactory == null) {
            popupFactory = new AquaPopupFactory();
            PopupFactory.setSharedInstance(popupFactory);
        }

        popupFactory.setActive(true);

        focusRingManager = AquaFocusRingManager.getInstance();
        //focusRingManager.install();

        KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventPostProcessor(AquaMnemonicHandler.getInstance());

        if (uiChangeListener == null) {
            uiChangeListener = new MyUIChangeListener();
        }

        UIManager.addPropertyChangeListener(uiChangeListener);
    }

    public void uninitialize() {
        KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventPostProcessor(AquaMnemonicHandler.getInstance());
        UIManager.removePropertyChangeListener(uiChangeListener);

        if (focusRingManager != null) {
            focusRingManager.uninstall();
            focusRingManager = null;
        }

        popupFactory.setActive(false);

        super.uninitialize();
    }

    @Override
    public Icon getDisabledIcon(JComponent component, Icon icon) {
        if (!suppressCreationOfDisabledButtonIcons) {
            if (icon instanceof ImageIcon) {
                if (component instanceof AbstractButton) {
                    AquaButtonUI ui = AquaUtils.getUI(component, AquaButtonUI.class);
                    if (ui != null) {
                        return ui.createDisabledIcon((AbstractButton) component, (ImageIcon) icon);
                    }
                }
            }

            return super.getDisabledIcon(component, icon);
        } else {
            return null;
        }
    }

    @Override
    public Icon getDisabledSelectedIcon(JComponent component, Icon icon) {
        if (!suppressCreationOfDisabledButtonIcons) {
            if (icon instanceof ImageIcon) {
                if (component instanceof AbstractButton) {
                    AquaButtonUI ui = AquaUtils.getUI(component, AquaButtonUI.class);
                    if (ui != null) {
                        return ui.createDisabledSelectedIcon((AbstractButton) component, (ImageIcon) icon);
                    }
                }
            }

            return super.getDisabledSelectedIcon(component, icon);
        } else {
            return null;
        }
    }

    protected class MyUIChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String prop = evt.getPropertyName();
            if ("lookAndFeel".equals(prop)) {
                Object o = evt.getNewValue();
                if (o == AquaLookAndFeel.this) {
                    // Hopefully, by the time the following code runs, the installation of this LAF has completed,
                    // including the conversion of existing components. We need the Aqua UIs to be installed to properly
                    // display a focus ring for the current focus owner, if any.
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            focusRingManager.install();
                        }
                    });
                }
            }
        }
    }

    /**
     * Display a window as a sheet, if possible. A sheet is dismissed when the window is hidden or disposed.
     * <p>
     * The behavior of a sheet is similar to a document modal dialog in that it prevents user interaction with the
     * existing windows in the hierarchy of the owner. Unlike {@code setVisible(true)} on a model dialog, however, this
     * method does not block waiting for the sheet to be dismissed.
     *
     * @param w the window. The window must have a visible owner. The window must not be visible. If the window is a
     * dialog, its modality will be set to modeless.
     * @param closeHandler If not null, this object will be invoked when the sheet is dismissed.
     * @throws UnsupportedOperationException if the window could not be displayed as a sheet.
     */
    public void displayAsSheet(Window w, Runnable closeHandler) throws UnsupportedOperationException {
        // access point for VSheet which accesses using reflection
        AquaSheetSupport.displayAsSheet(w, closeHandler);
    }

    public void showOptionPaneAsSheet(JDialog d, JOptionPane pane, Consumer<Integer> resultConsumer) {
        // access point for VSheet which accesses using reflection
        AquaSheetSupport.showOptionPaneAsSheet(d, pane, resultConsumer);
    }

    public void showFileChooserAsSheet(Window owner, JFileChooser fc, Consumer<Integer> resultConsumer) {
        // access point for VSheet which accesses using reflection
        AquaSheetSupport.showFileChooserAsSheet(owner, fc, resultConsumer);
    }

    @Override
    protected ActionMap getAudioActionMap() {
        ActionMap audioActionMap = (ActionMap)UIManager.get("AuditoryCues.actionMap");
        if (audioActionMap != null) return audioActionMap;

        final Object[] acList = (Object[])UIManager.get("AuditoryCues.cueList");
        if (acList != null) {
            audioActionMap = new ActionMapUIResource();
            for (int counter = acList.length - 1; counter >= 0; counter--) {
                audioActionMap.put(acList[counter], createAudioAction(acList[counter]));
            }
        }
        UIManager.getLookAndFeelDefaults().put("AuditoryCues.actionMap", audioActionMap);

        return audioActionMap;
    }

    protected static Object makeIcon(String location) {
        return new UIDefaults.ProxyLazyValue(
                "org.violetlib.aqua.AquaIcon", "loadResource",
                new Object[]{location});
    }

    /**
     * We override getDefaults() so we can install our own debug defaults
     * if needed for testing
     */
    public UIDefaults getDefaults() {
        final UIDefaults table = new UIDefaults();
        // use debug defaults if you want to see every query into the defaults object.
        //UIDefaults table = new DebugDefaults();

        table.put("ClassLoader", getClass().getClassLoader());

        try {
            initClassDefaults(table);

            // Here we install all the Basic defaults in case we missed some in our System color
            // or component defaults that follow. Eventually we will take this out.
            // This is a big negative to performance so we want to get it out as soon
            // as we are comfortable with the Aqua defaults.
            super.initSystemColorDefaults(table);
            super.initComponentDefaults(table);

            // Because the last elements added win in precedence we add all of our aqua elements here.
            initSystemColorDefaults(table);
            initComponentDefaults(table);
        } catch(final Exception e) {
            e.printStackTrace();
        }
        return table;
    }

    /**
     * Initialize the defaults table with the name of the ResourceBundle
     * used for getting localized defaults.  Also initialize the default
     * locale used when no locale is passed into UIDefaults.get().  The
     * default locale should generally not be relied upon. It is here for
     * compatibility with releases prior to 1.4.
     */
    private void initResourceBundle(final UIDefaults table) {
        table.setDefaultLocale(Locale.getDefault());
        addResourceBundle(table, PKG_PREFIX + "resources.aqua");
        addResourceBundle(table, PKG_PREFIX + "Labels");
    }

    private void addResourceBundle(final UIDefaults table, String name) {
        table.addResourceBundle(name);
        try {
            final ResourceBundle aquaProperties = ResourceBundle.getBundle(name);
            final Enumeration<String> propertyKeys = aquaProperties.getKeys();
            while (propertyKeys.hasMoreElements()) {
                final String key = propertyKeys.nextElement();
                table.put(key, aquaProperties.getString(key));
            }
        } catch (final Exception e) {
        }
    }

    /**
     * This is the last step in the getDefaults routine usually called from our superclass
     */
    @Override
    protected void initComponentDefaults(final UIDefaults table) {
        initResourceBundle(table);

        final InsetsUIResource zeroInsets = new InsetsUIResource(0, 0, 0, 0);
        final InsetsUIResource menuItemMargin = zeroInsets;

        // *** List value objects
        final Object listCellRendererActiveValue = new UIDefaults.ActiveValue(){
            public Object createValue(UIDefaults defaultsTable) {
                return new DefaultListCellRenderer.UIResource();
            }
        };

        // SJA - I'm basing this on what is in the MetalLookAndFeel class, but
        // without being based on BasicLookAndFeel. We want more flexibility.
        // The key to doing this well is to use Lazy initializing classes as
        // much as possible.

        // Here I want to go to native and get all the values we'd need for colors etc.
        final Border toolTipBorder = new BorderUIResource.EmptyBorderUIResource(2, 0, 2, 0);
        final ColorUIResource toolTipBackground = new ColorUIResource(255, 255, (int)(255.0 * 0.80));
        final ColorUIResource black = new ColorUIResource(Color.black);
        final ColorUIResource white = new ColorUIResource(Color.white);
        final ColorUIResource smokyGlass = new ColorUIResource(new Color(0, 0, 0, 152));
        final ColorUIResource dockIconRim = new ColorUIResource(new Color(192, 192, 192, 192));
        final ColorUIResource mediumTranslucentBlack = new ColorUIResource(new Color(0, 0, 0, 100));
        final ColorUIResource disabled = new ColorUIResource(192, 192, 192);
        final ColorUIResource disabledShadow = new ColorUIResource(0.25f, 0.25f, 0.25f);
        final ColorUIResource selected = new ColorUIResource(1.0f, 0.4f, 0.4f);
        final ColorUIResource alternateBackground = new ColorUIResource(245, 245, 245);
        final ColorUIResource separator = new ColorUIResource(new Color(0, 0, 0, 25));

        // Tabbed pane title colors

        final ColorUIResource selectedTabTitlePressedColor = white;
        final ColorUIResource selectedTabTitleDisabledColor = disabled;
        final ColorUIResource selectedTabTitleNormalColor = white;
        final ColorUIResource selectedTabTitleInactiveColor = black;

        final ColorUIResource nonSelectedTabTitlePressedColor = black;
        final ColorUIResource nonSelectedTabTitleDisabledColor = disabled;
        final ColorUIResource nonSelectedTabTitleNormalColor = black;
        final ColorUIResource nonSelectedTabTitleInactiveColor = black;

        final ColorUIResource toolbarDragHandleColor = new ColorUIResource(140, 140, 140);

        // sja todo Make these lazy values so we only get them when required - if we deem it necessary
        // it may be the case that we think the overhead of a proxy lazy value is not worth delaying
        // creating the object if we think that most swing apps will use this.
        // the lazy value is useful for delaying initialization until this default value is actually
        // accessed by the LAF instead of at init time, so making it lazy should speed up
        // our launch times of Swing apps.

        // *** Text value objects
        final LazyValue marginBorder = t -> new BasicBorders.MarginBorder();

        final int zero = 0;
        final Object editorMargin = zeroInsets; // this is not correct - look at TextEdit to determine the right margin
        final int textCaretBlinkRate = 500;
        final LazyValue textFieldBorder = t ->
            AquaTextFieldBorder.getTextFieldBorder();
        final Object textAreaBorder = marginBorder; // text areas have no real border - radar 311073

        final LazyValue aquaTitledBorder = t ->
            AquaGroupBorder.getBorderForTitledBorder();
        final LazyValue aquaInsetBorder = t ->
            AquaGroupBorder.getTitlelessBorder();

        final Border listHeaderBorder = AquaTableHeaderBorder.getListHeaderBorder();
        final Border zeroBorder = new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0);
        final Border scrollListBorder = new BorderUIResource.LineBorderUIResource(new Color(200, 200, 200), 1);

        final Border browserCellBorder = new BorderUIResource.CompoundBorderUIResource(
                new BorderUIResource.MatteBorderUIResource(0, 0, 1, 0, new ColorUIResource(0xffffff)),
                new BorderUIResource.EmptyBorderUIResource(0, 4, 1, 0));

        // we can't seem to proxy Colors
        final Color listSelectionBackground = AquaImageFactory.getSelectionBackgroundColorUIResource();
        final Color listSelectionForeground = AquaImageFactory.getSelectionForegroundColorUIResource();
        final Color listSelectionInactiveBackground = AquaImageFactory.getSelectionInactiveBackgroundColorUIResource();
        final Color listSelectionInactiveForeground = AquaImageFactory.getSelectionInactiveForegroundColorUIResource();
        final Color comboBoxSelectionBackground = AquaImageFactory.getComboBoxSelectionBackgroundColorUIResource();
        final Color comboBoxSelectionForeground = AquaImageFactory.getComboBoxSelectionForegroundColorUIResource();

        final Color toolBarTitleBackground = new ColorUIResource(211, 211, 211);

        final Color textHighlightText = AquaImageFactory.getTextSelectionForegroundColorUIResource();
        final Color textHighlight = AquaImageFactory.getTextSelectionBackgroundColorUIResource();
        final Color textHighlightInactive = new ColorUIResource(212, 212, 212);
        final Color gridColor = new ColorUIResource(200, 200, 200);

        final Color textInactiveText = disabled;
        final Color textForeground = black;
        final Color textBackground = white;
        final Color textInactiveBackground = white;

        final Color textPasswordFieldCapsLockIconColor = mediumTranslucentBlack;

        final LazyValue internalFrameBorder = t -> BasicBorders.getInternalFrameBorder();
        final Border cellBorder = new BorderUIResource.EmptyBorderUIResource(1, 1, 1, 1);

        final Color desktopBackgroundColor = AquaImageFactory.getDesktopBackgroundColorUIResource();
        final Color focusRingColor = AquaImageFactory.getFocusRingColorUIResource();
        final Color windowBackgroundColor = AquaImageFactory.getWindowBackgroundColorUIResource();
        final Color panelBackgroundColor = windowBackgroundColor;
        final Color tabBackgroundColor = windowBackgroundColor;
        final Color controlBackgroundColor = windowBackgroundColor;

        final Color texturedButtonSelectedColor = new ColorUIResource(Color.WHITE);
        final Color texturedButtonDisabledSelectedColor = new ColorUIResource(new Color(255, 255, 255, 155));
        final Color texturedButtonUnselectedColor = new ColorUIResource(new Color(0, 0, 0, 165));
        final Color texturedButtonDisabledUnselectedColor = new ColorUIResource(new Color(0, 0, 0, 75));

        final LazyValue controlFont = t -> AquaFonts.getControlTextFont();
        final LazyValue controlSmallFont = t -> AquaFonts.getControlTextSmallFont();
        final LazyValue controlMiniFont = t -> AquaFonts.getControlTextMiniFont();
        final LazyValue iconButtonFont = t -> AquaFonts.getIconButtonFont();
        final LazyValue iconButtonSmallFont = t -> AquaFonts.getIconButtonSmallFont();
        final LazyValue alertHeaderFont = t -> AquaFonts.getAlertHeaderFont();
        final LazyValue menuFont = t -> AquaFonts.getMenuFont();
        final LazyValue viewFont = t -> AquaFonts.getViewFont();
        final LazyValue sideBarFont = t -> AquaFonts.getSideBarFont();
        final LazyValue sideBarSelectionFont = t -> AquaFonts.getSideBarSelectionFont();
        final LazyValue sideBarCategoryFont = t -> AquaFonts.getSideBarCategoryFont();
        final LazyValue sideBarCategorySelectionFont = t -> AquaFonts.getSideBarCategorySelectionFont();
        final LazyValue previewLabelFont = t -> AquaFonts.getPreviewLabelFont();
        final LazyValue previewValueFont = t -> AquaFonts.getPreviewValueFont();
        final LazyValue previewNameFont = t -> AquaFonts.getPreviewNameFont();
        final LazyValue previewTypeSizeFont = t -> AquaFonts.getPreviewTypeSizeFont();

        final LazyValue recessedFont = t -> AquaFonts.getRecessedButtonFont();
        final LazyValue inlineFont = t -> AquaFonts.getInlineButtonFont();

        final Color clearColor = new ColorUIResource(new Color(0, 0, 0, 0));

        // can only approximate vibrant sidebar colors
        //final Color sideBarBackgroundColor = new ColorUIResource(230, 230, 230);
        //final Color sideBarInactiveBackgroundColor = new ColorUIResource(245, 245, 245);
        final Color sideBarSelectionBackgroundColor = new ColorUIResource(new Color(0, 0, 0, 67));
        final Color sideBarSelectionInactiveBackgroundColor = new ColorUIResource(205, 205, 205);
        final Color sideBarForegroundColor = new ColorUIResource(new Color(31, 31, 31, 217));
        final Color sideBarInactiveForegroundColor = new ColorUIResource(68, 68, 68);
        final Color sideBarSelectionForegroundColor = new ColorUIResource(30, 30, 30);
        final Color sideBarSelectionInactiveForegroundColor = new ColorUIResource(0, 0, 0);
        final Color sideBarCategoryForegroundColor = new ColorUIResource(new Color(85, 85, 85, 217));
        final Color sideBarCategorySelectionForegroundColor = new ColorUIResource(0, 0, 0);

        final Color menuBorderColor = new ColorUIResource(209, 209, 209);
        final Color menuBarBackgroundColor = new ColorUIResource(new Color(246, 246, 246));
        final Color menuBackgroundColor = clearColor; // new ColorUIResource(new Color(240, 240, 240));
        final Color menuForegroundColor = black;

        final Color menuSelectedForegroundColor = white;
        final Color menuSelectedBackgroundColor = new ColorUIResource(new Color(59, 152, 253));

        final Color menuDisabledBackgroundColor = menuBackgroundColor;
        final Color menuDisabledForegroundColor = disabled;
        final Color menuBarDisabledBackgroundColor = menuBarBackgroundColor;

        final Color menuAccelForegroundColor = black;
        final Color menuAccelSelectionForegroundColor = black;

        //final Border menuBorder = new AquaMenuBorder();
        final Border menuItemBorder = new AquaMenuItemBorder();
        final Border popupMenuBorder = new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0);

        final UIDefaults.LazyInputMap controlFocusInputMap = new UIDefaults.LazyInputMap(new Object[]{
            "SPACE", "pressed",
            "released SPACE", "released"
        });

        final UIDefaults.LazyInputMap fileChooserInputMap = new UIDefaults.LazyInputMap(new Object[]{
                        "ESCAPE", "cancelSelection",
                        "meta PERIOD", "cancelSelection",
                        "F5", "refresh",});

        // sja testing
        final LazyValue confirmIcon = t ->
            AquaImageFactory.getConfirmImageIcon();
        final LazyValue cautionIcon = t ->
            AquaImageFactory.getCautionImageIcon();
        final LazyValue stopIcon = t ->
            AquaImageFactory.getStopImageIcon();
        final LazyValue securityIcon = t ->
            AquaImageFactory.getLockImageIcon();

        final AquaKeyBindings aquaKeyBindings = AquaKeyBindings.instance();

        final Object[] defaults = {
            "control", windowBackgroundColor, /* Default color for controls (buttons, sliders, etc) */

            // JBrowser
            "Browser.selectionBackground", listSelectionBackground,
            "Browser.selectionForeground", listSelectionForeground,
            "Browser.inactiveSelectionBackground", listSelectionInactiveBackground,
            "Browser.inactiveSelectionForeground", listSelectionInactiveForeground,

            "Browser.expandedIcon", makeIcon("fc/Unselected.png"),
            "Browser.expandingIcon", makeIcon("fc/Unselected.png"),
            "Browser.focusedSelectedExpandedIcon", makeIcon("fc/SelectedFocused.png"),
            "Browser.focusedSelectedExpandingIcon", makeIcon("fc/SelectedFocused.png"),
            "Browser.selectedExpandedIcon", makeIcon("fc/SelectedUnfocused.png"),
            "Browser.selectedExpandingIcon", makeIcon("fc/SelectedUnfocused.png"),
            "Browser.sizeHandleIcon", (LazyValue) t -> new AquaBrowserSizeHandleIcon(),

            // Buttons
            "Button.background", controlBackgroundColor,
            "Button.foreground", black,
            "Button.disabledText", disabled,
            "Button.select", selected,
            "Button.border",(LazyValue) t -> AquaButtonBorder.getPushButtonBorder(),
            "Button.font", controlFont,
            "Button.textIconGap", new Integer(4),
            "Button.textShiftOffset", zero, // radar 3308129 - aqua doesn't move images when pressed.
            "Button.focusInputMap", controlFocusInputMap,
            "Button.margin", new InsetsUIResource(0, 0, 0, 0),
            "Button.opaque", false,
            "Button.recessed.font", recessedFont,
            "Button.inline.font", inlineFont,
            "Button.texturedSelectedColor", texturedButtonSelectedColor,
            "Button.texturedDisabledSelectedColor", texturedButtonDisabledSelectedColor,
            "Button.texturedUnselectedColor", texturedButtonUnselectedColor,
            "Button.texturedDisabledUnselectedColor", texturedButtonDisabledUnselectedColor,

            "CheckBox.background", controlBackgroundColor,
            "CheckBox.foreground", black,
            "CheckBox.disabledText", disabled,
            "CheckBox.select", selected,
            "CheckBox.font", controlFont,
            "CheckBox.margin", new InsetsUIResource(1, 1, 0, 1),
            "CheckBox.focusInputMap", controlFocusInputMap,

            "CheckBoxMenuItem.font", menuFont,
            "CheckBoxMenuItem.acceleratorFont", menuFont,
            "CheckBoxMenuItem.background", menuBackgroundColor,
            "CheckBoxMenuItem.foreground", menuForegroundColor,
            "CheckBoxMenuItem.selectionBackground", menuSelectedBackgroundColor,
            "CheckBoxMenuItem.selectionForeground", menuSelectedForegroundColor,
            "CheckBoxMenuItem.disabledBackground", menuDisabledBackgroundColor,
            "CheckBoxMenuItem.disabledForeground", menuDisabledForegroundColor,
            "CheckBoxMenuItem.acceleratorForeground", menuAccelForegroundColor,
            "CheckBoxMenuItem.acceleratorSelectionForeground", menuAccelSelectionForegroundColor,
            "CheckBoxMenuItem.acceleratorDelimiter", "",
            "CheckBoxMenuItem.border", menuItemBorder, // for inset calculation
            "CheckBoxMenuItem.margin", menuItemMargin,
            "CheckBoxMenuItem.borderPainted", Boolean.TRUE,
            "CheckBoxMenuItem.checkIcon",(LazyValue) t -> AquaImageFactory.getMenuItemCheckIcon(),
            "CheckBoxMenuItem.dashIcon",(LazyValue) t -> AquaImageFactory.getMenuItemDashIcon(),
            //"CheckBoxMenuItem.arrowIcon", null,

            "ColorChooser.background", panelBackgroundColor,

            // *** ComboBox
            "ComboBox.font", controlFont,
            "ComboBox.background", white, // see PopupMenu.* for non-editable combo boxes
            "ComboBox.foreground", black,
            "ComboBox.selectionBackground", comboBoxSelectionBackground,
            "ComboBox.selectionForeground", comboBoxSelectionForeground,
            "ComboBox.disabledBackground", menuDisabledBackgroundColor,
            "ComboBox.disabledForeground", menuDisabledForegroundColor,
            "ComboBox.ancestorInputMap", aquaKeyBindings.getComboBoxInputMap(),
            "ComboBox.padding", new InsetsUIResource(1, 4, 1, 4),   // affects only non-editable combo boxes
            "ComboBox.maximumRowCount", 5,

            "DesktopIcon.border", internalFrameBorder,
            "DesktopIcon.borderColor", smokyGlass,
            "DesktopIcon.borderRimColor", dockIconRim,
            "DesktopIcon.labelBackground", mediumTranslucentBlack,
            "Desktop.background", desktopBackgroundColor,

            "EditorPane.focusInputMap", aquaKeyBindings.getMultiLineTextInputMap(),
            "EditorPane.font", controlFont,
            "EditorPane.background", textBackground,
            "EditorPane.foreground", textForeground,
            "EditorPane.selectionBackground", textHighlight,
            "EditorPane.selectionForeground", textHighlightText,
            "EditorPane.caretForeground", textForeground,
            "EditorPane.caretBlinkRate", textCaretBlinkRate,
            "EditorPane.inactiveForeground", textInactiveText,
            "EditorPane.inactiveBackground", textInactiveBackground,
            "EditorPane.border", textAreaBorder,
            "EditorPane.margin", editorMargin,

//            "FileChooser.newFolderIcon", AquaIcon.SystemIcon.getFolderIconUIResource(),
//            "FileChooser.upFolderIcon", AquaIcon.SystemIcon.getFolderIconUIResource(),
//            "FileChooser.homeFolderIcon", AquaIcon.SystemIcon.getDesktopIconUIResource(),
//            "FileChooser.detailsViewIcon", AquaIcon.SystemIcon.getComputerIconUIResource(),
//            "FileChooser.listViewIcon", AquaIcon.SystemIcon.getComputerIconUIResource(),

//            "FileChooser.sideBarIcon.Imac", makeNativeSidebarIcon(sideBarIconsPrefix + "iMac.icns", 18, sideBarIconColor, sideBarIconSelectionColor),
//            "FileChooser.sideBarIcon.MacPro", makeNativeSidebarIcon(sideBarIconsPrefix + "MacPro.icns", 18, sideBarIconColor, sideBarIconSelectionColor),
//            "FileChooser.sideBarIcon.MacMini", makeNativeSidebarIcon(sideBarIconsPrefix + "MacMini.icns", 18, sideBarIconColor, sideBarIconSelectionColor),
//            "FileChooser.sideBarIcon.Laptop", makeNativeSidebarIcon(sideBarIconsPrefix + "Laptop.icns", 18, sideBarIconColor, sideBarIconSelectionColor),

            "FileChooser.ancestorInputMap", fileChooserInputMap,

            "FileChooser.cellTipOrigin", new Point(18, 1),

            "FileChooser.previewNameFont", previewNameFont,
            "FileChooser.previewTypeSizeFont", previewTypeSizeFont,
            "FileChooser.previewLabelFont", previewLabelFont,
            "FileChooser.previewValueFont", previewValueFont,
            "FileChooser.previewLabelForeground", new ColorUIResource(159, 159, 159),
            "FileChooser.previewValueForeground", new ColorUIResource(0, 0, 0),
            "FileChooser.previewLabelInsets", new InsetsUIResource(1, 0, 0, 4),
            "FileChooser.previewLabelDelimiter", "",

            "FileChooser.listView.extraColumnTextColor", new ColorUIResource(128, 128, 128),
            "FileChooser.listView.headerColor", new ColorUIResource(37, 37, 37),
            "FileChooser.listView.headerBackground", new ColorUIResource(240, 240, 240),

            "FileChooser.autovalidate", true,
            "FileChooser.quickLookEnabled", true,
            "FileChooser.orderByType", false,
            "FileChooser.speed", false,  // setting to true turns off the fancy file view

            "FileChooser.browserCellBorder", browserCellBorder,
            "FileChooser.browserCellFocusBorder", browserCellBorder,
            "FileChooser.browserCellFocusBorderGrayed", browserCellBorder,
            "FileChooser.browserCellTextIconGap", 6,
            "FileChooser.browserCellTextArrowIconGap", 5,

            "FileChooser.cancelButtonMnemonic", zero,
            "FileChooser.saveButtonMnemonic", zero,
            "FileChooser.openButtonMnemonic", zero,
            "FileChooser.updateButtonMnemonic", zero,
            "FileChooser.helpButtonMnemonic", zero,
            "FileChooser.directoryOpenButtonMnemonic", zero,

            "FileChooser.lookInLabelMnemonic", zero,
            "FileChooser.fileNameLabelMnemonic", zero,
            "FileChooser.filesOfTypeLabelMnemonic", zero,

            "FileChooser.sideBarRowHeight", 24,

            "FileChooser.sideBarIcon.Applications", OSXFile.getApplicationsSidebarIcon(),
            "FileChooser.sideBarIcon.Desktop", OSXFile.getDesktopSidebarIcon(),
            "FileChooser.sideBarIcon.Documents", OSXFile.getDocumentsSidebarIcon(),
            "FileChooser.sideBarIcon.Downloads", OSXFile.getDownloadsSidebarIcon(),
            "FileChooser.sideBarIcon.GenericFile", OSXFile.getGenericFileSidebarIcon(),
            "FileChooser.sideBarIcon.GenericFolder", OSXFile.getGenericFolderSidebarIcon(),
            "FileChooser.sideBarIcon.GenericVolume", OSXFile.getGenericVolumeSidebarIcon(),
            "FileChooser.sideBarIcon.Home", OSXFile.getHomeSidebarIcon(),
            "FileChooser.sideBarIcon.Movies", OSXFile.getMoviesSidebarIcon(),
            "FileChooser.sideBarIcon.Music", OSXFile.getMusicSidebarIcon(),
            "FileChooser.sideBarIcon.Network", OSXFile.getNetworkSidebarIcon(),
            "FileChooser.sideBarIcon.Pictures", OSXFile.getPicturesSidebarIcon(),
            "FileChooser.sideBarIcon.Utilities", OSXFile.getUtilitiesSidebarIcon(),
            "FileChooser.sideBarIcon.SmartFolder", OSXFile.getSmartFolderSidebarIcon(),
            "FileChooser.sideBarIcon.TimeMachineVolume", OSXFile.getTimeMachineSidebarIcon(),

            "FileView.aliasBadgeIcon", OSXFile.getAliasBadgeIcon(),
            "FileView.computerIcon", AquaImageFactory.getComputerIcon(),
            "FileView.directoryIcon", OSXFile.getDirectoryIcon(),
            "FileView.fileIcon", OSXFile.getFileIcon(),
            //"FileView.hardDriveIcon", OSXFile.getHardDriveIcon(),
            "FileView.networkIcon", OSXFile.getNetworkIcon(),

            "Focus.color", focusRingColor,

            "FormattedTextField.focusInputMap", aquaKeyBindings.getFormattedTextFieldInputMap(),
            "FormattedTextField.font", controlFont,
            "FormattedTextField.background", textBackground,
            "FormattedTextField.foreground", textForeground,
            "FormattedTextField.inactiveForeground", textInactiveText,
            "FormattedTextField.inactiveBackground", textInactiveBackground,
            "FormattedTextField.selectionBackground", textHighlight,
            "FormattedTextField.selectionForeground", textHighlightText,
            "FormattedTextField.caretForeground", textForeground,
            "FormattedTextField.caretBlinkRate", textCaretBlinkRate,
            "FormattedTextField.border", textFieldBorder,
            "FormattedTextField.margin", zeroInsets,

            "IconButton.font", iconButtonFont,
            "IconButton.smallFont", iconButtonSmallFont,

            "InternalFrame.titleFont", controlFont,
            "InternalFrame.background", windowBackgroundColor,
            "InternalFrame.borderColor", windowBackgroundColor,
            "InternalFrame.borderShadow", Color.red,
            "InternalFrame.borderDarkShadow", Color.green,
            "InternalFrame.borderHighlight", Color.blue,
            "InternalFrame.borderLight", Color.yellow,
            "InternalFrame.opaque", Boolean.FALSE,
            "InternalFrame.border", null, //internalFrameBorder,
            "InternalFrame.icon", null,

            "InternalFrame.paletteBorder", null,//internalFrameBorder,
            "InternalFrame.paletteTitleFont", controlSmallFont,
            "InternalFrame.paletteBackground", windowBackgroundColor,

            "InternalFrame.optionDialogBorder", null,//internalFrameBorder,
            "InternalFrame.optionDialogTitleFont", menuFont,
            "InternalFrame.optionDialogBackground", windowBackgroundColor,

            // InternalFrame Auditory Cue Mappings
            "InternalFrame.closeSound", null,
            "InternalFrame.maximizeSound", null,
            "InternalFrame.minimizeSound", null,
            "InternalFrame.restoreDownSound", null,
            "InternalFrame.restoreUpSound", null,

            "InternalFrame.activeTitleBackground", windowBackgroundColor,
            "InternalFrame.activeTitleForeground", textForeground,
            "InternalFrame.inactiveTitleBackground", windowBackgroundColor,
            "InternalFrame.inactiveTitleForeground", textInactiveText,
            "InternalFrame.windowBindings", new Object[]{
                "shift ESCAPE", "showSystemMenu",
                "ctrl SPACE", "showSystemMenu",
                "ESCAPE", "hideSystemMenu"
            },

            // Radar [3543438]. We now define the TitledBorder properties for font and color.
            // Aqua HIG doesn't define TitledBorders as Swing does. Eventually, we might want to
            // re-think TitledBorder to behave more like a Box (NSBox). (vm)
            "TitledBorder.font", controlSmallFont,
            "TitledBorder.titleColor", black,
            "TitledBorder.aquaVariant", aquaTitledBorder, // this is the border that matches what aqua really looks like
            "InsetBorder.aquaVariant", aquaInsetBorder, // this is the title-less variant

            // *** Label
            "Label.font", controlFont, // themeLabelFont is for small things like ToolbarButtons
            "Label.background", controlBackgroundColor,
            "Label.foreground", black,
            "Label.disabledForeground", disabled,
            "Label.disabledShadow", disabledShadow,
            "Label.border", null,

            "List.font", viewFont, // [3577901] Aqua HIG says "default font of text in lists and tables" should be 12 point (vm).
            "List.background", white,
            "List.foreground", black,
            "List.selectionBackground", listSelectionBackground,
            "List.selectionForeground", listSelectionForeground,
            "List.selectionInactiveBackground", listSelectionInactiveBackground,
            "List.selectionInactiveForeground", listSelectionInactiveForeground,
            "List.focusCellHighlightBorder", cellBorder,
            "List.focusSelectedCellHighlightBorder", cellBorder,
            "List.cellNoFocusBorder", cellBorder,
            "List.border", null,
            "List.cellRenderer", listCellRendererActiveValue,

            "List.sourceListBackgroundPainter",(LazyValue) t -> AquaListUI.getSourceListBackgroundPainter(),
            "List.sourceListSelectionBackgroundPainter",(LazyValue) t -> AquaListUI.getSourceListSelectionBackgroundPainter(),
            "List.sourceListFocusedSelectionBackgroundPainter",(LazyValue) t -> AquaListUI.getSourceListFocusedSelectionBackgroundPainter(),
            "List.evenRowBackgroundPainter",(LazyValue) t -> AquaListUI.getListEvenBackgroundPainter(),
            "List.oddRowBackgroundPainter",(LazyValue) t -> AquaListUI.getListOddBackgroundPainter(),
            "List.evenRowBackground", white,
            "List.oddRowBackground", alternateBackground,

            // <rdar://Problem/3743210> The modifier for the Mac is meta, not control.
            "List.focusInputMap", aquaKeyBindings.getListInputMap(),

            //"List.scrollPaneBorder", listBoxBorder, // Not used in Swing1.1
            //"ListItem.border", ThemeMenu.listItemBorder(), // for inset calculation

            // *** Menus
            "Menu.font", menuFont,
            "Menu.acceleratorFont", menuFont,
            "Menu.background", menuBackgroundColor,
            "Menu.foreground", menuForegroundColor,
            "Menu.selectionBackground", menuSelectedBackgroundColor,
            "Menu.selectionForeground", menuSelectedForegroundColor,
            "Menu.disabledBackground", menuDisabledBackgroundColor,
            "Menu.disabledForeground", menuDisabledForegroundColor,
            "Menu.acceleratorForeground", menuAccelForegroundColor,
            "Menu.acceleratorSelectionForeground", menuAccelSelectionForegroundColor,
            //"Menu.border", ThemeMenu.menuItemBorder(), // for inset calculation
            "Menu.border", menuItemBorder,
            "Menu.borderPainted", Boolean.FALSE,
            "Menu.borderColor", menuBorderColor,
            "Menu.margin", menuItemMargin,
            "Menu.arrowIcon",(LazyValue) t -> AquaImageFactory.getMenuArrowIcon(),
            "Menu.consumesTabs", Boolean.TRUE,
            "Menu.menuPopupOffsetY", new Integer(1),
            "Menu.submenuPopupOffsetY", new Integer(-4),

            "MenuBar.font", menuFont,
            "MenuBar.background", menuBarBackgroundColor, // not a menu item, not selected
            "MenuBar.foreground", menuForegroundColor,
            "MenuBar.border", new AquaMenuBarBorder(), // sja make lazy!
            "MenuBar.margin", new InsetsUIResource(0, 8, 0, 8), // sja make lazy!
            "MenuBar.selectionBackground", menuSelectedBackgroundColor, // not a menu item, is selected
            "MenuBar.selectionForeground", menuSelectedForegroundColor,
            "MenuBar.disabledBackground", menuBarDisabledBackgroundColor, //ThemeBrush.GetThemeBrushForMenu(false, false), // not a menu item, not selected
            "MenuBar.disabledForeground", menuDisabledForegroundColor,
            "MenuBar.backgroundPainter",(LazyValue) t -> AquaMenuPainter.getMenuBarPainter(),
            "MenuBar.selectedBackgroundPainter",(LazyValue) t -> AquaMenuPainter.getSelectedMenuBarItemPainter(),

            "MenuItem.font", menuFont,
            "MenuItem.acceleratorFont", menuFont,
            "MenuItem.background", menuBackgroundColor,
            "MenuItem.foreground", menuForegroundColor,
            "MenuItem.selectionBackground", menuSelectedBackgroundColor,
            "MenuItem.selectionForeground", menuSelectedForegroundColor,
            "MenuItem.disabledBackground", menuDisabledBackgroundColor,
            "MenuItem.disabledForeground", menuDisabledForegroundColor,
            "MenuItem.acceleratorForeground", menuAccelForegroundColor,
            "MenuItem.acceleratorSelectionForeground", menuAccelSelectionForegroundColor,
            "MenuItem.acceleratorDelimiter", "",
            "MenuItem.border", menuItemBorder,
            "MenuItem.margin", menuItemMargin,
            "MenuItem.borderPainted", Boolean.TRUE,
            //"MenuItem.arrowIcon", null,
            "MenuItem.selectedBackgroundPainter",(LazyValue) t -> AquaMenuPainter.getSelectedMenuItemPainter(),

            // *** OptionPane
            // You can additionally define OptionPane.messageFont which will
            // dictate the fonts used for the message, and
            // OptionPane.buttonFont, which defines the font for the buttons.
            "OptionPane.font", alertHeaderFont,
            "OptionPane.messageFont", controlFont,
            "OptionPane.buttonFont", controlFont,
            "OptionPane.background", windowBackgroundColor,
            "OptionPane.foreground", black,
            "OptionPane.messageForeground", black,
            "OptionPane.border", new BorderUIResource.EmptyBorderUIResource(12, 21, 17, 21),
            "OptionPane.messageAreaBorder", zeroBorder,
            "OptionPane.buttonAreaBorder", new BorderUIResource.EmptyBorderUIResource(13, 0, 0, 0),
            "OptionPane.minimumSize", new DimensionUIResource(262, 90),

            "OptionPane.errorIcon", stopIcon,
            "OptionPane.informationIcon", confirmIcon,
            "OptionPane.warningIcon", cautionIcon,
            "OptionPane.questionIcon", confirmIcon,
            "_SecurityDecisionIcon", securityIcon,
            "OptionPane.windowBindings", new Object[]{"ESCAPE", "close"},
            // OptionPane Auditory Cue Mappings
            "OptionPane.errorSound", null,
            "OptionPane.informationSound", null, // Info and Plain
            "OptionPane.questionSound", null,
            "OptionPane.warningSound", null,
            "OptionPane.buttonClickThreshhold", new Integer(500),
            "OptionPane.yesButtonMnemonic", "",
            "OptionPane.noButtonMnemonic", "",
            "OptionPane.okButtonMnemonic", "",
            "OptionPane.cancelButtonMnemonic", "",

            "Panel.font", controlFont,
            "Panel.background", panelBackgroundColor, //new ColorUIResource(0.5647f, 0.9957f, 0.5059f),
            "Panel.foreground", black,

            "PasswordField.focusInputMap", aquaKeyBindings.getPasswordFieldInputMap(),
            "PasswordField.font", controlFont,
            "PasswordField.background", textBackground,
            "PasswordField.foreground", textForeground,
            "PasswordField.inactiveForeground", textInactiveText,
            "PasswordField.inactiveBackground", textInactiveBackground,
            "PasswordField.selectionBackground", textHighlight,
            "PasswordField.selectionForeground", textHighlightText,
            "PasswordField.caretForeground", textForeground,
            "PasswordField.caretBlinkRate", textCaretBlinkRate,
            "PasswordField.border", textFieldBorder,
            "PasswordField.margin", zeroInsets,
            "PasswordField.echoChar", new Character((char)0x25CF),
            "PasswordField.capsLockIconColor", textPasswordFieldCapsLockIconColor,

            "PopupMenu.font", menuFont,
            "PopupMenu.background", clearColor,
            // Fix for 7154516: make popups opaque
            "PopupMenu.translucentBackground", white,
            "PopupMenu.foreground", menuForegroundColor,
            "PopupMenu.selectionBackground", menuSelectedBackgroundColor,
            "PopupMenu.selectionForeground", menuSelectedForegroundColor,
            "PopupMenu.border", popupMenuBorder,
//            "PopupMenu.margin",

            "ProgressBar.font", controlFont,
            "ProgressBar.foreground", black,
            "ProgressBar.background", controlBackgroundColor,
            "ProgressBar.selectionForeground", black,
            "ProgressBar.selectionBackground", white,
            "ProgressBar.border", new BorderUIResource(BorderFactory.createEmptyBorder()),
            "ProgressBar.repaintInterval", 30,  // milliseconds
            "ProgressBar.circularRepaintInterval", 70,  // milliseconds

            "RadioButton.background", controlBackgroundColor,
            "RadioButton.foreground", black,
            "RadioButton.disabledText", disabled,
            "RadioButton.select", selected,
            "RadioButton.font", controlFont,
            "RadioButton.margin", new InsetsUIResource(1, 1, 0, 1),
            "RadioButton.focusInputMap", controlFocusInputMap,

            "RadioButtonMenuItem.font", menuFont,
            "RadioButtonMenuItem.acceleratorFont", menuFont,
            "RadioButtonMenuItem.background", menuBackgroundColor,
            "RadioButtonMenuItem.foreground", menuForegroundColor,
            "RadioButtonMenuItem.selectionBackground", menuSelectedBackgroundColor,
            "RadioButtonMenuItem.selectionForeground", menuSelectedForegroundColor,
            "RadioButtonMenuItem.disabledBackground", menuDisabledBackgroundColor,
            "RadioButtonMenuItem.disabledForeground", menuDisabledForegroundColor,
            "RadioButtonMenuItem.acceleratorForeground", menuAccelForegroundColor,
            "RadioButtonMenuItem.acceleratorSelectionForeground", menuAccelSelectionForegroundColor,
            "RadioButtonMenuItem.acceleratorDelimiter", "",
            "RadioButtonMenuItem.border", menuItemBorder, // for inset calculation
            "RadioButtonMenuItem.margin", menuItemMargin,
            "RadioButtonMenuItem.borderPainted", Boolean.TRUE,
            "RadioButtonMenuItem.checkIcon",(LazyValue) t -> AquaImageFactory.getMenuItemCheckIcon(),
            "RadioButtonMenuItem.dashIcon",(LazyValue) t -> AquaImageFactory.getMenuItemDashIcon(),
            //"RadioButtonMenuItem.arrowIcon", null,

            "Separator.background", null,
            "Separator.foreground", separator,
            "Separator.width", 1,

            "ScrollBar.border", null,
            "ScrollBar.focusInputMap", aquaKeyBindings.getScrollBarInputMap(),
            "ScrollBar.focusInputMap.RightToLeft", aquaKeyBindings.getScrollBarRightToLeftInputMap(),
            "ScrollBar.width", new Integer(16),
            "ScrollBar.background", white,
            "ScrollBar.foreground", black,

            "ScrollPane.font", controlFont,
            "ScrollPane.background", white,
            "ScrollPane.foreground", black, //$
            "ScrollPane.border", scrollListBorder,
            "ScrollPane.viewportBorder", null,

            "ScrollPane.ancestorInputMap", aquaKeyBindings.getScrollPaneInputMap(),
            "ScrollPane.ancestorInputMap.RightToLeft", new UIDefaults.LazyInputMap(new Object[]{}),

            "Viewport.font", controlFont,
            "Viewport.background", white, // The background for tables, lists, etc
            "Viewport.foreground", black,

            // *** Slider
            "Slider.foreground", black, "Slider.background", controlBackgroundColor,
            "Slider.font", controlSmallFont,
            //"Slider.highlight", table.get("controlLtHighlight"),
            //"Slider.shadow", table.get("controlShadow"),
            //"Slider.focus", table.get("controlDkShadow"),
            "Slider.tickColor", new ColorUIResource(Color.GRAY),
            "Slider.border", null,
            "Slider.focusInsets", new InsetsUIResource(0, 0, 0, 0),
            "Slider.focusInputMap", aquaKeyBindings.getSliderInputMap(),
            "Slider.focusInputMap.RightToLeft", aquaKeyBindings.getSliderRightToLeftInputMap(),

            // *** Spinner
            "Spinner.font", controlFont,
            "Spinner.background", controlBackgroundColor,
            "Spinner.foreground", black,
            "Spinner.border", null,
            "Spinner.arrowButtonSize", new Dimension(16, 5),
            "Spinner.ancestorInputMap", aquaKeyBindings.getSpinnerInputMap(),
            "Spinner.editorBorderPainted", Boolean.TRUE,
            "Spinner.editorAlignment", SwingConstants.TRAILING,

            // *** SplitPane
            //"SplitPane.highlight", table.get("controlLtHighlight"),
            //"SplitPane.shadow", table.get("controlShadow"),
            "SplitPane.background", panelBackgroundColor,
            "SplitPane.border", null,
            "SplitPane.continuousLayout", Boolean.TRUE,
            "SplitPane.dividerSize", 9, // will be replaced by AquaSplitPaneUI
            "SplitPane.dividerColor", new ColorUIResource(Color.GRAY),  // used for too-thin divider
            "SplitPaneDivider.border", null, // AquaSplitPaneDividerUI draws it

            // *** TabbedPane
            "TabbedPane.font", controlFont,
            "TabbedPane.smallFont", controlSmallFont,
            "TabbedPane.useSmallLayout", Boolean.FALSE,//sSmallTabs ? Boolean.TRUE : Boolean.FALSE,
            "TabbedPane.background", tabBackgroundColor, // for bug [3398277] use a background color so that
            // tabs on a custom pane get erased when they are removed.
            "TabbedPane.foreground", black, //ThemeTextColor.GetThemeTextColor(AppearanceConstants.kThemeTextColorTabFrontActive),
            //"TabbedPane.lightHighlight", table.get("controlLtHighlight"),
            //"TabbedPane.highlight", table.get("controlHighlight"),
            //"TabbedPane.shadow", table.get("controlShadow"),
            //"TabbedPane.darkShadow", table.get("controlDkShadow"),
            //"TabbedPane.focus", table.get("controlText"),
            "TabbedPane.textIconGap", new Integer(4),
            "TabbedPane.tabInsets", new InsetsUIResource(0, 10, 3, 10), // Label within tab (top, left, bottom, right)
            //"TabbedPane.rightTabInsets", new InsetsUIResource(0, 10, 3, 10), // Label within tab (top, left, bottom, right)
            //"TabbedPane.leftTabInsets", new InsetsUIResource(0, 10, 3, 10), // Label within tab
            //"TabbedPane.rightTabInsets", new InsetsUIResource(0, 10, 3, 10), // Label within tab
            //"TabbedPane.tabAreaInsets", new InsetsUIResource(3, 9, -1, 9), // Tabs relative to edge of pane (negative value for overlapping)
            "TabbedPane.tabAreaInsets", new InsetsUIResource(3, 9, -1, 9), // Tabs relative to edge of pane (negative value for overlapping)
            // (top = side opposite pane, left = edge || to pane, bottom = side adjacent to pane, right = left) - see rotateInsets
            "TabbedPane.contentBorderInsets", new InsetsUIResource(8, 0, 0, 0), // width of border
            //"TabbedPane.selectedTabPadInsets", new InsetsUIResource(0, 0, 1, 0), // Really outsets, this is where we allow for overlap
            "TabbedPane.selectedTabPadInsets", new InsetsUIResource(0, 0, 0, 0), // Really outsets, this is where we allow for overlap
            "TabbedPane.tabsOverlapBorder", Boolean.TRUE,
            "TabbedPane.selectedTabTitlePressedColor", selectedTabTitlePressedColor,
            "TabbedPane.selectedTabTitleDisabledColor", selectedTabTitleDisabledColor,
            "TabbedPane.selectedTabTitleNormalColor", selectedTabTitleNormalColor,
            "TabbedPane.selectedTabTitleInactiveColor", selectedTabTitleInactiveColor,
            "TabbedPane.nonSelectedTabTitlePressedColor", nonSelectedTabTitlePressedColor,
            "TabbedPane.nonSelectedTabTitleDisabledColor", nonSelectedTabTitleDisabledColor,
            "TabbedPane.nonSelectedTabTitleNormalColor", nonSelectedTabTitleNormalColor,
            "TabbedPane.nonSelectedTabTitleInactiveColor", nonSelectedTabTitleInactiveColor,
            "TabbedPane.selectedLabelShift", -1,
            "TabbedPane.labelShift", 1,
            "TabbedPane.selectionFollowsFocus", true,

            // *** Table
            "Table.font", viewFont, // [3577901] Aqua HIG says "default font of text in lists and tables" should be 12 point (vm).
            "Table.foreground", black, // cell text color
            "Table.background", white, // cell background color
            "Table.selectionForeground", listSelectionForeground,
            "Table.selectionBackground", listSelectionBackground,
            "Table.selectionInactiveBackground", listSelectionInactiveBackground,
            "Table.selectionInactiveForeground", listSelectionInactiveForeground,
            "Table.gridColor", gridColor, // grid line color
            //"Table.focusCellBackground", selectionBackground,
            //"Table.focusCellForeground", selectionForeground,
            "Table.focusCellHighlightBorder", cellBorder,
            "Table.focusSelectedCellHighlightBorder", cellBorder,
            "Table.cellFocusNoBorder", cellBorder,
            "Table.scrollPaneBorder", scrollListBorder,
            "Table.evenRowBackground", white,
            "Table.oddRowBackground", alternateBackground,
            "Table.scrollPaneCornerComponent",
                new UIDefaults.ActiveValue() {
                    @Override
                    public Object createValue(UIDefaults table) {
                        return new AquaTableScrollPaneCorner();
                    }
                },

            "Table.ancestorInputMap", aquaKeyBindings.getTableInputMap(),
            "Table.ancestorInputMap.RightToLeft", aquaKeyBindings.getTableRightToLeftInputMap(),

            "TableHeader.font", controlSmallFont,
            "TableHeader.foreground", black,
            "TableHeader.background", white, // header background
            "TableHeader.cellBorder", listHeaderBorder,
            "TableHeader.borderHeight", 1,
            "TableHeader.borderColor", gridColor,

            // *** Text
            "TextArea.focusInputMap", aquaKeyBindings.getMultiLineTextInputMap(),
            "TextArea.font", controlFont,
            "TextArea.background", textBackground,
            "TextArea.foreground", textForeground,
            "TextArea.inactiveForeground", textInactiveText,
            "TextArea.inactiveBackground", textInactiveBackground,
            "TextArea.selectionBackground", textHighlight,
            "TextArea.selectionForeground", textHighlightText,
            "TextArea.caretForeground", textForeground,
            "TextArea.caretBlinkRate", textCaretBlinkRate,
            "TextArea.border", textAreaBorder,
            "TextArea.margin", zeroInsets,

            "TextComponent.selectionBackgroundInactive", textHighlightInactive,

            "TextField.focusInputMap", aquaKeyBindings.getTextFieldInputMap(),
            "TextField.font", controlFont,
            "TextField.background", textBackground,
            "TextField.foreground", textForeground,
            "TextField.inactiveForeground", textInactiveText,
            "TextField.inactiveBackground", textInactiveBackground,
            "TextField.selectionBackground", textHighlight,
            "TextField.selectionForeground", textHighlightText,
            "TextField.caretForeground", textForeground,
            "TextField.caretBlinkRate", textCaretBlinkRate,
            "TextField.border", textFieldBorder,
            "TextField.margin", zeroInsets,

            "TextPane.focusInputMap", aquaKeyBindings.getMultiLineTextInputMap(),
            "TextPane.font", controlFont,
            "TextPane.background", textBackground,
            "TextPane.foreground", textForeground,
            "TextPane.selectionBackground", textHighlight,
            "TextPane.selectionForeground", textHighlightText,
            "TextPane.caretForeground", textForeground,
            "TextPane.caretBlinkRate", textCaretBlinkRate,
            "TextPane.inactiveForeground", textInactiveText,
            "TextPane.inactiveBackground", textInactiveBackground,
            "TextPane.border", textAreaBorder,
            "TextPane.margin", editorMargin,

            // *** ToggleButton
            // For best toolbar results should match Button
            "ToggleButton.background", controlBackgroundColor,
            "ToggleButton.foreground", black,
            "ToggleButton.disabledText", disabled,
            "ToggleButton.selectedText", Color.WHITE,
            // we need to go through and find out if these are used, and if not what to set
            // so that subclasses will get good aqua like colors.
            //    "ToggleButton.select", getControlShadow(),
            //    "ToggleButton.text", getControl(),
            //    "ToggleButton.disabledSelectedText", getControlDarkShadow(),
            //    "ToggleButton.disabledBackground", getControl(),
            //    "ToggleButton.disabledSelectedBackground", getControlShadow(),
            //"ToggleButton.focus", getFocusColor(),
            "ToggleButton.border",(LazyValue) t -> AquaButtonBorder.getToggleButtonBorder(), // sja make this lazy!
            "ToggleButton.font", controlFont,
            "ToggleButton.textIconGap", new Integer(4),
            "ToggleButton.textShiftOffset", zero, // radar 3308129 - aqua doesn't move images when pressed.
            "ToggleButton.focusInputMap", controlFocusInputMap,
            "ToggleButton.margin", new InsetsUIResource(0, 0, 0, 0),
            "ToggleButton.opaque", false,

            // *** ToolBar
            "ToolBar.font", controlFont,
            "ToolBar.background", panelBackgroundColor,
            "ToolBar.foreground", new ColorUIResource(Color.gray),
            "ToolBar.dockingBackground", panelBackgroundColor,
            "ToolBar.dockingForeground", listSelectionBackground,
            "ToolBar.floatingBackground", panelBackgroundColor,
            "ToolBar.floatingForeground", new ColorUIResource(Color.darkGray),
            "ToolBar.border",(LazyValue) t -> AquaToolBarUI.getToolBarBorder(),
            "ToolBar.borderHandleColor", toolbarDragHandleColor,
            //"ToolBar.separatorSize", new DimensionUIResource( 10, 10 ),
            "ToolBar.separatorSize", null,
            "ToolBar.title.background", toolBarTitleBackground,

            // *** ToolTips
            "ToolTip.font", controlSmallFont,
            //$ Tooltips - Same color as help balloons?
            "ToolTip.background", toolTipBackground,
            "ToolTip.foreground", black,
            "ToolTip.border", toolTipBorder,

            // *** Tree
            "Tree.font", viewFont, // [3577901] Aqua HIG says "default font of text in lists and tables" should be 12 point (vm).
            "Tree.background", white,
            "Tree.foreground", black,
            // for now no lines
            "Tree.hash", white, //disabled, // Line color
            "Tree.line", white, //disabled, // Line color
            "Tree.textForeground", black,
            "Tree.textBackground", white,
            "Tree.selectionForeground", listSelectionForeground,
            "Tree.selectionBackground", listSelectionBackground,
            "Tree.selectionInactiveBackground", listSelectionInactiveBackground,
            "Tree.selectionInactiveForeground", listSelectionInactiveForeground,
            "Tree.selectionBorderColor", listSelectionBackground, // match the background so it looks like we don't draw anything
            "Tree.editorBorderSelectionColor", null, // The EditTextFrame provides its own border
            "Tree.editorBorder", zeroBorder,
            "Tree.leftChildIndent", 8,
            "Tree.rightChildIndent", 12,
            "Tree.rowHeight", 19,   // iconHeight + 3, to match finder - a zero would have the renderer decide, except that leaves the icons touching
            "Tree.scrollsOnExpand", Boolean.FALSE,
            "Tree.openIcon",(LazyValue) t -> AquaImageFactory.getTreeOpenFolderIcon(), // Open folder icon
            "Tree.closedIcon",(LazyValue) t -> AquaImageFactory.getTreeFolderIcon(), // Closed folder icon
            "Tree.leafIcon",(LazyValue) t -> AquaImageFactory.getTreeDocumentIcon(), // Document icon
            // no expand or collapse icons
            "Tree.changeSelectionWithFocus", Boolean.TRUE,
            "Tree.drawsFocusBorderAroundIcon", Boolean.FALSE,
            "Tree.evenRowBackground", white,
            "Tree.oddRowBackground", alternateBackground,

            "Tree.sideBar.font", sideBarFont,
            "Tree.sideBar.selectionFont", sideBarSelectionFont,
            "Tree.sideBarCategory.font", sideBarCategoryFont,
            "Tree.sideBarCategory.selectionFont", sideBarCategorySelectionFont,

            // Fairly arbitrary choices for backgrounds, since actual colors depend upon what is underneath
            //"Tree.sideBar.background", sideBarBackgroundColor,
            //"Tree.sideBar.inactiveBackground", sideBarInactiveBackgroundColor,
            "Tree.sideBar.selectionBackground", sideBarSelectionBackgroundColor,
            "Tree.sideBar.selectionInactiveBackground", sideBarSelectionInactiveBackgroundColor,
            "Tree.sideBar.foreground", sideBarForegroundColor,
            "Tree.sideBar.inactiveForeground", sideBarInactiveForegroundColor,
            "Tree.sideBar.selectionForeground", sideBarSelectionForegroundColor,
            "Tree.sideBar.selectionInactiveForeground", sideBarSelectionInactiveForegroundColor,
            "Tree.sideBarCategory.foreground", sideBarCategoryForegroundColor,
            "Tree.sideBarCategory.selectionForeground", sideBarCategorySelectionForegroundColor,

            "Tree.focusInputMap", aquaKeyBindings.getTreeInputMap(),
            "Tree.focusInputMap.RightToLeft", aquaKeyBindings.getTreeRightToLeftInputMap(),
            "Tree.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[]{"ESCAPE", "cancel"}),};

        table.putDefaults(defaults);

        AquaUtils.installAATextInfo(table);
    }

    protected void initSystemColorDefaults(final UIDefaults table) {
//        String[] defaultSystemColors = {
//                  "desktop", "#005C5C", /* Color of the desktop background */
//          "activeCaption", "#000080", /* Color for captions (title bars) when they are active. */
//          "activeCaptionText", "#FFFFFF", /* Text color for text in captions (title bars). */
//        "activeCaptionBorder", "#C0C0C0", /* Border color for caption (title bar) window borders. */
//            "inactiveCaption", "#808080", /* Color for captions (title bars) when not active. */
//        "inactiveCaptionText", "#C0C0C0", /* Text color for text in inactive captions (title bars). */
//      "inactiveCaptionBorder", "#C0C0C0", /* Border color for inactive caption (title bar) window borders. */
//                 "window", "#FFFFFF", /* Default color for the interior of windows */
//           "windowBorder", "#000000", /* ??? */
//             "windowText", "#000000", /* ??? */
//               "menu", "#C0C0C0", /* Background color for menus */
//               "menuText", "#000000", /* Text color for menus  */
//               "text", "#C0C0C0", /* Text background color */
//               "textText", "#000000", /* Text foreground color */
//          "textHighlight", "#000080", /* Text background color when selected */
//          "textHighlightText", "#FFFFFF", /* Text color when selected */
//           "textInactiveText", "#808080", /* Text color when disabled */
//                "control", "#C0C0C0", /* Default color for controls (buttons, sliders, etc) */
//            "controlText", "#000000", /* Default color for text in controls */
//           "controlHighlight", "#C0C0C0", /* Specular highlight (opposite of the shadow) */
//         "controlLtHighlight", "#FFFFFF", /* Highlight color for controls */
//          "controlShadow", "#808080", /* Shadow color for controls */
//            "controlDkShadow", "#000000", /* Dark shadow color for controls */
//              "scrollbar", "#E0E0E0", /* Scrollbar background (usually the "track") */
//               "info", "#FFFFE1", /* ??? */
//               "infoText", "#000000"  /* ??? */
//        };
//
//        loadSystemColors(table, defaultSystemColors, isNativeLookAndFeel());
    }

    @Override
    protected void initClassDefaults(final UIDefaults table) {
        final String basicPackageName = "javax.swing.plaf.basic.";

        // Aqua UI classes that support the screen menu bar are required as long as the screen menu bar support
        // is not available to third party LAFs.

        final Object[] uiDefaults = {
            "ButtonUI", PKG_PREFIX + "AquaButtonUI",
            "CheckBoxUI", PKG_PREFIX + "AquaButtonCheckBoxUI",
            "CheckBoxMenuItemUI", "com.apple.laf.AquaMenuItemUI", // PKG_PREFIX + "AquaMenuItemUI",
            "LabelUI", PKG_PREFIX + "AquaLabelUI",
            "ListUI", PKG_PREFIX + "AquaListUI",
            "MenuUI", PKG_PREFIX + "AquaMenuUI",
            "MenuItemUI", "com.apple.laf.AquaMenuItemUI", // PKG_PREFIX + "AquaMenuItemUI",
            "OptionPaneUI", PKG_PREFIX + "AquaOptionPaneUI",
            "PanelUI", PKG_PREFIX + "AquaPanelUI",
            "RadioButtonMenuItemUI", "com.apple.laf.AquaMenuItemUI", // PKG_PREFIX + "AquaMenuItemUI",
            "RadioButtonUI", PKG_PREFIX + "AquaButtonRadioUI",
            "ProgressBarUI", PKG_PREFIX + "AquaProgressBarUI",
            "RootPaneUI", PKG_PREFIX + "AquaRootPaneUI",
            "SliderUI", PKG_PREFIX + "AquaSliderUI",
            "ScrollBarUI", PKG_PREFIX + "AquaScrollBarUI",
            "TabbedPaneUI", PKG_PREFIX + "AquaTabbedPaneUI",
            "TableUI", PKG_PREFIX + "AquaTableUI",
            "ToggleButtonUI", PKG_PREFIX + "AquaButtonToggleUI",
            "ToolBarUI", PKG_PREFIX + "AquaToolBarUI",
            "ToolTipUI", PKG_PREFIX + "AquaToolTipUI",
            "TreeUI", PKG_PREFIX + "AquaTreeUI",

            "InternalFrameUI", PKG_PREFIX + "AquaInternalFrameUI",
            "DesktopIconUI", PKG_PREFIX + "AquaInternalFrameDockIconUI",
            "DesktopPaneUI", PKG_PREFIX + "AquaInternalFramePaneUI",
            "EditorPaneUI", PKG_PREFIX + "AquaEditorPaneUI",
            "TextFieldUI", PKG_PREFIX + "AquaTextFieldUI",
            "TextPaneUI", PKG_PREFIX + "AquaTextPaneUI",
            "ComboBoxUI", PKG_PREFIX + "AquaComboBoxUI",
            "ComboBoxPopupMenuUI", PKG_PREFIX + "AquaComboBoxPopupMenuUI",
            "PopupMenuUI", PKG_PREFIX + "AquaPopupMenuUI",
            "TextAreaUI", PKG_PREFIX + "AquaTextAreaUI",
            "MenuBarUI", "com.apple.laf.AquaMenuBarUI", // PKG_PREFIX + "AquaMenuBarUI",
            "FileChooserUI", PKG_PREFIX + "fc.AquaFileChooserUI",
            "PasswordFieldUI", PKG_PREFIX + "AquaTextPasswordFieldUI",
            "TableHeaderUI", PKG_PREFIX + "AquaTableHeaderUI",

            "FormattedTextFieldUI", PKG_PREFIX + "AquaTextFieldFormattedUI",

            "SpinnerUI", PKG_PREFIX + "AquaSpinnerUI",
            "SplitPaneUI", PKG_PREFIX + "AquaSplitPaneUI",
            "ScrollPaneUI", PKG_PREFIX + "AquaScrollPaneUI",

            "PopupMenuSeparatorUI", PKG_PREFIX + "AquaPopupMenuSeparatorUI",
            "SeparatorUI", PKG_PREFIX + "AquaSeparatorUI",
            "ToolBarSeparatorUI", PKG_PREFIX + "AquaToolBarSeparatorUI",

            // as we implement aqua versions of the swing elements
            // we will add the corresponding Aqua UI classes to this table.

            "ColorChooserUI", basicPackageName + "BasicColorChooserUI",

            "ViewportUI", PKG_PREFIX + "AquaViewportUI",
        };
        table.putDefaults(uiDefaults);
    }

    public String getLongDescription() {
        return "VAqua release " + getReleaseName() + " (build " + getBuildID() + ")";
    }

    @Override
    public String toString() {
        return getLongDescription();
    }

    public static String getReleaseName() {
        return getStringResource("RELEASE.txt");
    }

    public static String getBuildID() {
        return getStringResource("BUILD.txt");
    }

    public static String getVersion() {
        return "VAqua look and feel release " + getReleaseName() + ", build " + getBuildID()
                + " using VAquaRendering release " + AquaNativeRendering.getReleaseName() + ", build " + AquaNativeRendering.getBuildID();
    }

    public static void showVersion() {
        System.err.println("VAqua look and feel: release " + getReleaseName() + ", build " + getBuildID());
        System.err.println("  using VAquaRendering: release " + AquaNativeRendering.getReleaseName() + ", build " + AquaNativeRendering.getBuildID());
    }

    private static String getStringResource(String name)
    {
        InputStream s = AquaLookAndFeel.class.getResourceAsStream(name);
        if (s != null) {
            try {
                BufferedReader r = new BufferedReader(new InputStreamReader(s));
                StringBuilder sb = new StringBuilder();
                for (; ; ) {
                    int ch = r.read();
                    if (ch < 0) {
                        break;
                    }
                    sb.append((char) ch);
                }
                return sb.toString();
            } catch (IOException ex) {
            }
        }

        return "Unknown";
    }
}
