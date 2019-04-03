/*
 * Changes copyright (c) 2015-2019 Alan Snyder.
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

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Enumeration;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.function.Consumer;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicBorders;
import javax.swing.plaf.basic.BasicLookAndFeel;

import org.violetlib.aqua.fc.OSXFile;
import org.violetlib.jnr.aqua.AquaNativeRendering;

import static javax.swing.UIDefaults.LazyValue;

@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaLookAndFeel extends BasicLookAndFeel {
    // for lazy initializers. Following the pattern from metal.
    private static final String PKG_PREFIX = "org.violetlib.aqua.";

    private AquaFocusRingManager focusRingManager;
    private PropertyChangeListener uiChangeListener;
    private AquaPopupFactory popupFactory;

    public static boolean suppressCreationOfDisabledButtonIcons;

    public static final Border NOTHING_BORDER = new EmptyBorder(0, 0, 0, 0);

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
            popupFactory = JavaSupport.createPopupFactory();
        }

        if (PopupFactory.getSharedInstance() != popupFactory) {
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
                            if (focusRingManager != null) {
                                focusRingManager.install();
                            }
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

    /**
     * Set the debugging option to force all windows to display as active. Used to compare a Java window with a native
     * active window.
     */
    public void setForceActiveWindowDisplay(boolean b) {
        AquaRootPaneUI.setForceActiveWindowDisplay(b);
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
        } catch(Exception e) {
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
    private void initResourceBundle(UIDefaults table) {
        table.setDefaultLocale(Locale.getDefault());
        addResourceBundle(table, PKG_PREFIX + "resources.aqua");
        addResourceBundle(table, PKG_PREFIX + "Labels");
    }

    private void addResourceBundle(UIDefaults table, String name) {
        table.addResourceBundle(name);
        try {
            ResourceBundle aquaProperties = ResourceBundle.getBundle(name);
            Enumeration<String> propertyKeys = aquaProperties.getKeys();
            while (propertyKeys.hasMoreElements()) {
                String key = propertyKeys.nextElement();
                table.put(key, aquaProperties.getString(key));
            }
        } catch (Exception e) {
        }
    }

    /**
     * This is the last step in the getDefaults routine usually called from our superclass
     */
    @Override
    protected void initComponentDefaults(UIDefaults table) {
        initResourceBundle(table);

        InsetsUIResource zeroInsets = new InsetsUIResource(0, 0, 0, 0);
        InsetsUIResource menuItemMargin = zeroInsets;

        // *** List value objects
        Object listCellRendererActiveValue = new UIDefaults.ActiveValue(){
            public Object createValue(UIDefaults defaultsTable) {
                return new DefaultListCellRenderer.UIResource();
            }
        };

        // SJA - I'm basing this on what is in the MetalLookAndFeel class, but
        // without being based on BasicLookAndFeel. We want more flexibility.
        // The key to doing this well is to use Lazy initializing classes as
        // much as possible.

        // Here I want to go to native and get all the values we'd need for colors etc.
        Border toolTipBorder = new BorderUIResource.EmptyBorderUIResource(2, 0, 2, 0);
        ColorUIResource smokyGlass = new ColorUIResource(new Color(0, 0, 0, 152));
        ColorUIResource dockIconRim = new ColorUIResource(new Color(192, 192, 192, 192));
        ColorUIResource mediumTranslucentBlack = new ColorUIResource(new Color(0, 0, 0, 100));

        // sja todo Make these lazy values so we only get them when required - if we deem it necessary
        // it may be the case that we think the overhead of a proxy lazy value is not worth delaying
        // creating the object if we think that most swing apps will use this.
        // the lazy value is useful for delaying initialization until this default value is actually
        // accessed by the LAF instead of at init time, so making it lazy should speed up
        // our launch times of Swing apps.

        // *** Text value objects
        LazyValue marginBorder = t -> new BasicBorders.MarginBorder();

        int zero = 0;
        Object editorMargin = zeroInsets; // this is not correct - look at TextEdit to determine the right margin
        int textCaretBlinkRate = 500;
        LazyValue textFieldBorder = null;  // created on demand
        Object textAreaBorder = null;  // created on demand
        LazyValue aquaTitledBorder = t -> AquaGroupBorder.getBorderForTitledBorder();

        Border listHeaderBorder = AquaTableHeaderBorder.getListHeaderBorder();
        Border zeroBorder = new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0);
        Border scrollPaneBorder = new AquaLineBorder("scrollPaneBorder");

        Color toolBarDragHandleColor = new ColorUIResource(140, 140, 140);

        LazyValue internalFrameBorder = t -> BasicBorders.getInternalFrameBorder();
        Border cellBorder = new BorderUIResource.EmptyBorderUIResource(1, 1, 1, 1);

        Color windowBackgroundColor = new ColorUIResource(237, 237, 237); // needed in macOS 10.13 and earlier
        Color panelBackgroundColor = windowBackgroundColor;

        LazyValue controlFont = t -> AquaFonts.getControlTextFont();
        LazyValue controlSmallFont = t -> AquaFonts.getControlTextSmallFont();
        LazyValue controlMiniFont = t -> AquaFonts.getControlTextMiniFont();
        LazyValue iconButtonFont = t -> AquaFonts.getIconButtonFont();
        LazyValue iconButtonSmallFont = t -> AquaFonts.getIconButtonSmallFont();
        LazyValue alertHeaderFont = t -> AquaFonts.getAlertHeaderFont();
        LazyValue menuFont = t -> AquaFonts.getMenuFont();
        LazyValue viewFont = t -> AquaFonts.getViewFont();
        LazyValue sideBarFont = t -> AquaFonts.getSideBarFont();
        LazyValue sideBarSelectionFont = t -> AquaFonts.getSideBarSelectionFont();
        LazyValue sideBarCategoryFont = t -> AquaFonts.getSideBarCategoryFont();
        LazyValue sideBarCategorySelectionFont = t -> AquaFonts.getSideBarCategorySelectionFont();
        LazyValue previewLabelFont = t -> AquaFonts.getPreviewLabelFont();
        LazyValue previewValueFont = t -> AquaFonts.getPreviewValueFont();
        LazyValue previewNameFont = t -> AquaFonts.getPreviewNameFont();
        LazyValue previewTypeSizeFont = t -> AquaFonts.getPreviewTypeSizeFont();

        LazyValue recessedFont = t -> AquaFonts.getRecessedButtonFont();
        LazyValue inlineFont = t -> AquaFonts.getInlineButtonFont();

        Border menuItemBorder = new AquaMenuItemBorder();
        Border popupMenuBorder = new BorderUIResource.EmptyBorderUIResource(0, 0, 0, 0);

        UIDefaults.LazyInputMap controlFocusInputMap = new UIDefaults.LazyInputMap(new Object[]{
                "SPACE", "pressed",
                "released SPACE", "released"
        });

        UIDefaults.LazyInputMap fileChooserInputMap = new UIDefaults.LazyInputMap(new Object[]{
                "ESCAPE", "cancelSelection",
                "meta PERIOD", "cancelSelection",
                "F5", "refresh",});

        // sja testing
        LazyValue confirmIcon = t ->
                AquaImageFactory.getConfirmImageIcon();
        LazyValue cautionIcon = t ->
                AquaImageFactory.getCautionImageIcon();
        LazyValue stopIcon = t ->
                AquaImageFactory.getStopImageIcon();
        LazyValue securityIcon = t ->
                AquaImageFactory.getLockImageIcon();

        AquaKeyBindings aquaKeyBindings = AquaKeyBindings.instance();

        Object[] defaults = {
                // "control" is used by JFrame, cannot be transparent
                "control", windowBackgroundColor, /* Default color for controls (buttons, sliders, etc) */

                // Buttons
                "Button.border",(LazyValue) t -> AquaButtonBorder.getPushButtonBorder(),
                "Button.font", controlFont,
                "Button.textIconGap", 4,
                "Button.textShiftOffset", zero, // radar 3308129 - aqua doesn't move images when pressed.
                "Button.focusInputMap", controlFocusInputMap,
                "Button.margin", new InsetsUIResource(0, 0, 0, 0),
                "Button.recessed.font", recessedFont,
                "Button.inline.font", inlineFont,

                "CheckBox.font", controlFont,
                "CheckBox.margin", new InsetsUIResource(1, 1, 0, 1),
                "CheckBox.focusInputMap", controlFocusInputMap,

                "CheckBoxMenuItem.font", menuFont,
                "CheckBoxMenuItem.acceleratorFont", menuFont,
                "CheckBoxMenuItem.acceleratorDelimiter", "",
                "CheckBoxMenuItem.border", menuItemBorder, // for inset calculation
                "CheckBoxMenuItem.margin", menuItemMargin,
                "CheckBoxMenuItem.borderPainted", true,
                "CheckBoxMenuItem.checkIcon",(LazyValue) t -> AquaImageFactory.getMenuItemCheckIcon(),
                "CheckBoxMenuItem.dashIcon",(LazyValue) t -> AquaImageFactory.getMenuItemDashIcon(),

                "ColorChooser.background", panelBackgroundColor,

                // *** ComboBox
                "ComboBox.font", controlFont,
                "ComboBox.ancestorInputMap", aquaKeyBindings.getComboBoxInputMap(),
                "ComboBox.padding", new InsetsUIResource(1, 4, 1, 4),   // affects only non-editable combo boxes
                "ComboBox.maximumRowCount", 5,

                "DesktopIcon.border", internalFrameBorder,
                "DesktopIcon.borderColor", smokyGlass,
                "DesktopIcon.borderRimColor", dockIconRim,
                "DesktopIcon.labelBackground", mediumTranslucentBlack,

                "EditorPane.focusInputMap", aquaKeyBindings.getMultiLineTextInputMap(),
                "EditorPane.font", controlFont,
                "EditorPane.caretBlinkRate", textCaretBlinkRate,
                "EditorPane.border", textAreaBorder,
                "EditorPane.margin", editorMargin,

                "FileChooser.ancestorInputMap", fileChooserInputMap,

                "FileChooser.cellTipOrigin", new Point(18, 1),

                "FileChooser.previewNameFont", previewNameFont,
                "FileChooser.previewTypeSizeFont", previewTypeSizeFont,
                "FileChooser.previewLabelFont", previewLabelFont,
                "FileChooser.previewValueFont", previewValueFont,
                "FileChooser.previewLabelInsets", new InsetsUIResource(1, 0, 0, 4),
                "FileChooser.previewLabelDelimiter", "",

                "FileChooser.autovalidate", true,
                "FileChooser.quickLookEnabled", true,
                "FileChooser.orderByType", false,

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

                "FileView.aliasBadgeIcon", OSXFile.getAliasBadgeIcon(),
                "FileView.computerIcon", AquaImageFactory.getComputerIcon(),
                "FileView.directoryIcon", OSXFile.getDirectoryIcon(),
                "FileView.fileIcon", OSXFile.getFileIcon(),
                "FileView.networkIcon", OSXFile.getNetworkIcon(),

                "FormattedTextField.focusInputMap", aquaKeyBindings.getFormattedTextFieldInputMap(),
                "FormattedTextField.font", controlFont,
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

                "InternalFrame.windowBindings",
                new Object[]{
                        "shift ESCAPE", "showSystemMenu",
                        "ctrl SPACE", "showSystemMenu",
                        "ESCAPE", "hideSystemMenu"
                },

                // *** Label
                "Label.font", controlFont, // themeLabelFont is for small things like ToolbarButtons
                "Label.border", null,

                "List.font", viewFont, // [3577901] Aqua HIG says "default font of text in lists and tables" should be 12 point (vm).
                "List.focusCellHighlightBorder", cellBorder,
                "List.focusSelectedCellHighlightBorder", cellBorder,
                "List.cellNoFocusBorder", cellBorder,
                "List.border", null,
                "List.cellRenderer", listCellRendererActiveValue,
                "List.focusInputMap", aquaKeyBindings.getListInputMap(),

                // *** Menus
                "Menu.font", menuFont,
                "Menu.acceleratorFont", menuFont,
                "Menu.border", menuItemBorder,
                "Menu.borderPainted", false,
                "Menu.margin", menuItemMargin,
                "Menu.arrowIcon",(LazyValue) t -> AquaImageFactory.getMenuArrowIcon(),
                "Menu.consumesTabs", true,
                "Menu.menuPopupOffsetY", 1,
                "Menu.submenuPopupOffsetY", -4,
                "Menu.opaque", false,

                "MenuBar.font", menuFont,
                "MenuBar.border", new AquaMenuBarBorder(), // sja make lazy!
                "MenuBar.margin", new InsetsUIResource(0, 8, 0, 8),

                "MenuItem.font", menuFont,
                "MenuItem.acceleratorFont", menuFont,
                "MenuItem.acceleratorDelimiter", "",
                "MenuItem.border", menuItemBorder,
                "MenuItem.margin", menuItemMargin,
                "MenuItem.borderPainted", true,

                // *** OptionPane
                // You can additionally define OptionPane.messageFont which will
                // dictate the fonts used for the message, and
                // OptionPane.buttonFont, which defines the font for the buttons.
                "OptionPane.font", alertHeaderFont,
                "OptionPane.messageFont", controlFont,
                "OptionPane.buttonFont", controlFont,
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
                "OptionPane.buttonClickThreshhold", 500,
                "OptionPane.yesButtonMnemonic", "",
                "OptionPane.noButtonMnemonic", "",
                "OptionPane.okButtonMnemonic", "",
                "OptionPane.cancelButtonMnemonic", "",

                "Panel.font", controlFont,
                "Panel.background", panelBackgroundColor,   // needed because content panes can be forced to be opaque

                "PasswordField.focusInputMap", aquaKeyBindings.getPasswordFieldInputMap(),
                "PasswordField.font", controlFont,
                "PasswordField.caretBlinkRate", textCaretBlinkRate,
                "PasswordField.border", textFieldBorder,
                "PasswordField.margin", zeroInsets,
                "PasswordField.echoChar", (char) 0x25CF,

                "PopupMenu.font", menuFont,
                "PopupMenu.border", popupMenuBorder,

                "ProgressBar.font", controlFont,
                "ProgressBar.border", new BorderUIResource(BorderFactory.createEmptyBorder()),
                "ProgressBar.repaintInterval", 30,  // milliseconds
                "ProgressBar.circularRepaintInterval", 70,  // milliseconds

                "RadioButton.font", controlFont,
                "RadioButton.margin", new InsetsUIResource(1, 1, 0, 1),
                "RadioButton.focusInputMap", controlFocusInputMap,

                "RadioButtonMenuItem.font", menuFont,
                "RadioButtonMenuItem.acceleratorFont", menuFont,
                "RadioButtonMenuItem.acceleratorDelimiter", "",
                "RadioButtonMenuItem.border", menuItemBorder, // for inset calculation
                "RadioButtonMenuItem.margin", menuItemMargin,
                "RadioButtonMenuItem.borderPainted", true,
                "RadioButtonMenuItem.checkIcon",(LazyValue) t -> AquaImageFactory.getMenuItemCheckIcon(),
                "RadioButtonMenuItem.dashIcon",(LazyValue) t -> AquaImageFactory.getMenuItemDashIcon(),

                "Separator.width", 1,

                "ScrollBar.border", null,
                "ScrollBar.focusInputMap", aquaKeyBindings.getScrollBarInputMap(),
                "ScrollBar.focusInputMap.RightToLeft", aquaKeyBindings.getScrollBarRightToLeftInputMap(),
                "ScrollBar.width", 16,

                "ScrollPane.font", controlFont,
                "ScrollPane.border", scrollPaneBorder,
                "ScrollPane.viewportBorder", null,

                "ScrollPane.ancestorInputMap", aquaKeyBindings.getScrollPaneInputMap(),
                "ScrollPane.ancestorInputMap.RightToLeft", new UIDefaults.LazyInputMap(new Object[]{}),

                "Viewport.font", controlFont,

                // *** Slider
                "Slider.font", controlSmallFont,
                "Slider.border", null,
                "Slider.focusInsets", new InsetsUIResource(0, 0, 0, 0),
                "Slider.focusInputMap", aquaKeyBindings.getSliderInputMap(),
                "Slider.focusInputMap.RightToLeft", aquaKeyBindings.getSliderRightToLeftInputMap(),

                // *** Spinner
                "Spinner.font", controlFont,
                "Spinner.border", null,
                "Spinner.arrowButtonSize", new Dimension(16, 5),
                "Spinner.ancestorInputMap", aquaKeyBindings.getSpinnerInputMap(),
                "Spinner.editorBorderPainted", true,
                "Spinner.editorAlignment", SwingConstants.TRAILING,

                // *** SplitPane
                "SplitPane.border", null,
                "SplitPane.continuousLayout", true,
                "SplitPane.dividerSize", 1, // will be replaced by AquaSplitPaneUI
                "SplitPaneDivider.border", null, // AquaSplitPaneDividerUI draws it

                // *** TabbedPane
                "TabbedPane.font", controlFont,
                "TabbedPane.smallFont", controlSmallFont,
                "TabbedPane.useSmallLayout", false,//sSmallTabs ? true : false,
                "TabbedPane.textIconGap", 4,
                "TabbedPane.tabInsets", new InsetsUIResource(0, 10, 3, 10), // Label within tab (top, left, bottom, right)
                "TabbedPane.tabAreaInsets", new InsetsUIResource(3, 9, -1, 9), // Tabs relative to edge of pane (negative value for overlapping)
                // (top = side opposite pane, left = edge || to pane, bottom = side adjacent to pane, right = left) - see rotateInsets
                "TabbedPane.contentBorderInsets", new InsetsUIResource(8, 0, 0, 0), // width of border
                "TabbedPane.selectedTabPadInsets", new InsetsUIResource(0, 0, 0, 0), // Really outsets, this is where we allow for overlap
                "TabbedPane.tabsOverlapBorder", true,
                "TabbedPane.selectedLabelShift", -1,
                "TabbedPane.labelShift", 1,
                "TabbedPane.selectionFollowsFocus", true,

                // *** Table
                "Table.font", viewFont, // [3577901] Aqua HIG says "default font of text in lists and tables" should be 12 point (vm).
                "Table.focusCellHighlightBorder", cellBorder,
                "Table.focusSelectedCellHighlightBorder", cellBorder,
                "Table.cellFocusNoBorder", cellBorder,
                "Table.scrollPaneBorder", scrollPaneBorder,
                "Table.scrollPaneCornerComponent",
                (UIDefaults.ActiveValue) table1 -> new AquaTableScrollPaneCorner(),

                "Table.ancestorInputMap", aquaKeyBindings.getTableInputMap(),
                "Table.ancestorInputMap.RightToLeft", aquaKeyBindings.getTableRightToLeftInputMap(),

                "TableHeader.font", controlSmallFont,
                "TableHeader.cellBorder", listHeaderBorder,
                "TableHeader.borderHeight", 1,

                // *** Text
                "TextArea.focusInputMap", aquaKeyBindings.getMultiLineTextInputMap(),
                "TextArea.font", controlFont,
                "TextArea.caretBlinkRate", textCaretBlinkRate,
                "TextArea.border", textAreaBorder,
                "TextArea.margin", zeroInsets,

                "TextField.focusInputMap", aquaKeyBindings.getTextFieldInputMap(),
                "TextField.font", controlFont,
                "TextField.caretBlinkRate", textCaretBlinkRate,
                "TextField.border", textFieldBorder,
                "TextField.margin", zeroInsets,

                "TextPane.focusInputMap", aquaKeyBindings.getMultiLineTextInputMap(),
                "TextPane.font", controlFont,
                "TextPane.caretBlinkRate", textCaretBlinkRate,
                "TextPane.border", textAreaBorder,
                "TextPane.margin", editorMargin,

                "TitledBorder.font", controlSmallFont,
                "TitledBorder.border", aquaTitledBorder,
                "TitledBorder.position", TitledBorder.ABOVE_TOP,

                // *** ToggleButton
                // For best toolbar results should match Button
                "ToggleButton.border",(LazyValue) t -> AquaButtonBorder.getToggleButtonBorder(),
                "ToggleButton.font", controlFont,
                "ToggleButton.textIconGap", 4,
                "ToggleButton.textShiftOffset", zero, // radar 3308129 - aqua doesn't move images when pressed.
                "ToggleButton.focusInputMap", controlFocusInputMap,
                "ToggleButton.margin", new InsetsUIResource(0, 0, 0, 0),

                // *** ToolBar
                "ToolBar.font", controlFont,
                "ToolBar.dockingBackground", panelBackgroundColor,
                "ToolBar.dockingForeground", new ColorUIResource(Color.darkGray),
                "ToolBar.floatingBackground", panelBackgroundColor,
                "ToolBar.floatingForeground", new ColorUIResource(Color.darkGray),
                "ToolBar.borderHandleColor", toolBarDragHandleColor,
                "ToolBar.separatorSize", null,

                // *** ToolTips
                "ToolTip.font", controlSmallFont,
                "ToolTip.border", toolTipBorder,

                // *** Tree
                "Tree.font", viewFont, // [3577901] Aqua HIG says "default font of text in lists and tables" should be 12 point (vm).
                "Tree.editorBorder", zeroBorder,
                "Tree.leftChildIndent", 8,
                "Tree.rightChildIndent", 12,
                "Tree.rowHeight", 19,   // iconHeight + 3, to match finder - a zero would have the renderer decide, except that leaves the icons touching
                "Tree.scrollsOnExpand", false,
                "Tree.openIcon",(LazyValue) t -> AquaImageFactory.getTreeOpenFolderIcon(), // Open folder icon
                "Tree.closedIcon",(LazyValue) t -> AquaImageFactory.getTreeFolderIcon(), // Closed folder icon
                "Tree.leafIcon", OSXFile.getGenericFileSidebarIcon(),
                // no expand or collapse icons
                "Tree.changeSelectionWithFocus", true,
                "Tree.drawsFocusBorderAroundIcon", false,

                "Tree.sideBar.font", sideBarFont,
                "Tree.sideBar.selectionFont", sideBarSelectionFont,
                "Tree.sideBarCategory.font", sideBarCategoryFont,
                "Tree.sideBarCategory.selectionFont", sideBarCategorySelectionFont,

                "Tree.focusInputMap", aquaKeyBindings.getTreeInputMap(),
                "Tree.focusInputMap.RightToLeft", aquaKeyBindings.getTreeRightToLeftInputMap(),
                "Tree.ancestorInputMap", new UIDefaults.LazyInputMap(new Object[]{"ESCAPE", "cancel"}),

        };

        table.putDefaults(defaults);

        int version = AquaUtils.getJavaVersion();
        if (version < 900000) {
            // prior to Java 9, the platform UI is needed to support the screen menu bar
            // the following definitions allow the platform UI to paint a non-screen menu bar
            final Color menuBackgroundColor = new ColorUIResource(Color.white);
            final Color menuForegroundColor = new ColorUIResource(Color.black);

            final Color menuSelectedForegroundColor = new ColorUIResource(Color.white);;
            final Color menuSelectedBackgroundColor = new ColorUIResource(54, 148, 253);

            final Color menuDisabledBackgroundColor = menuBackgroundColor;
            final Color menuDisabledForegroundColor = new ColorUIResource(0.5f, 0.5f, 0.5f);

            Object[] menuBarDefaults = {
                    "MenuBar.font", menuFont,
                    "MenuBar.background", menuBackgroundColor, // not a menu item, not selected
                    "MenuBar.foreground", menuForegroundColor,
                    "MenuBar.border", new AquaMenuBarBorder(),
                    "MenuBar.margin", new InsetsUIResource(0, 8, 0, 8), // sja make lazy!
                    "MenuBar.selectionBackground", menuSelectedBackgroundColor, // not a menu item, is selected
                    "MenuBar.selectionForeground", menuSelectedForegroundColor,
                    "MenuBar.disabledBackground", menuDisabledBackgroundColor, //ThemeBrush.GetThemeBrushForMenu(false, false), // not a menu item, not selected
                    "MenuBar.disabledForeground", menuDisabledForegroundColor,
                    "MenuBar.backgroundPainter", NOTHING_BORDER,
                    "MenuBar.selectedBackgroundPainter", NOTHING_BORDER,
            };
            table.putDefaults(menuBarDefaults);
        }

        JavaSupport.installAATextInfo(table);
    }

    protected void initSystemColorDefaults(UIDefaults table) {
    }

    @Override
    protected void initClassDefaults(UIDefaults table) {
        String basicPackageName = "javax.swing.plaf.basic.";

        int version = AquaUtils.getJavaVersion();

        Object[] uiDefaults = {
                "ButtonUI", PKG_PREFIX + "AquaButtonUI",
                "CheckBoxUI", PKG_PREFIX + "AquaButtonCheckBoxUI",
                "CheckBoxMenuItemUI", PKG_PREFIX + "AquaMenuItemUI",
                "LabelUI", PKG_PREFIX + "AquaLabelUI",
                "ListUI", PKG_PREFIX + "AquaListUI",
                "MenuUI", PKG_PREFIX + "AquaMenuUI",
                "MenuItemUI", PKG_PREFIX + "AquaMenuItemUI",
                "OptionPaneUI", PKG_PREFIX + "AquaOptionPaneUI",
                "PanelUI", PKG_PREFIX + "AquaPanelUI",
                "RadioButtonMenuItemUI", PKG_PREFIX + "AquaMenuItemUI",
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
                // prior to Java 9, the platform UI is needed to support the screen menu bar
                "MenuBarUI", version >= 900000 ? PKG_PREFIX + "AquaMenuBarUI" : "com.apple.laf.AquaMenuBarUI",
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
