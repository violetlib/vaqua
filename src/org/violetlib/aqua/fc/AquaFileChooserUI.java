/*
 * Copyright (c) 2011-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2014-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.*;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.TreeUI;
import javax.swing.plaf.basic.BasicFileChooserUI;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.*;

import static org.violetlib.aqua.AquaButtonUI.BUTTON_TYPE;
import static org.violetlib.aqua.AquaRootPaneUI.*;
import static org.violetlib.aqua.OSXSystemProperties.OSVersion;

/**
 * Provides a list view and a column view similar to the one provided with the native Aqua user interface.
 *
 * Original author: Werner Randelshofer
 */
public class AquaFileChooserUI extends BasicFileChooserUI implements AquaComponentUI {
    // Implementation derived from MetalFileChooserUI

    /*
      In general, this class makes changes to the user interface directly, rather than making changes to the file
      chooser and responding to events from the file chooser. This design is needed to support smart folders (saved
      searches). With saved searches, a file can appear in multiple models using different paths. Using this design,
      tree paths are generally used instead of files.

      In general, events are used only to receive notification of user actions and changes made by the application to
      the file chooser (e.g. from an accessory). Events are not used internally to achieve consistency. The isAdjusting
      variable is used to prevent updates made by this class from being processed by event listeners in this class.

      There are a few minor exceptions where this class changes an attribute of the file chooser with the intent that an
      event will be received and processed. These exceptions are cases where a user action is translated directly into a
      change on the file chooser, which is processed exactly the same as if the application had made the change.
    */

    public static final String PACKAGE_TRAVERSABLE_PROPERTY = "JFileChooser.packageIsTraversable";
    public static final String APPLICATION_TRAVERSABLE_PROPERTY = "JFileChooser.appBundleIsTraversable";
    public static final String OPTIONS_PANEL_ENABLED_PROPERTY = "JFileChooser.optionsPanelEnabled";
    public static final String CAN_CREATE_DIRECTORIES_PROPERTY = "JFileChooser.canCreateDirectories";

    private JFileChooser fc;
    private DirectoryComboBoxModel directoryComboBoxModel;
    private Action directoryComboBoxAction = new DirectoryComboBoxAction();
    private Action optionsAction = new OptionsAction();
    private FileView fileView;
    private FilterComboBoxModel filterComboBoxModel;
    private FileSystemTreeModel model = null;
    private SubtreeTreeModel subtreeModel = null;

    private final static boolean isOptionsButtonAvailable = OSVersion >= 1011;
    private boolean isOptionsEnabled = false;  // used only when the options button is displayed
    private @Nullable String windowStyle;
    private boolean useToolBar;  // true if the top panel should act like a tool bar when the textured window style is used

    /**
     * Each saved search has its own file system tree model.
     */
    private Map<File,FileSystemTreeModel> savedSearches;
    private FileSystemTreeModel fileSystemModel = null;

    // Labels, mnemonics, and tooltips (oh my!)
    protected String optionsButtonText = null;
    protected String optionsButtonToolTipText = null;
    private int fileNameLabelMnemonic = 0;
    private String fileNameLabelText = null;
    ///private int filesOfTypeLabelMnemonic = 0;
    ///private String filesOfTypeLabelText = null;
    ///private String upFolderToolTipText = null;
    ///private String upFolderAccessibleName = null;
    ///private String homeFolderToolTipText = null;
    ///private String homeFolderAccessibleName = null;
    private String newFolderButtonText = null;
    private String newFolderToolTipText = null;
    ///private String newFolderAccessibleName = null;
    protected String chooseButtonText = null;
    private String newFolderDialogPrompt, newFolderDefaultName, newFolderErrorText, newFolderExistsErrorText, newFolderTitleText;
    private String goToFolderDialogPrompt;
    private String goToFolderCancelButtonText;
    private String goToFolderAcceptButtonText;
    private String goToFolderErrorText;
    private String defaultInitialSaveFileName;
    private SidebarTreeModel sidebarTreeModel;
    private HierarchyListener hierarchyListener;
    /**
     * This listener is used to handle files that were dropped on the dir chooser.
     */
    private FileTransferHandler fileTransferHandler;
    /**
     * Actions.
     */
    private Action newFolderAction = new NewFolderAction();
    private Action approveSelectionAction = new AquaApproveSelectionAction();
    /**
     * Values greater than zero indicate that a change is being made by this class to state that is monitored using
     * events and that processing of any events by this class should be inhibited. (This is the normal mode of
     * operation, see comment above.) Examples of state monitored using events: file chooser attributes, file name text
     * field, view root, view selection, sidebar selection.
     */
    private int isAdjusting = 0;
    /**
     * Indicates which view (list or browser) is currently active.
     */
    private int viewMode = ViewModeControl.COLUMN_VIEW;
    /**
     * If true, the current view mode has not been installed.
     */
    private boolean isViewInstalled;
    /**
     * The active view.
     */
    private FileChooserView activeView;
    /**
     * Set to true to detect recursive calls to get the minimum or preferred size.
     */
    private boolean isRecursiveLayoutCall;
    /**
     * The default text for the Go To Folder dialog
     */
    private static String goToFolderText = "";

    /*
     * TBD: These keystrokes should go into an InputMap created by the AquaLookAndFeel class.
     */
    private KeyStroke[] KEYSTROKES = {
            KeyStroke.getKeyStroke(KeyEvent.VK_A, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_D, InputEvent.META_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_D, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_G, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_H, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_I, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_K, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_U, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
            KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD, InputEvent.META_MASK | InputEvent.SHIFT_MASK),
    };

    private class KeyListenerAction extends AbstractAction {
        // TBD: This should be rewritten using an ActionMap

        @Override
        public void actionPerformed(ActionEvent ae) {
            File file = null;
            switch (ae.getActionCommand().charAt(0)) {
                case 'd':
                    file = new File(System.getProperty("user.home") + "/Desktop");
                    break;
                case 'c':
                    FileInfo info = (FileInfo) fileSystemModel.getRoot();
                    file = info.getResolvedFile();
                    break;
                case 'h':
                    file = new File(System.getProperty("user.home"));
                    break;
                case 'k':
                    file = new File("/Network");
                    break;
                case 'i':
                    // not doing iDisk for now
                    file = null;
                    return;
                case 'a':
                    file = new File("/Applications");
                    break;
                case 'u':
                    file = new File( "/Applications/Utilities" );
                    break;
                case 'g':
                    requestFileSelectionPath(null);
                    return;
                case 'o':
                    file = new File(System.getProperty("user.home") + "/Documents");
                    break;
                case '.':
                    // toggle show/hide hidden files
                    boolean isHiding = fc.isFileHidingEnabled();
                    // Here we want to respond to a change event
                    fc.setFileHidingEnabled(!isHiding);
                    return;
                default:
                    // Unknown Key Command in: + ae );
                    break;
            }
            // set the dir if non-null:
            if (file != null) {
                // if the dir is in the sidebar,
                // select the sidebar, otherwise just
                // select the dir
                // FIXME - Implement me
                /*
                for (int i=0, n = sidebarTreeModel.getSize(); i < n; i++) {
                FileInfo sidebarFile = (FileInfo) sidebarTreeModel.getElementAt(i);
                if (sidebarFile != null && sidebarFile.getFile().equals(dir)) {
                sidebarTree.setSelectedIndex(i);
                return;
                }
                }*/

                if (file.isDirectory()) {
                    selectDirectory(file, SELECT_DIRECTORY_BY_KEYSTROKE);
                } else if (isAcceptable(file)) {
                    // Here we want to respond to a change event
                    fc.setSelectedFile(file);
                }
            }
        }
    }

    /**
     * A key listener that implements keyboard shortcuts that use text characters. By handling the key typed event, we
     * (hopefully) avoid a race condition observed in Java 1.6 when the shortcut was implemented using an input map. The
     * input map processes key pressed events. On occasion, the subsequent key typed event would be processed by the
     * sheet, resulting in a doubling of the shortcut character in the text field.
     */
    private class TextKeyListener extends KeyAdapter {
        @Override
        public void keyTyped(KeyEvent e) {
            char ch = e.getKeyChar();
            if (ch == '/' || ch == '~') {
                String text = "" + ch;
                requestFileSelectionPath(text);
                e.consume();
            }
        }
    }

    /**
     * Ask the user for a path for a new file selection.
     */
    protected void requestFileSelectionPath(String initialText) {
        Window parent = SwingUtilities.getWindowAncestor(fc);

        String prompt = goToFolderDialogPrompt;
        String cancelLabel = goToFolderCancelButtonText;
        String acceptLabel = goToFolderAcceptButtonText;
        String errorText = goToFolderErrorText;

        JDialog dialog;
        if (parent instanceof Frame) {
            dialog = new JDialog((Frame) parent);
        } else {
            dialog = new JDialog((Dialog) parent);
        }

        RequestFileSelectionPathPane pane
                = new RequestFileSelectionPathPane(dialog, initialText, prompt, cancelLabel, acceptLabel, errorText) {
            @Override
            protected void canceled() {
            }

            @Override
            protected void accepted() {
                File file = getSelection();
                if (file.isDirectory()) {
                    selectDirectory(file, SELECT_DIRECTORY_BY_KEYSTROKE);
                } else if (isAcceptable(file)) {
                    // Here we want to respond to a change event
                    fc.setSelectedFile(file);
                }
            }
        };

        pane.setBorder(new EmptyBorder(20, 15, 10, 15));
        pane.setComponentOrientation(parent.getComponentOrientation());
        dialog.setContentPane(pane);
        dialog.pack();
        AquaSheetSupport.displayAsSheet(dialog, null);
        //dialog.setVisible(true);
    }

    /*
      Not using a JOptionPane because we do not want to display an icon, we want to disable the accept button when text
      field is empty, we want to display an error message.
    */

    protected class RequestFileSelectionPathPane extends JPanel implements DocumentListener {
        private JLabel messageLabel;
        private JTextField inputField;
        private JLabel errorLabel;
        private JButton cancelButton;
        private JButton acceptButton;
        private File selection;

        public RequestFileSelectionPathPane(JDialog parent,
                                            String initialText,
                                            String message,
                                            String cancelLabel,
                                            String acceptLabel,
                                            String errorText) {
            messageLabel = new JLabel(message);
            errorLabel = new JLabel();
            inputField = createTextField("Request File Input Field");
            cancelButton = createButton(cancelLabel);
            acceptButton = createButton(acceptLabel);

            inputField.putClientProperty("Quaqua.TextComponent.autoSelect", false); // avoid Quaqua auto select

            messageLabel.setAlignmentX(0);
            inputField.setAlignmentX(0);
            inputField.setColumns(32);
            errorLabel.setMaximumSize(new Dimension(100000, 100000));

            {
                Font f = UIManager.getFont("FileChooser.sheetErrorFont");
                if (f == null) {
                    f = UIManager.getFont("SmallSystemFont");
                }
                errorLabel.setFont(f);
            }

            if (parent != null) {
                parent.getRootPane().setDefaultButton(acceptButton);
            }

            /*
              The goal is to select the text unless the user typed / or ~.
            */

            if (initialText != null) {
                inputField.setText(initialText);
            } else {
                // TBD: should be a persistent property
                inputField.setText(goToFolderText);
                if (!goToFolderErrorText.isEmpty()) {
                    inputField.addFocusListener(new FocusAdapter() {
                        @Override
                        public void focusGained(FocusEvent e) {
                            inputField.selectAll();
                        }
                    });
                }
            }

            JPanel buttonPane = new JPanel();
            buttonPane.setAlignmentX(0);
            buttonPane.setBorder(new EmptyBorder(12, 0, 6, 0));
            buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.X_AXIS));
            buttonPane.add(errorLabel);
            buttonPane.add(cancelButton);
            buttonPane.add(Box.createHorizontalStrut(6));
            buttonPane.add(acceptButton);

            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            add(messageLabel);
            add(inputField);
            add(buttonPane);

            inputField.getDocument().addDocumentListener(this);

            cancelButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (parent != null) {
                        parent.setVisible(false);
                    }
                    canceled();
                }
            });

            acceptButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    String path = inputField.getText();
                    File currentDirectory = fc.getCurrentDirectory();
                    File f = OSXFile.resolvePath(path, currentDirectory);
                    if (!f.isDirectory()) {
                        errorLabel.setText(errorText);
                        inputField.selectAll();
                    } else {
                        selection = f;
                        goToFolderText = path;
                        if (parent != null) {
                            parent.setVisible(false);
                        }
                        accepted();
                    }
                }
            });

            pathChanged();
        }

        public File getSelection() {
            return selection;
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            pathChanged();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            pathChanged();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            pathChanged();
        }

        protected void pathChanged() {
            errorLabel.setText("");
            acceptButton.setEnabled(!inputField.getText().isEmpty());
        }

        protected void canceled() {
        }

        protected void accepted() {
        }
    }

    private AbstractAction keyListenerAction = new KeyListenerAction();

    protected TopPanel topPanel;
    protected   SavePanel savePanel;
    protected     JPanel fileNameLine;
    protected       JLabel fileNameLabel;
    protected       JTextField fileNameTextField;
    protected       JPanel fileNameSpringPanel;
    protected     JSeparator separator;
    protected   NavigationPanel navigationPanel;
    protected     ViewModeControl viewModeControl;
    protected     JComboBox directoryComboBox;
    protected JSplitPane splitPane;
    protected   JScrollPane sidebarScrollPane;
    protected   JPanel viewsPanel;
    protected JPanel controlsPanel;
    protected   OptionsPanel optionsPanel;
    protected     JPanel accessoryPanel;
    protected     JPanel formatPanel;
    protected       JLabel filesOfTypeLabel;
    protected       JComboBox filterComboBox;
    protected       JPanel formatSpringPanel;
    protected     JSeparator optionsSeparator;
    protected   ButtonPanel buttonsPanel;
    protected     JButton newFolderButton;
    protected     JButton optionsButton;
    protected     JButton cancelButton;
    protected     JButton approveButton;

    protected ListView listView;
    protected ColumnView columnView;
    protected JTree sidebarTree;
    //protected JButton nextButton;
    //protected JButton previousButton;

    private interface Reconfigurable {
        void reconfigure();
    }

    public static ComponentUI createUI(JComponent c) {
        return new AquaFileChooserUI((JFileChooser) c);
    }

    public AquaFileChooserUI(JFileChooser filechooser) {
        super(filechooser);
    }

    @Override
    public void installUI(JComponent c) {
        fc = (JFileChooser) c;
        super.installUI(c);
        installSelectedView(false, true);
    }

    private void configureTopPanel() {
        if (fc.getDialogType() == JFileChooser.SAVE_DIALOG) {
            navigationPanel.setToolBar(useToolBar);
            topPanel.setToolBar(false);
        } else {
            navigationPanel.setToolBar(false);
            topPanel.setToolBar(useToolBar);
        }
    }

    private class TopPanel extends JPanel {
        public TopPanel() {
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            setOpaque(false);
            setBorder(new EmptyBorder(4, 0, 4, 0));
        }

        public void setToolBar(boolean b) {
            putClientProperty(AquaUtils.TOOLBAR_PANEL_PROPERTY, b);
        }

        @Override
        public void doLayout() {
            super.doLayout();
            updateWindowStyleParameters();
        }
    }

    private class SavePanel extends JPanel {
        public SavePanel() {
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            setOpaque(false);
        }
    }

    // Navigation panel: depends upon Open/Save dialog type

    private class NavigationPanel extends JPanel implements Reconfigurable {
        public NavigationPanel() {
            setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
            setOpaque(false);
            reconfigure();
        }

        public void setToolBar(boolean b) {
            putClientProperty(AquaUtils.TOOLBAR_PANEL_PROPERTY, b);
        }

        public void reconfigure() {
            if (fc.getDialogType() == JFileChooser.SAVE_DIALOG) {
                if (OSXSystemProperties.OSVersion >= 1011) {
                    setBorder(new EmptyBorder(9, 11, 6, 11));
                } else {
                    setBorder(new EmptyBorder(17, 11, 6, 11));
                }
            } else {
                if (OSXSystemProperties.OSVersion >= 1014) {
                    setBorder(new EmptyBorder(5, 8, 6, 8));
                } else {
                    setBorder(new EmptyBorder(3, 8, 2, 8));
                }
            }
        }
    }

    // Options panel: depends upon Open/Save dialog type, sheet status, active status

    private class OptionsPanel extends JPanel implements Reconfigurable {
        public OptionsPanel() {
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            reconfigure();
        }

        @Override
        public void reconfigure() {
            setOpaque(false);
            if (useGroupBox()) {
                Border b = new AquaGroupBorder(new Insets(10, 10, 1, 10), new Insets(18, 22, 18, 22), false);
                setBorder(b);
                optionsSeparator.setVisible(false);
            } else {
                setBorder(null);
                optionsSeparator.setVisible(true);
                // Use a transparent background when displayed in a sheet.
                if (!AquaSheetSupport.isSheet(fc)) {
                    AquaAppearance appearance = AppearanceManager.getAppearance(fc);
                    boolean isSave = fc.getDialogType() == JFileChooser.SAVE_DIALOG;
                    String name = isSave ? "saveOptionsArea" : "openOptionsArea";
                    EffectName effect = AquaFocusHandler.isActive(fc) ? EffectName.EFFECT_NONE : EffectName.EFFECT_DISABLED;
                    Color color = appearance.getColorForEffect(name, effect);
                    if (color != null) {
                        // Must not use a UI color, as this color should override a vibrant ancestor.
                        setOpaque(true);
                        setBackground(AquaColors.getOrdinaryColor(color));
                    }
                }
            }
        }

        boolean useGroupBox() {
            if (OSVersion < 1013) {
                return OSVersion == 1010 || fc.getDialogType() == JFileChooser.SAVE_DIALOG;
            } else {
                return false;
            }
        }
    }

    private class ControlsPanel extends JPanel {

        public ControlsPanel() {
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            setBorder(AquaLookAndFeel.NOTHING_BORDER);  // allow divider to show
            setOpaque(false);
        }
    }

    // Button panel: depends upon Open/Save dialog type

    private class ButtonPanel extends JPanel implements Reconfigurable {
        private Component spacer1 = Box.createHorizontalStrut(8);

        public ButtonPanel() {
            setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
            reconfigure();
        }

        protected void hideControlButtons() {
            newFolderButton.setVisible(false);
            optionsButton.setVisible(false);
            cancelButton.setVisible(false);
            approveButton.setVisible(false);
        }

        protected void showControlButtons() {
            newFolderButton.setVisible(shouldDisplayNewFolderButton());
            optionsButton.setVisible(shouldDisplayOptionsButton());
            cancelButton.setVisible(true);
            approveButton.setVisible(true);
            spacer1.setVisible(newFolderButton.isVisible() && optionsButton.isVisible());
        }

        public void populate() {
            add(newFolderButton);
            add(spacer1);
            add(optionsButton);
            add(Box.createHorizontalGlue());
            add(cancelButton);
            add(Box.createHorizontalStrut(8));
            add(approveButton);
            updateButtons();
        }

        public void updateButtons() {
            updateButton(newFolderButton);
            updateButton(optionsButton);
            updateButton(cancelButton);
            updateButton(approveButton);
        }

        @Override
        public void reconfigure() {
            // The margins were decreased in El Capitan, except for save dialogs.
            // One wonders if save dialogs were not changed intentionally or by omission.

            if (fc.getDialogType() == JFileChooser.SAVE_DIALOG || OSVersion < 1011) {
                setBorder(BorderFactory.createEmptyBorder(10, 23, 9, 10));
            } else {
                setBorder(BorderFactory.createEmptyBorder(8, 8, 9, 8));
            }
        }
    }

    private class SidebarTree extends JTree {

        @Override
        public Dimension getPreferredSize() {
            Dimension d = super.getPreferredSize();
            d.width = 10;
            return d;
        }

        @Override
        public String toString() {
            return "SidebarTree";
        }
    }

    @Override
    public void installComponents(JFileChooser fc) {
        sidebarTree = new SidebarTree();

        ChangeListener viewSelectionChangeListener = createViewSelectionChangeListener(fc);
        FileChooserView.SelectListener viewSelectListener = createViewSelectListener(fc);

        GridBagConstraints gridBagConstraints;

        topPanel = new TopPanel();
        savePanel = new SavePanel();
        fileNameLine = new JPanel();
        fileNameLabel = new JLabel();
        fileNameTextField = createTextField("File Name Text Field");
        fileNameSpringPanel = new JPanel();
        separator = createSeparator();
        navigationPanel = new NavigationPanel();
        //previousButton = createButton();
        //nextButton = createButton();
        directoryComboBox = createDirectoryComboBox();
        splitPane = new JSplitPane();
        sidebarScrollPane = new JScrollPane();
        viewsPanel = new JPanel();
        columnView = ColumnView.create(fc);
        viewModeControl = ViewModeControl.create();
        listView = ListView.create(fc);
        controlsPanel = new ControlsPanel();
        optionsSeparator = createOptionsSeparator();    // create before options panel
        optionsPanel = new OptionsPanel();
        accessoryPanel = new JPanel();
        formatPanel = new JPanel();
        filesOfTypeLabel = new JLabel();
        filterComboBox = createComboBox();
        formatSpringPanel = new JPanel();
        buttonsPanel = new ButtonPanel();
        newFolderButton = createButton();
        optionsButton = createButton();
        cancelButton = createButton();
        approveButton = createButton();

        GroupLayout layout = new GroupLayout(fc);
        fc.setLayout(layout);

        fileNameLine.setLayout(new GridBagLayout());
        fileNameLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        fileNameLabel.setText("Save As:");
        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.anchor = GridBagConstraints.EAST;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new Insets(7, 0, 0, 6);
        fileNameLine.add(fileNameLabel, gridBagConstraints);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.ipadx = 250;
        gridBagConstraints.insets = new Insets(7, 0, 0, 0);
        fileNameLine.add(fileNameTextField, gridBagConstraints);

        fileNameSpringPanel.setLayout(null);
        fileNameSpringPanel.setOpaque(false);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new Insets(0, 6, 0, 0);
        fileNameLine.add(fileNameSpringPanel, gridBagConstraints);

        if (OSXSystemProperties.OSVersion >= 1013) {
            fileNameLine.setBorder(new EmptyBorder(5, 0, 7, 0));
        } else if (OSXSystemProperties.OSVersion >= 1011) {
            fileNameLine.setBorder(new EmptyBorder(12, 0, 7, 0));
        } else {
            fileNameLine.setBorder(new EmptyBorder(12, 0, 14, 0));
        }

        savePanel.add(fileNameLine);

        if (OSXSystemProperties.OSVersion != 1013) {
            savePanel.add(separator);
        }

        viewsPanel.setLayout(new CardLayout());

        viewsPanel.add(columnView, "browser");

        columnView.addSelectionChangeListener(viewSelectionChangeListener);
        columnView.addSelectListener(viewSelectListener);

        if (viewModeControl != null && listView != null) {
            listView.addSelectionChangeListener(viewSelectionChangeListener);
            listView.addSelectListener(viewSelectListener);
            viewModeControl.setAlignmentY(0.5f);
            navigationPanel.add(viewModeControl);
            navigationPanel.add(Box.createHorizontalGlue());
            viewModeControl.addChangeListener(new ViewModeChangeListener());
            viewsPanel.add(listView, "list");
        }

        navigationPanel.add(directoryComboBox);
        navigationPanel.add(Box.createHorizontalGlue());
        navigationPanel.add(Box.createRigidArea(new Dimension(42, 0))); // make up for the search field we do not have

        configureTopPanel();

        topPanel.add(savePanel);
        topPanel.add(navigationPanel);

        int w = UIManager.getInt("FileChooser.sideBarWidth");
        int sidebarWidth = w > 0 ? w : 134;
        sidebarScrollPane.setMinimumSize(new Dimension(sidebarWidth, 0));
        splitPane.setDividerLocation(sidebarWidth); // used by subclass
        splitPane.putClientProperty("JSplitPane.style", "thin");

        sidebarScrollPane.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        sidebarScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        sidebarTree.setRootVisible(false);
        sidebarTree.setShowsRootHandles(true);
        sidebarScrollPane.setViewportView(sidebarTree);

        // A legacy scroll bar on a sidebar does not display a track
        sidebarScrollPane.getVerticalScrollBar().putClientProperty("JScrollBar.style", "sidebar");

        splitPane.setLeftComponent(sidebarScrollPane);
        splitPane.setRightComponent(viewsPanel);

        accessoryPanel.setLayout(new BorderLayout());
        optionsPanel.add(accessoryPanel);
        controlsPanel.add(optionsPanel);

        accessoryPanel.setOpaque(false);
        formatPanel.setOpaque(false);
        buttonsPanel.setOpaque(false);

        formatPanel.setLayout(new GridBagLayout());
        formatPanel.setBorder(BorderFactory.createEmptyBorder(6, 0, 6, 0));
        filesOfTypeLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        filesOfTypeLabel.setText("Format:");
        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = GridBagConstraints.EAST;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new Insets(0, 0, 0, 6);
        formatPanel.add(filesOfTypeLabel, gridBagConstraints);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.ipadx = 250;
        formatPanel.add(filterComboBox, gridBagConstraints);

        formatSpringPanel.setLayout(null);
        formatSpringPanel.setOpaque(false);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new Insets(0, 6, 0, 0);
        formatPanel.add(formatSpringPanel, gridBagConstraints);

        optionsPanel.add(formatPanel);
        optionsPanel.add(optionsSeparator);

        isOptionsEnabled = getOptionsPanelEnabledProperty();

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.anchor = GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;

        buttonsPanel.populate();
        controlsPanel.add(buttonsPanel);

        layout.setVerticalGroup(layout.createSequentialGroup()
                .addComponent(topPanel, GroupLayout.PREFERRED_SIZE, GroupLayout.PREFERRED_SIZE, GroupLayout.PREFERRED_SIZE)
                .addComponent(splitPane, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE, Short.MAX_VALUE)
                .addComponent(controlsPanel, GroupLayout.PREFERRED_SIZE, GroupLayout.PREFERRED_SIZE, GroupLayout.PREFERRED_SIZE));
        layout.setHorizontalGroup(layout.createParallelGroup()
                .addComponent(topPanel, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE, Short.MAX_VALUE)
                .addComponent(splitPane, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE, Short.MAX_VALUE)
                .addComponent(controlsPanel, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE, Short.MAX_VALUE));

        separator.putClientProperty("Quaqua.Component.visualMargin", new Insets(3, 0, 3, 0));

        TreeUI ui = createSidebarTreeUI();
        sidebarTree.setUI(ui);
        sidebarTree.putClientProperty("JTree.style", "sideBar");

        // sidebarTree must use largest font used by the TreeCellRenderer
        //   sidebarTree.setFont(UIManager.getFont("Tree.sideBar.selectionFont"));

        sidebarTree.setSelectionModel(new SidebarTreeSelectionModel());

        {
            InputMap map = sidebarTree.getInputMap(JComponent.WHEN_FOCUSED).getParent();
            KeyStroke ks = KeyStroke.getKeyStroke("shift meta A");
            Object v = map.get(ks);
            if (v != null && v.equals("clearSelection")) {
                InputMap newMap = new InputMap();
                newMap.setParent(map);
                newMap.put(ks, "selectApplicationsFolder"); // dummy name for now
                SwingUtilities.replaceUIInputMap(sidebarTree, JComponent.WHEN_FOCUSED, newMap);
            }
        }

        {
            int h = UIManager.getInt("FileChooser.sideBarRowHeight");
            if (h > 0) {
                sidebarTree.setRowHeight(h);
            }
        }

        int h;
        h = fileNameLabel.getPreferredSize().height;
        fileNameLabel.setMinimumSize(new Dimension(0, h));
        fileNameLabel.setPreferredSize(new Dimension(0, h));
        fileNameLabel.setMaximumSize(new Dimension(32767, h));

        h = fileNameTextField.getPreferredSize().height;
        fileNameTextField.setPreferredSize(new Dimension(0, h));
        fileNameTextField.setMinimumSize(new Dimension(0, h));
        fileNameTextField.setMaximumSize(new Dimension(32767, h));

        h = filesOfTypeLabel.getPreferredSize().height;
        filesOfTypeLabel.setMinimumSize(new Dimension(0, h));
        filesOfTypeLabel.setPreferredSize(new Dimension(0, h));
        filesOfTypeLabel.setMaximumSize(new Dimension(32767, h));

        h = filterComboBox.getPreferredSize().height;
        filterComboBox.setPreferredSize(new Dimension(0, h));
        filterComboBox.setMinimumSize(new Dimension(0, h));
        filterComboBox.setMaximumSize(new Dimension(32767, h));

        //Configure views
        FileRenderer fileRenderer = new FileRenderer(fc);
        columnView.setFileRenderer(fileRenderer);
        columnView.setMultipleSelection(isMultipleSelection());

        if (listView != null) {
            listView.setFileRenderer(fileRenderer);
            listView.setMultipleSelection(isMultipleSelection());
        }

        columnView.setModel(subtreeModel);
        if (listView != null) {
            listView.setModel(subtreeModel);
        }

        // Configure Sidebar Panel
        sidebarScrollPane.putClientProperty("Quaqua.Component.visualMargin", new Insets(3, 2, 3, 2));

        // Configure Format Panel
        installChoosableFileFilters(fc.getChoosableFileFilters());

        // Configure Accessory Panel
        installAccessory(fc.getAccessory());

        // Text assignment
        newFolderButton.setText(newFolderButtonText);
        newFolderButton.setToolTipText(newFolderToolTipText);
        fileNameLabel.setText(fileNameLabelText);
        fileNameLabel.setDisplayedMnemonic(fileNameLabelMnemonic);

        approveButton.setText(getApproveButtonText(fc));
        // Note: Metal does not use mnemonics for approve and cancel
        approveButton.addActionListener(getApproveSelectionAction());
        approveButton.setToolTipText(getApproveButtonToolTipText(fc));

        optionsButton.setText(optionsButtonText);
        optionsButton.setToolTipText(optionsButtonToolTipText);
        optionsButton.addActionListener(optionsAction);

        cancelButton.setText(cancelButtonText);
        cancelButton.setToolTipText(cancelButtonToolTipText);
        cancelButton.addActionListener(getCancelSelectionAction());

        {
            Dimension od = optionsButton.getPreferredSize();
            Dimension cd = cancelButton.getPreferredSize();
            Dimension ad = approveButton.getPreferredSize();
            int width = Math.max(Math.max(Math.max(od.width, cd.width), ad.width), 69);
            optionsButton.setPreferredSize(new Dimension(width, od.height));
            cancelButton.setPreferredSize(new Dimension(width, cd.height));
            approveButton.setPreferredSize(new Dimension(width, ad.height));
        }

        newFolderButton.setMargin(new Insets(0, 10, 0, 10));

        updateControlButtonVisibility();

        fileNameTextField.setDocument(new FilenameDocument());
        if (defaultInitialSaveFileName != null) {
            setFileNameTextField(defaultInitialSaveFileName);
        }

        // End of Text assignment

        // Model and Renderer assignment
        directoryComboBoxModel = createDirectoryComboBoxModel(fc);
        directoryComboBox.setModel(directoryComboBoxModel);
        directoryComboBox.setRenderer(createDirectoryComboBoxRenderer(directoryComboBox));
        sidebarTreeModel = new SidebarTreeModel(fc, new TreePath(model.getRoot()), model);
        sidebarTree.setModel(sidebarTreeModel);
        sidebarTree.setCellRenderer(createSidebarCellRenderer(fc));
        for (int i = sidebarTree.getRowCount() - 1; i >= 0; i--) {
            sidebarTree.expandRow(i);
        }

        filterComboBoxModel = createFilterComboBoxModel();
        filterComboBox.setModel(filterComboBoxModel);
        filterComboBox.setRenderer(createFilterComboBoxRenderer(filterComboBox));
        // Model and Renderer assignment

        // Listener assignment
        directoryComboBox.addActionListener(directoryComboBoxAction);
        newFolderButton.addActionListener(getNewFolderAction());
        fileNameTextField.addFocusListener(new SaveTextFocusListener());
        fileNameTextField.getDocument().addDocumentListener(new SaveTextDocumentListener());
        fileNameTextField.addActionListener(getApproveSelectionAction());
        sidebarTree.addTreeSelectionListener(createSidebarSelectionListener(fc));
        // End of listener assignment

        // Focus traversal
        sidebarScrollPane.setFocusable(false);
        sidebarScrollPane.getVerticalScrollBar().setFocusable(false);
        sidebarScrollPane.getHorizontalScrollBar().setFocusable(false);

        // Drag and drop assignment
        fileTransferHandler = new FileTransferHandler(fc);

        KeyListener kl = new TextKeyListener();

        Component[] dropComponents = {
                fc,
                accessoryPanel,
                approveButton,
                columnView,
                buttonsPanel,
                optionsButton,
                cancelButton,
                controlsPanel,
                directoryComboBox,
                fileNameLabel,
                savePanel,
                fileNameSpringPanel,
                fileNameTextField,
                filesOfTypeLabel,
                filterComboBox,
                formatPanel,
                formatSpringPanel,
                listView,
                navigationPanel,
                newFolderButton,
                //nextButton,
                //previousButton,
                separator,
                splitPane,
                viewModeControl,
                viewsPanel,
                sidebarTree,
                sidebarScrollPane
        };
        for (int i = 0; i < dropComponents.length; i++) {
            Component c = dropComponents[i];
            if (c != null) {
                new DropTarget(c, DnDConstants.ACTION_COPY, fileTransferHandler);
                c.addKeyListener(kl);
            }
        }
        // End of drag and drop assignment

        // Change component visibility to match the dialog type
        boolean isSave = (fc.getDialogType() == JFileChooser.SAVE_DIALOG) || (fc.getDialogType() == JFileChooser.CUSTOM_DIALOG);
        fileNameTextField.setEnabled(isSave);
        savePanel.setVisible(isSave);

        // Preview column
        doPreviewComponentChanged(null);

        // Button state
        updateApproveButtonState();
        updateOptionsPanelVisibility(); // also update buttons

        // Configure size of split pane
        int columnWidth = 207 + 16;
        splitPane.setPreferredSize(new Dimension(sidebarWidth + 2 * columnWidth, 298));
        splitPane.setMaximumSize(new Dimension(100000, 100000));

        // register key events with window
        // do not register handlers for ordinary characters, use key typed event handlers instead

        ActionMap am = fc.getActionMap();
        InputMap globalInputMap = fc.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        for (KeyStroke ks : KEYSTROKES) {
            if ((ks.getModifiers() & KeyEvent.META_MASK) != 0) {
                globalInputMap.put(ks, ks);
            }
            am.put(ks, keyListenerAction);
        }

        // Enforce layout, so that the selected file is visible when the
        // file chooser is opened with its preferred size.
        Dimension ps = fc.getPreferredSize();
        fc.setBounds(0, 0, ps.width, ps.height);
        fc.doLayout();
    }

    protected TreeUI createSidebarTreeUI() {
        return new SidebarTreeUI();
    }

    /**
     * Create a text field.
     */
    protected JTextField createTextField(String theName) {
        JTextField t = new JTextField() {
            @Override
            public String toString() {
                return theName;
            }
        };
        t.setOpaque(false);
        return t;
    }

    /**
     * Create a button.
     */
    protected JButton createButton() {
        return createButton((String) null);
    }

    /**
     * Create a button.
     */
    protected JButton createButton(String text) {
        JButton b = new JButton(text);
        return b;
    }

    protected JComboBox createDirectoryComboBox() {
        return new DirectoryComboBox();
    }

    private class DirectoryComboBox extends JComboBox {

        public DirectoryComboBox() {
            putClientProperty("JComboBox.style", "textured");
            setAlignmentY(0.5f);
            putClientProperty("Quaqua.Component.visualMargin", new Insets(1, 1, 1, 1));
        }

        @Override
        public Dimension getPreferredSize()
        {
            // native is 200 wide but we have more room so why not use it
            // The trick here is that we cannot predict the preferred height
            Dimension d = super.getPreferredSize();
            int width = Math.max(d.width, 300);
            int height = d.height;
            return new Dimension(width, height);
        }

        @Override
        public Dimension getMaximumSize() {
            return getPreferredSize();
        }
    }

    protected JComboBox createComboBox() {
        JComboBox b = new JComboBox();
        return b;
    }

    protected JSeparator createSeparator() {
        JSeparator s = new JSeparator();
        s.setBorder(AquaLookAndFeel.NOTHING_BORDER);
        return s;
    }

    protected JSeparator createOptionsSeparator() {
        JSeparator s = new JSeparator();
        s.setBorder(AquaLookAndFeel.NOTHING_BORDER);
        return s;
    }

    @Override
    public void uninstallComponents(JFileChooser fc) {
        fc.removeAll();

        // Dispose the models
        fileSystemModel.dispose();
        for (FileSystemTreeModel m : savedSearches.values()) {
            m.dispose();
        }

        // Remove listeners on UI components
        optionsButton.removeActionListener(optionsAction);
        cancelButton.removeActionListener(getCancelSelectionAction());
        approveButton.removeActionListener(getApproveSelectionAction());
        fileNameTextField.removeActionListener(getApproveSelectionAction());
    }

    @Override
    protected void installListeners(JFileChooser fc) {
        super.installListeners(fc);
        hierarchyListener = createHierarchyListener(fc);
        if (hierarchyListener != null) {
            fc.addHierarchyListener(hierarchyListener);
        }
        fc.addPropertyChangeListener(filterComboBoxModel);
        AppearanceManager.installListener(fc);
    }

    @Override
    protected void uninstallListeners(JFileChooser fc) {
        super.uninstallListeners(fc);
        AppearanceManager.uninstallListener(fc);
        if (hierarchyListener != null) {
            fc.removeHierarchyListener(hierarchyListener);
        }
        fc.removePropertyChangeListener(filterComboBoxModel);
    }

    private Locale getLocale() {
        try {
            return fc.getLocale();
        } catch (IllegalComponentStateException e) {
            return Locale.getDefault();
        }
    }

    @Override
    protected void installDefaults(JFileChooser fc) {
        super.installDefaults(fc);

        boolean isShowAllFiles = OSXSystemProperties.isShowAllFiles();
        fc.setFileHidingEnabled(!isShowAllFiles);

        setPackageTraversable(UIManager.getBoolean(PACKAGE_TRAVERSABLE_PROPERTY));
        setApplicationTraversable(UIManager.getBoolean(APPLICATION_TRAVERSABLE_PROPERTY));
    }

    @Override
    protected void installStrings(JFileChooser fc) {
        super.installStrings(fc);

        Locale l;
        try {
            l = getLocale();
        } catch (IllegalComponentStateException e) {
            l = Locale.getDefault();
        }

        // FIXME - We must not read these strings from the UIManager, as long
        //         as we don't provide them with our own Look and Feel. This
        //         is, because these strings are version dependent, and thus
        //         are not necessarily in sync with what we need in our UI.
        chooseButtonText = UIManager.getString("FileChooser.chooseButtonText"/*,l*/);

        fileNameLabelMnemonic = UIManager.getInt("FileChooser.fileNameLabelMnemonic");
        fileNameLabelText = UIManager.getString("FileChooser.fileNameLabelText"/*,l*/);
        // XXX - Localize "Save as:" text.
        //if (fileNameLabelText == null || fileNameLabelText.charAt(fileNameLabelText.length() -1) != ':') fileNameLabelText = "Save as:";

        ///filesOfTypeLabelMnemonic = UIManager.getInt("FileChooser.filesOfTypeLabelMnemonic");
        ///filesOfTypeLabelText = UIManager.getString("FileChooser.filesOfTypeLabelText"/*,l*/);

        ///upFolderToolTipText = UIManager.getString("FileChooser.upFolderToolTipText"/*,l*/);
        ///upFolderAccessibleName = UIManager.getString("FileChooser.upFolderAccessibleName"/*,l*/);

        ///homeFolderToolTipText = UIManager.getString("FileChooser.homeFolderToolTipText"/*,l*/);
        ///homeFolderAccessibleName = UIManager.getString("FileChooser.homeFolderAccessibleName"/*,l*/);

        optionsButtonText = UIManager.getString("FileChooser.optionsButtonText"/*,l*/);
        optionsButtonToolTipText = UIManager.getString("FileChooser.optionsToolTipText"/*,l*/);
        cancelButtonText = UIManager.getString("FileChooser.cancelButtonText"/*,l*/);
        cancelButtonToolTipText = UIManager.getString("FileChooser.cancelToolTipText"/*,l*/);

        // New Folder Dialog
        newFolderErrorText = getString("FileChooser.newFolderErrorText", l, "Error occurred during folder creation");
        newFolderExistsErrorText = getString("FileChooser.newFolderExistsErrorText", l, "That name is already taken");
        newFolderButtonText = getString("FileChooser.newFolderButtonText", l, "New Folder");
        newFolderTitleText = getString("FileChooser.newFolderTitleText", l, "New Folder");
        newFolderDialogPrompt = getString("FileChooser.newFolderPromptText", l, "Name of new folder:");
        newFolderDefaultName = getString("FileChooser.untitledFolderName", l, "untitled folder");
        newFolderToolTipText = UIManager.getString("FileChooser.newFolderToolTipText"/*, l*/);
        ///newFolderAccessibleName = getString("FileChooser.newFolderAccessibleName", l, newFolderTitleText);
        goToFolderDialogPrompt = getString("FileChooser.goToFolderPromptText", l, "Go to the folder:");
        goToFolderCancelButtonText = getString("FileChooser.goToFolderCancelButtonText", l, "Cancel");
        goToFolderAcceptButtonText = getString("FileChooser.goToFolderAcceptButtonText", l, "Accept");
        goToFolderErrorText = getString("FileChooser.goToFolderErrorText", l, "The folder can\u2019t be found.");
        defaultInitialSaveFileName = getString("FileChooser.defaultSaveFileName", l, "Untitled");
    }

    /**
     * FIXME - This could be moved up to BasicFileChooserUI.
     */
    @Override
    public JPanel getAccessoryPanel() {
        return accessoryPanel;
    }

    /**
     * Gets a locale dependent string.
     */
    private String getString(String string, Locale l, String defaultValue) {
        String value = UIManager.getString(string/*, l*/);
        return (value == null) ? defaultValue : value;
    }

    protected HierarchyListener createHierarchyListener(JFileChooser fc) {
        return new FileChooserHierarchyListener();
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        reconfigureChooser();
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        reconfigure(optionsPanel);
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        if (isRecursiveLayoutCall) {
            // return null to use the size computed by the layout manager
            return null;
        }
        isRecursiveLayoutCall = true;
        try {
            // The options panel should be included in the preferred size even if it is not currently displayed.
            Dimension d = c.getPreferredSize();
            if (!optionsPanel.isVisible()) {
                Dimension od = optionsPanel.getPreferredSize();
                return new Dimension(Math.max(d.width, od.width), d.height + od.height);
            }
            return d;
        } finally {
            isRecursiveLayoutCall = false;
        }
    }

    @Override
    public Dimension getMinimumSize(JComponent c) {
        if (isRecursiveLayoutCall) {
            // return null to use the size computed by the layout manager
            return null;
        }
        isRecursiveLayoutCall = true;
        try {
            // The options panel should be included in the minimum size even if it is not currently displayed.
            Dimension d = c.getMinimumSize();
            if (!optionsPanel.isVisible()) {
                Dimension od = optionsPanel.getMinimumSize();
                return new Dimension(Math.max(d.width, od.width), d.height + od.height);
            }
            return d;
        } finally {
            isRecursiveLayoutCall = false;
        }
    }

    @Override
    public Dimension getMaximumSize(JComponent c) {
        return new Dimension(100000, 100000);
    }

    public int getViewMode() {
        return viewMode;
    }

    public void setViewMode(int mode) {
        if (mode == ViewModeControl.COLUMN_VIEW || listView != null && mode == ViewModeControl.LIST_VIEW) {
            if (mode != viewMode) {
                viewMode = mode;
                installSelectedView(false, false);
                if (activeView != null) {
                    activeView.requestFocusInWindow();
                }
            }
        }
    }

    private void installSelectedView(boolean forceInstall, boolean forceReconfigure) {
        if (!fc.isShowing() && !forceInstall) {
            isViewInstalled = false;
        } else {
            isViewInstalled = true;
            CardLayout cl = (CardLayout) viewsPanel.getLayout();
            FileChooserView viewToInstall = getViewForMode(viewMode);
            FileChooserView oldView = activeView;
            if (viewToInstall != oldView) {
                if (oldView != null) {
                    oldView.setActive(false);
                }
                activeView = viewToInstall;
                String viewName = getViewNameForMode(viewMode);
                cl.show(viewsPanel, viewName);
                activeView.setActive(true);
                forceReconfigure = true;
            }

            if (forceReconfigure) {
                activeView.reconfigure();
                model.invalidateAll();
            }

            int source = oldView != null ? SELECT_DIRECTORY_NEW_VIEW : SELECT_DIRECTORY_TO_INITIALIZE;
            updateSelection(source);
        }
    }

    private FileChooserView getViewForMode(int viewMode) {
        if (viewMode == ViewModeControl.LIST_VIEW) {
            return listView;
        } else {
            return columnView;
        }
    }

    private String getViewNameForMode(int viewMode) {
        if (viewMode == ViewModeControl.LIST_VIEW) {
            return "list";
        } else {
            return "browser";
        }
    }

    @Override
    public void createModel() {
        // FIXME - We should not overwrite the FileSystemView attribute of the JFileChooser.
        fc.setFileSystemView(AquaFileSystemView.getAquaFileSystemView());

        // FIXME - We should not overwrite the FileView attribute of the JFileChooser.

        fileView = AquaFileSystemView.getAquaFileSystemView().createFileView(fc);
        fc.setFileView(fileView);

        fileSystemModel = new FileSystemTreeModel(fc);
        model = fileSystemModel;
        subtreeModel = new SubtreeTreeModel(model);
        savedSearches = new HashMap<>();
    }

    public FileSystemTreeModel getSavedSearchTreeModel(File savedSearchFile) {
        FileSystemTreeModel m = savedSearches.get(savedSearchFile);
        if (m == null) {
            m = createSavedSearchTreeModel(savedSearchFile);
            savedSearches.put(savedSearchFile, m);
        }
        return m;
    }

    protected FileSystemTreeModel createSavedSearchTreeModel(File savedSearchFile) {
        return new SavedSearchFileSystemTreeModel(fc, savedSearchFile);
    }

    /**
     * Update the selection in the active view to match the selected files of the JFileChooser.
     *
     * This method maps the (application provided) file chooser selection to a (possibly new) view selection and a
     * (possibly new) view root.
     *
     * In a column view, all elements of a multiple selection must reside in the same parent directory. Although not
     * mandatory for a list view, we enforce the common parent rule in all cases to ensure that the file selection is
     * viewable. The file chooser selection is updated as needed to be consistent with what we display in the view.
     *
     * If the view is displaying a smart folder (saved search), we display the file selection in the the smart folder
     * if possible. Otherwise, we switch to the file system model.
     */
    private void updateSelection(int source) {

        if (activeView == null) {
            return;
        }

        java.util.List<File> files = null;
        TreePath directoryPath = null;

        /*
          Special case for save dialogs: Ordinary files are not normally acceptable in a save dialog view: the user may
          click on a file but not actually select it. However, an application may set the selected file of the file
          chooser as a way of selecting a directory and a default file name.
        */

        if (fc.getDialogType() == JFileChooser.SAVE_DIALOG && fileNameTextField != null && isFileNameFieldVisible()) {
            files = getChooserSelection();
            if (!files.isEmpty()) {
                File f = files.get(0);
                files.clear();
                File parent = f.getParentFile();
                if (fc.isTraversable(parent)) {
                    files.add(parent);
                    directoryPath = model.toPath(parent, null);
                    ++isAdjusting;
                    setFileName(f.getName());
                    --isAdjusting;
                }
            }
        }

        if (files == null) {
            files = getNormalizedChooserSelection();      // ensures files are absolute
        }

        java.util.List<File> originalFiles = files;
        java.util.List<TreePath> list = new ArrayList<TreePath>(files.size());

        TreePath commonParentPath = null;
        for (int i = 0; i < files.size(); i++) {
            File file = files.get(i);
            TreePath fullPath = model.toPath(file, null);

            if (i == 0) {
                commonParentPath = fullPath.getParentPath();
            } else {
                TreePath parentPath = fullPath.getParentPath();
                if (!Objects.equals(commonParentPath, parentPath)) {
                    continue;
                }
            }

            list.add(fullPath);
        }

        if (directoryPath == null) {
            directoryPath = commonParentPath;
        }

        if (directoryPath != null) {

            Runnable r = new Runnable() {
                @Override
                public void run() {
                    java.util.List<TreePath> subtreePaths = getSubtreePaths(list);
                    setViewSelection(subtreePaths);
                    finishUpdateSelection(originalFiles, list);
                }
            };

            FileSystemTreeModel.Node n = (FileSystemTreeModel.Node) directoryPath.getLastPathComponent();
            File dir = n != null ? n.getResolvedFile() : new File("/");

                /*
                  Because this method may be called from a property change listener, it is better to defer any change to
                  the file chooser state, otherwise other property change listeners may be invoked in the wrong order.
                */

            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    selectDirectory(dir, source, false, r);
                }
            });

        } else {

            // The file chooser selection is not valid.
            // Update the view selection based on the current directory. We can not assume that a change event will
            // be delivered for the current directory because it might not have changed.

            Runnable r = new Runnable() {
                @Override
                public void run() {
                    updateSelectedFiles(null);
                    updateApproveButtonState();
                }
            };

            /*
              Because this method may be called from a property change listener, it is better to defer any change to the
              file chooser state, otherwise other property change listeners may be invoked in the wrong order.
            */

            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    File dir = fc.getCurrentDirectory();
                    selectDirectory(dir, source, true, r);
                }
            });
        }
    }

    private void finishUpdateSelection(java.util.List<File> originalFiles, java.util.List<TreePath> newSelection) {
        if (originalFiles.size() != newSelection.size()) {
            // We filtered out some selected files because they did not share a common parent
            int count = newSelection.size();
            java.util.List<File> files = new ArrayList<>(count);
            for (TreePath path : newSelection) {
                File f = ((FileInfo) path.getLastPathComponent()).getFile();
                files.add(f);
            }
            updateSelectedFiles(files);
        }

        updateApproveButtonState();
    }

    private java.util.List<TreePath> getSubtreePaths(java.util.List<TreePath> paths) {
        java.util.List<TreePath> result = new ArrayList<>();
        for (TreePath fullPath : paths) {
            TreePath subtreePath = subtreeModel.toSubPath(fullPath);
            if (subtreePath != null) {
                result.add(subtreePath);
            }
        }

        return result;
    }

    private TreePath getSubtreePath(File f) {
        if (f != null) {
            TreePath fullPath = model.toPath(f, subtreeModel.getPathToRoot());
            if (fullPath != null) {
                return subtreeModel.toSubPath(fullPath);
            }
        }
        return null;
    }

    /**
     * Returns true, if the dir name field contains a dir name.
     */
    private boolean isFileNameFieldValid() {
        String string = getFileName();
        return string != null && !string.equals("");
    }

    /**
     * Returns true, if the dir name field is visible.
     */
    private boolean isFileNameFieldVisible() {
        return (fc.getDialogType() == JFileChooser.SAVE_DIALOG) || (fc.getDialogType() == JFileChooser.CUSTOM_DIALOG);
    }

    private void updateApproveButtonState() {
        if (fc.getControlButtonsAreShown()) {
            boolean isEnabled = computeApproveButtonEnabled();
            setApproveButtonEnabled(isEnabled);
        }
    }

    private boolean computeApproveButtonEnabled() {
        if (fc.getDialogType() == JFileChooser.SAVE_DIALOG) {
            return isAcceptableSaveDialogState();
        }

        if (isFileNameFieldVisible() && isFileNameFieldValid() && fc.getFileSelectionMode() == JFileChooser.FILES_ONLY) {
            return true;
        }

        java.util.List<File> files = getChooserSelection();
        if (files.isEmpty()) {
            return fc.isDirectorySelectionEnabled() && isAcceptable((fc.getCurrentDirectory()));
        }

        for (File f : files) {
            if (!isAcceptable(f)) {
                return false;
            }
        }

        return true;
    }

    private boolean isAcceptableSaveDialogState() {
        File dir = fc.getCurrentDirectory();

        if (dir == null || !dir.isDirectory()) { // TBD: could test for directory being writable
            return false;
        }

        if (!isFileNameFieldValid()) {
            return false;
        }

        return true;
    }

    private boolean isSelected(File f) {
        if (f != null) {
            File[] fs = fc.getSelectedFiles();
            if (fs != null) {
                for (File sf : fs) {
                    if (f.equals(sf)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private boolean isAcceptable(File f) {
        if (f == null) {
            return false;
        }

        TreePath path = model.toPath(f, null);
        if (path != null) {
            Object pc = path.getLastPathComponent();
            if (pc instanceof FileInfo) {
                FileInfo info = (FileInfo) pc;
                if (info.getFile().equals(f)) { // handle obscure behavior of toPath() when file does not exist
                    return info.isAcceptable();
                }
            }
        }
        return false;
    }

    private boolean isMultipleSelection() {
        return fc != null && fc.isMultiSelectionEnabled() && fc.getDialogType() != JFileChooser.SAVE_DIALOG;
    }

    private void setApproveButtonEnabled(boolean isEnabled) {
        if (fc.getControlButtonsAreShown()) {
            approveButton.setEnabled(isEnabled);
            if (isEnabled) {
                JRootPane rp = approveButton.getRootPane();
                if (rp != null) {
                    rp.setDefaultButton(approveButton);
                }
            }
        }
    }

    private void updateApproveButtonText() {
        approveButton.setText(getApproveButtonText(fc));
        approveButton.setToolTipText(getApproveButtonToolTipText(fc));
        approveButton.setMnemonic(getApproveButtonMnemonic(fc));
        //cancelButton.setToolTipText(getCancelButtonToolTipText(fc));
    }

    /**
     * Update the file chooser current directory, selected files, and the approve button enabled state based on the
     * selection in the active view. The active view selection may be updated if it is not consistent with the file
     * chooser configuration. If the file chooser current directory is changed, update the directory combo box to be
     * consistent.
     */

    private void respondToUISelectionChange() {

        java.util.List<TreePath> paths = getNormalizedUISelection();

        /*
          Update the file chooser current directory based on the normalized view selection (column view only).
        */

        if (viewMode == ViewModeControl.COLUMN_VIEW) {
            TreePath dirPath = getColumnViewCurrentDirectoryPath(paths);
            if (dirPath != null) {
                updateCurrentDirectory(dirPath);
                updateComboBoxModel(dirPath);
            }
        }

        /*
          Update the file chooser selected files based on the normalized view selection. The current directory is
          preserved.
        */

        java.util.List<File> fs = getFileSelectionFromViewSelection(paths);
        installSelectedFiles(fs);

        if (!paths.isEmpty()) {
            TreePath path = paths.get(0);
            model.lazyInvalidatePath(path);
            if (fc.isDisplayable()) {
                model.validatePath(path);
            }
        }

        updateApproveButtonState();
    }

    /**
     * Determine the (possibly new) current directory based on the selected files in the column view.
     */
    private TreePath getColumnViewCurrentDirectoryPath(java.util.List<TreePath> paths) {
        if (paths.isEmpty()) {
            return null;
        }

        TreePath path = paths.get(0);
        FileInfo info = (FileInfo) path.getLastPathComponent();

        if (paths.size() == 1 && info.isTraversable()) {
            return path;
        }

        // If there are multiple selected files, they should have a common parent.
        return path.getParentPath();
    }

    /**
     * Determine the (possibly new) selected files based on the selected files in the active view. The special case is
     * where a single unacceptable, traversable node is selected in the view.
     */
    private java.util.List<File> getFileSelectionFromViewSelection(java.util.List<TreePath> paths) {
        java.util.List<File> fs = new ArrayList<>();

        for (TreePath p : paths) {
            FileSystemTreeModel.Node node = (FileSystemTreeModel.Node) p.getLastPathComponent();
            if (paths.size() == 1 && !node.isAcceptable()) {
                // Must be a traversable node
                return new ArrayList<>();
            }
            File f = node.getFile();
            fs.add(f);
        }
        return fs;
    }

    /**
     * Set the current directory in the file chooser.
     */
    private void updateCurrentDirectory(TreePath subtreePath) {
        if (isAdjusting == 0) {
            FileInfo info = (FileInfo) subtreePath.getLastPathComponent();
            File f = info.getResolvedFile();
            File current = fc.getCurrentDirectory();
            if (!Objects.equals(current, f)) {
                ++isAdjusting;
                fc.setSelectedFiles(null);
                fc.setCurrentDirectory(f);
                --isAdjusting;
            }
        }
    }

    /**
     * Set the selected file in the file chooser.
     */
    private void updateSelectedFile(File file) {
        if (isAdjusting == 0) {
            ++isAdjusting;
            if (isMultipleSelection()) {
                fc.setSelectedFiles(new File[]{file});
            } else {
                fc.setSelectedFile(file);
            }
            --isAdjusting;
        }
    }

    /**
     * Set the selected files in the file chooser. The file chooser may change its current directory.
     */
    private void updateSelectedFiles(java.util.List<File> files) {
        if (isAdjusting == 0) {
            ++isAdjusting;
            if (isMultipleSelection()) {
                File[] fs = files != null ? files.toArray(new File[files.size()]) : null;
                fc.setSelectedFiles(fs);
            } else {
                fc.setSelectedFile(files != null && !files.isEmpty() ? files.get(0) : null);
            }
            --isAdjusting;
        }
    }

    /**
     * Set the selected files in the file chooser. The file chooser current directory is restored if needed.
     */
    private void installSelectedFiles(java.util.List<File> fs) {
        if (isAdjusting == 0) {
            ++isAdjusting;
            // Setting the selected file(s) will probably reset the current directory. We do not want that.
            File currentDirectory = fc.getCurrentDirectory();
            fc.setSelectedFiles(fs.toArray(new File[fs.size()]));
            fc.setCurrentDirectory(currentDirectory);
            --isAdjusting;
        }
    }

    private TreePath first(java.util.List<TreePath> paths) {
        return paths.isEmpty() ? null : paths.get(0);
    }

    protected ChangeListener createViewSelectionChangeListener(JFileChooser fc) {
        return new ViewSelectionChangeListener();
    }

    protected FileChooserView.SelectListener createViewSelectListener(JFileChooser fc) {
        return new ViewSelectListener();
    }

    /**
     * Selection change listener for the view.
     */
    protected class ViewSelectionChangeListener implements ChangeListener {
        @Override
        public void stateChanged(ChangeEvent e) {
            if (isAdjusting == 0) {
                respondToUISelectionChange();
            }
        }
    }

    /**
     * Respond to double click on a item in the view that is either traversable or acceptable (or both) or to a single
     * click on a file in a Save dialog.
     *
     * A double click on a traversable item installs the item as the current directory (list view only), which may
     * update the display subtree root and the sidebar selection. (Nothing special happens in a column view.)
     *
     * A double click on an acceptable (but not traversable) item selects that item as the selected file (possibly
     * adding it to the existing selection) and dismisses the dialog.
     *
     * A single click on a file in a Save dialog installs the file name in the file name text field.
     */
    protected class ViewSelectListener implements FileChooserView.SelectListener {
        @Override
        public void select(TreePath path) {
            FileInfo info = (FileInfo) path.getLastPathComponent();
            File f = info.getFile();
            if (info.isTraversable()) {
                if (viewMode == ViewModeControl.LIST_VIEW) {
                    AquaFileChooserUI.this.selectDirectory(f, SELECT_DIRECTORY_BY_DOUBLE_CLICK);
                }
            } else if (info.isAcceptable()) {
                /*
                  If the item is a member of the selection, the selection is not changed. Otherwise, the item is
                  selected.
                */
                if (!isSelected(f)) {
                    java.util.List<File> files = new ArrayList<>();
                    files.add(f);
                    installSelectedFiles(files);
                }
                maybeApproveSelection();
            } else if (fc.getDialogType() == JFileChooser.SAVE_DIALOG) {
                setFileName(f.getName());
            }
        }
    }

    protected class ViewModeChangeListener implements ChangeListener {
        @Override
        public void stateChanged(ChangeEvent e) {
            setViewMode(viewModeControl.getSelectedViewMode());
        }
    }

    /* The following methods are used by the PropertyChange Listener */
    private void doSelectedFileChanged(PropertyChangeEvent e) {
        updateSelection(SELECT_DIRECTORY_FROM_API);
    }

    private void doSelectedFilesChanged(PropertyChangeEvent e) {
        updateSelection(SELECT_DIRECTORY_FROM_API);
    }

    private void doDirectoryChanged(PropertyChangeEvent e) {
        if (activeView == null) {
            return;
        }

        /*
          The application has changed the file chooser current directory. We map that to a change in our selected
          directory. Programmatic selection of a directory in NSOpenPanel always chooses the best sidebar root.
        */

        File dir = (File) e.getNewValue();
        selectDirectory(dir, SELECT_DIRECTORY_FROM_API);
    }

    /**
     * Update a model after a change in the file chooser configuration that might impact the model.
     */
    private void updateModel() {
        model.invalidateAll();

        if (fc.isShowing() && activeView != null) {
            reconfigureView();
            java.util.List<TreePath> oldSelection = activeView.getSelection();
            java.util.List<TreePath> newSelection = getNormalizedUISelection();
            if (oldSelection.equals(newSelection)) {
                model.validatePath(subtreeModel.toFullPath(first(oldSelection)));
                updateApproveButtonState();
            } else {
                if (viewMode == ViewModeControl.COLUMN_VIEW && newSelection.isEmpty()) {
                    File dir = fc.getCurrentDirectory();
                    TreePath viewPath = getSubtreePath(dir);
                    if (viewPath != null) {
                        newSelection.add(viewPath);
                    }
                }
                setViewSelection(newSelection);
                respondToUISelectionChange();
            }

            /*
              TBD: would like to make sure that the current selection is still visible after the model change. The issue
              is that validation is asynchronous, so we do not know when to update the scroll pane.
            */
        }
    }

    private void doFilterChanged(PropertyChangeEvent e) {
        clearIconCache();
        updateModel();
    }

    private void doFileHidingChanged(PropertyChangeEvent e) {
        updateModel();
    }

    private void doPackageTraversableChanged(PropertyChangeEvent e) {
        Boolean b = (Boolean) e.getNewValue();
        setPackageTraversable(b != null ? b : false);
    }

    private void doApplicationTraversableChanged(PropertyChangeEvent e) {
        Boolean b = (Boolean) e.getNewValue();
        setApplicationTraversable(b != null ? b : false);
    }

    private void setPackageTraversable(boolean b) {
        FileView fv = fc.getFileView();
        if (fv instanceof AquaFileView) {
            AquaFileView q = (AquaFileView) fv;
            if (b != q.isPackageTraversable()) {
                q.setPackageTraversable(b);
                updateModel();
            }
        }
    }

    private void setApplicationTraversable(boolean b) {
        FileView fv = fc.getFileView();
        if (fv instanceof AquaFileView) {
            AquaFileView q = (AquaFileView) fv;
            if (b != q.isApplicationTraversable()) {
                q.setApplicationTraversable(b);
                updateModel();
            }
        }
    }

    private void doFileViewChanged(PropertyChangeEvent e) {
        updateModel();
    }

    private void doFileSelectionModeChanged(PropertyChangeEvent e) {
        updateApproveButtonText();
        updateApproveButtonState();
    }

    private void doMultiSelectionChanged(PropertyChangeEvent e) {
        if (!isMultipleSelection()) {
            // Here we want to respond to a change event
            fc.setSelectedFiles(null);
        }
    }

    private void doChoosableFilterChanged(PropertyChangeEvent e) {
        installChoosableFileFilters((FileFilter[]) e.getNewValue());
        updateModel();
    }

    private void doAccessoryChanged(PropertyChangeEvent e) {
        installAccessory((JComponent) e.getNewValue());
    }

    private void installChoosableFileFilters(FileFilter[] ffs) {
        formatPanel.setVisible(ffs.length > 1);
        accessoryPanel.revalidate();
        accessoryPanel.repaint();
        updateOptionsPanelVisibility();
    }

    private void installAccessory(JComponent c) {
        if (accessoryPanel != null) {
            accessoryPanel.removeAll();
            if (c != null) {
                Border b = c.getBorder();
                if (!(b instanceof EmptyBorder)) {
                    c.setBorder(null);
                    c.revalidate();
                }
                JPanel wrapper = new JPanel();
                wrapper.setOpaque(false);
                wrapper.setLayout(new BorderLayout());
                wrapper.setBorder(new EmptyBorder(5, 15, 5, 15));
                wrapper.add(c, BorderLayout.CENTER);
                wrapper.revalidate();
                accessoryPanel.add(wrapper, BorderLayout.CENTER);
            }
            accessoryPanel.setVisible(c != null);
            updateOptionsPanelVisibility();
        }
    }

    private void updateOptionsPanelVisibility() {
        // TBD: could respond to changes to accessoryPanel and formatPanel visible state
        optionsPanel.setVisible(
                (!isOptionsButtonAvailable || fc.getDialogType() != JFileChooser.OPEN_DIALOG || isOptionsEnabled)
                        && (accessoryPanel.isVisible() || formatPanel.isVisible()));
        optionsPanel.revalidate();
        optionsPanel.repaint();
        updateControlButtonVisibility();
    }

    private void doOptionsPanelEnabledChanged() {
        if (isOptionsButtonAvailable) {
            boolean b = getOptionsPanelEnabledProperty();
            if (b != isOptionsEnabled) {
                isOptionsEnabled = b;
                updateOptionsPanelVisibility();
            }
        }
    }

    private boolean getOptionsPanelEnabledProperty() {
        Boolean b = AquaUtils.getBooleanProperty(fc, OPTIONS_PANEL_ENABLED_PROPERTY);
        return Boolean.TRUE.equals(b);
    }

    private void setOptionsPanelEnabledProperty(boolean b) {
        if (isOptionsButtonAvailable) {
            ++isAdjusting;
            fc.putClientProperty(OPTIONS_PANEL_ENABLED_PROPERTY, b);
            --isAdjusting;
        }
    }

    private void doCanCreateDirectoriesChanged(@NotNull PropertyChangeEvent e) {
        updateControlButtonVisibility();
    }

    private boolean getCanCreateDirectoriesProperty() {
        Boolean b = AquaUtils.getBooleanProperty(fc, CAN_CREATE_DIRECTORIES_PROPERTY);
        return b != null ? b : fc.getDialogType() == JFileChooser.SAVE_DIALOG;
    }

    private void doApproveButtonTextChanged(PropertyChangeEvent e) {
        approveButton.setText(getApproveButtonText(fc));
        approveButton.setToolTipText(getApproveButtonToolTipText(fc));
    }

    private void doDialogTypeChanged(PropertyChangeEvent e) {
        approveButton.setText(getApproveButtonText(fc));
        approveButton.setToolTipText(getApproveButtonToolTipText(fc));
        boolean isSave = isFileNameFieldVisible();
        fileNameTextField.setEnabled(isSave);
        savePanel.setVisible(isSave);
        updateApproveButtonState();
        buttonsPanel.updateButtons();
        updateOptionsPanelVisibility();  // also updates buttons
        reconfigureChooser();  // some display properties are different for open and save dialogs
        configureDialog();     // dialog window properties are different for open and save dialogs
    }

    private void doApproveButtonMnemonicChanged(PropertyChangeEvent e) {
        // Note: Metal does not use mnemonics for approve and cancel
    }

    private void doControlButtonsChanged(PropertyChangeEvent e) {
        if (fc.getControlButtonsAreShown()) {
            buttonsPanel.showControlButtons();
        } else {
            buttonsPanel.hideControlButtons();
        }
    }

    private void doFileSystemViewChanged(PropertyChangeEvent e) {
        boolean isInstalled = model == fileSystemModel;
        fileSystemModel = new FileSystemTreeModel(fc);
        sidebarTreeModel = new SidebarTreeModel(fc, new TreePath(fileSystemModel.getRoot()), fileSystemModel);
        sidebarTree.setModel(sidebarTreeModel);

        if (isInstalled) {
            installModel(fileSystemModel);
        }
    }

    private boolean shouldDisplayOptionsButton() {
        return isOptionsButtonAvailable
                && fc.getDialogType() == JFileChooser.OPEN_DIALOG
                && (accessoryPanel.isVisible() || formatPanel.isVisible());
    }

    private void updateButton(JButton b) {
        String buttonType = null;
        if (fc.getDialogType() == JFileChooser.OPEN_DIALOG && OSVersion >= 1011 && OSVersion <= 1012) {
            buttonType = "textured";
        }
        b.putClientProperty(BUTTON_TYPE, buttonType);
    }

    private void ensureFileSystemModel() {
        installModel(fileSystemModel);
    }

    private void installModel(FileSystemTreeModel m) {
        if (m != model) {
            model = m;
            subtreeModel = new SubtreeTreeModel(m);
            columnView.setModel(subtreeModel);
            if (listView != null) {
                listView.setModel(subtreeModel);
            }
        }
    }

    private void doPreviewComponentChanged(PropertyChangeEvent e) {
        reconfigureView();
    }

    /*
     * Listen for filechooser property changes, such as
     * the selected dir changing, or the type of the dialog changing.
     */

    @Override
    public PropertyChangeListener createPropertyChangeListener(JFileChooser fc) {
        return new FileChooserPropertyChangeListener();
    }

    protected class FileChooserPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent e) {
            if (isAdjusting > 0) {
                return;
            }

            String s = e.getPropertyName();
            if (s.equals(JFileChooser.SELECTED_FILE_CHANGED_PROPERTY)) {
                doSelectedFileChanged(e);
            } else if (s.equals(JFileChooser.SELECTED_FILES_CHANGED_PROPERTY)) {
                doSelectedFilesChanged(e);
            } else if (s.equals(JFileChooser.DIRECTORY_CHANGED_PROPERTY)) {
                doDirectoryChanged(e);
            } else if (s.equals(JFileChooser.FILE_FILTER_CHANGED_PROPERTY)) {
                doFilterChanged(e);
            } else if (s.equals(JFileChooser.FILE_SYSTEM_VIEW_CHANGED_PROPERTY)) {
                doFileSystemViewChanged(e);
            } else if (s.equals(JFileChooser.FILE_SELECTION_MODE_CHANGED_PROPERTY)) {
                doFileSelectionModeChanged(e);
            } else if (s.equals(JFileChooser.MULTI_SELECTION_ENABLED_CHANGED_PROPERTY)) {
                doMultiSelectionChanged(e);
            } else if (s.equals(JFileChooser.ACCESSORY_CHANGED_PROPERTY)) {
                doAccessoryChanged(e);
            } else if (s.equals(JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY)) {
                doChoosableFilterChanged(e);
            } else if (s.equals(JFileChooser.APPROVE_BUTTON_TEXT_CHANGED_PROPERTY)
                    || s.equals(JFileChooser.APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY)) {
                doApproveButtonTextChanged(e);
            } else if (s.equals(JFileChooser.DIALOG_TYPE_CHANGED_PROPERTY)) {
                doDialogTypeChanged(e);
            } else if (s.equals(JFileChooser.APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY)) {
                doApproveButtonMnemonicChanged(e);
            } else if (s.equals(JFileChooser.CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY)) {
                doControlButtonsChanged(e);
            } else if (s.equals(JFileChooser.FILE_VIEW_CHANGED_PROPERTY)) {
                doFileViewChanged(e);
            } else if (s.equals(JFileChooser.FILE_HIDING_CHANGED_PROPERTY)) {
                doFileHidingChanged(e);
            } else if (s.equals(PACKAGE_TRAVERSABLE_PROPERTY)) {
                doPackageTraversableChanged(e);
            } else if (s.equals(APPLICATION_TRAVERSABLE_PROPERTY)) {
                doApplicationTraversableChanged(e);
            } else if (s.equals("Aqua.FileChooser.preview")) {
                doPreviewComponentChanged(e);
            } else if (s.equals(OPTIONS_PANEL_ENABLED_PROPERTY)) {
                doOptionsPanelEnabledChanged();
            } else if (s.equals(CAN_CREATE_DIRECTORIES_PROPERTY)) {
                doCanCreateDirectoriesChanged(e);
            }
        }
    }

    private void updateControlButtonVisibility() {
        if (fc.getControlButtonsAreShown()) {
            buttonsPanel.showControlButtons();
        } else {
            buttonsPanel.hideControlButtons();
        }
    }

    protected boolean shouldDisplayNewFolderButton() {
        return getCanCreateDirectoriesProperty();
    }

    @Override
    public String getFileName() {
        if (fileNameTextField != null) {
            return fileNameTextField.getText();
        } else {
            return null;
        }
    }

    @Override
    public void setFileName(String filename) {
        setFileNameTextField(filename);
    }

    private void selectRootFromSidebarSelection() {
        TreePath sidebarPath = sidebarTree.getSelectionPath();
        if (sidebarPath != null) {
            Object o = sidebarPath.getLastPathComponent();
            if (o instanceof SidebarTreeNode) {
                SidebarTreeNode info = (SidebarTreeNode) o;
                File file = info.getResolvedFile();

                while (file != null && !file.isDirectory() && !fc.isTraversable(file)) {
                    file = file.getParentFile();
                }

                if (file != null) {
                    selectDirectory(file, SELECT_DIRECTORY_FROM_SIDEBAR);
                }
            }
        }
    }

    /*
      Sources for selecting a directory by means other than the user changing the selection in the view.
    */

    protected final int SELECT_DIRECTORY_FROM_API = 1;          // the application changed the file chooser current directory
    protected final int SELECT_DIRECTORY_FROM_COMBO_BOX = 2;    // the user selected a directory using the combo box
    protected final int SELECT_DIRECTORY_BY_DOUBLE_CLICK = 3;   // the user double clicked a directory in the view
    protected final int SELECT_DIRECTORY_BY_KEYSTROKE = 4;      // the user used a keyboard shortcut
    protected final int SELECT_DIRECTORY_FROM_SIDEBAR = 5;      // the user selected an item in the sidebar
    protected final int SELECT_DIRECTORY_TO_INITIALIZE = 6;     // the file chooser UI is being initialized
    protected final int SELECT_DIRECTORY_NEW_VIEW = 7;          // the view has been changed

    /**
     * Select the specified directory. Update the current view to display the directory. A new view root may be chosen and the sidebar
     * selection may be changed. The file chooser current directory will be changed if needed and the combo box model
     * updated. The view selection will be updated as needed. The approve button enabled state is updated.
     *
     * This method may perform some updates asynchronously.
     *
     * @param f The traversable file to become the current directory. If {@code f} is not a traversable file, the
     *          nearest traversable ancestor is used (should be the parent).
     * @param source The source of this change. (See constants above.)
     */
    protected void selectDirectory(File f, int source) {
        selectDirectory(f, source, true, null);
    }

    /**
     * Update the current view to display the specified directory. A new view root may be chosen and the sidebar
     * selection may be changed. The file chooser current directory will be changed if needed and the combo box model
     * updated. The view selection will be updated as needed. The approve button enabled state is updated.
     *
     * This method may perform some updates asynchronously.
     *
     * @param f The traversable file to become the current directory. If {@code f} is not a traversable file, the
     *          nearest traversable ancestor is used (should be the parent).
     * @param source The source of this change. (See constants above.)
     * @param r This runnable is invoked after all updates have been completed.
     */
    protected void selectDirectory(File f, int source, boolean updateSelection, Runnable r) {

        /*
          Smart folders are displayed when they are selected using the sidebar. If a smart folder is already displayed
          in a column view and a directory in the smart folder tree is selected by means other than the sidebar, then
          the directory is displayed as part of the smart folder. In all other cases, the selected directory is
          displayed in a file system tree.
        */

        if ((source == SELECT_DIRECTORY_FROM_SIDEBAR || source == SELECT_DIRECTORY_TO_INITIALIZE || source == SELECT_DIRECTORY_NEW_VIEW)
                && OSXFile.isSavedSearch(f)) {
            installModel(getSavedSearchTreeModel(f));
        } else {
            f = toTraversableFile(f);
            if (viewMode != ViewModeControl.COLUMN_VIEW
                    || source == SELECT_DIRECTORY_FROM_SIDEBAR || model.toPath(f, null) == null) {
                ensureFileSystemModel();
            }
        }

        /*
          If the selection was made using the sidebar, the sidebar item becomes the view root.

          Otherwise, if not displaying an element of a smart folder:

          If displaying a list view, the specified directory becomes the view root and directory is selected in the
          sidebar if it is present (otherwise the sidebar selection is cleared).

          If displaying a column view, a new view root is determined by searching for the best matching sidebar item and
          that sidebar item (if found) is selected.
        */

        TreePath fullPath = model.toPath(f, null);

        if (source == SELECT_DIRECTORY_FROM_SIDEBAR) {
            setViewRoot(fullPath);
            finishDisplayDirectory(source, fullPath, updateSelection, r);
        } else if (!(model instanceof SavedSearchFileSystemTreeModel)) {
            File theFile = f;
            sidebarTreeModel.invokeWhenValid(new Runnable() {
                public void run() {
                    TreePath sidebarPath = selectViewRoot(theFile, viewMode == ViewModeControl.COLUMN_VIEW);
                    finishDisplayDirectory(source, fullPath, updateSelection, r);
                    /*
                      Seems to be a race condition causing AquaTreeUI to get confused about which row is selected
                      when new items have been added to the sidebar tree model but not yet displayed.
                    */
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            setSidebarSelection(sidebarPath);
                        }
                    });
                }
            });
        }
    }

    /**
     * Complete the display of a specified directory. This method is called after any update to the view root.
     */
    private void finishDisplayDirectory(int source, TreePath fullPath, boolean updateSelection, Runnable r) {
        if (fullPath != null) {
            model.lazyInvalidatePath(fullPath);
        } else {
            model.invalidateAll();
        }

        TreePath viewPath = fullPath != null ? subtreeModel.toSubPath(fullPath) : null;
        if (viewPath == null) {
            viewPath = subtreeModel.getPathToRoot();
        }

        if (updateSelection) {
            setViewSelection(viewPath);
        }

        if (viewMode == ViewModeControl.LIST_VIEW) {
            viewPath = subtreeModel.getPathToRoot();
        }

        if (source != SELECT_DIRECTORY_FROM_API && source != SELECT_DIRECTORY_TO_INITIALIZE && source != SELECT_DIRECTORY_NEW_VIEW) {
            updateCurrentDirectory(viewPath);
        }

        updateComboBoxModel(viewPath);

        if (r != null) {
            r.run();
        }
    }

    private File toTraversableFile(File f) {
        while (f != null && !fc.isTraversable(f)) {
            f = f.getParentFile();
        }
        return f != null ? f : new File(System.getProperty("user.home"));
    }

    /**
     * Select a subtree of the model to display that contains the specified file and identify the matching sidebar item,
     * if any.
     *
     * @param f The (resolved) file.
     * @param isRestrictedToSidebar If true, new subtree root will be the best matching sidebar item, where a sidebar
     *  item matches if the file is contained in the subtree defined by that sidebar item. (If there is no matching
     *  sidebar item, the entire file system tree model is displayed.) If false, the specified file becomes the new
     *  subtree root, and the sidebar item for that (exact) file is returned (if any).
     *
     * @return a path identifying the matching sidebar item, or null if none.
     */
    protected TreePath selectViewRoot(File f, boolean isRestrictedToSidebar) {

        /*
          Special case: /Network displays as a root but has no sidebar item.
        */

        if (f.getPath().equals("/Network")) {
            setViewRoot(fileSystemModel.toPath(f, null));
            return null;
        }

        if (!isRestrictedToSidebar) {
            TreePath sidebarPath = selectRootOneStep(f);
            if (sidebarPath == null) {
                TreePath path = model.toPath(f, null);
                setViewRoot(path);
            }
            return sidebarPath;
        }

        FileSystemTreeModel.Node root = fileSystemModel.getRoot();

        while (f != null) {
            TreePath sidebarPath = selectRootOneStep(f);
            if (sidebarPath != null) {
                return sidebarPath;
            }

            if (f.equals(root.getFile())) {
                break;
            }

            f = f.getParentFile();
        }

        setViewRoot(new TreePath(model.getRoot()));
        return null;
    }

    private TreePath selectRootOneStep(File target) {
        if (target == null) {
            return null;
        }

        TreePath sidebarPath = searchSidebarTreeForFile((TreeNode) sidebarTreeModel.getRoot(), target);
        if (sidebarPath != null) {
            TreePath p = fileSystemModel.toPath(target, null);
            setViewRoot(p);
            return sidebarPath;
        }

        return null;
    }

    private TreePath searchSidebarTreeForFile(TreeNode node, File f) {

        if (node instanceof SidebarTreeNode) {
            SidebarTreeNode info = (SidebarTreeNode) node;
            File nf = info.getResolvedFile();
            if (nf != null) {
                File rf = OSXFile.resolve(f);
                File nrf = OSXFile.resolve(nf);
                if (rf != null && rf.equals(nrf)) {
                    return new TreePath(sidebarTreeModel.getPathToRoot(node));
                }
            }
        }

        if (node != null) {
            int count = sidebarTreeModel.getChildCount(node);
            for (int index = 0; index < count; index++) {
                Object child = sidebarTreeModel.getChild(node, index);
                if (child instanceof TreeNode) {
                    TreePath path = searchSidebarTreeForFile((TreeNode) child, f);
                    if (path != null) {
                        return path;
                    }
                }
            }
        }
        return null;
    }

    /**
     * Update the display subtree root. This change affects all views.
     */
    protected void setViewRoot(TreePath path) {
        ++isAdjusting;
        subtreeModel.setPathToRoot(path);
        --isAdjusting;
    }

    /**
     * Update the view selection.
     */
    protected void setViewSelection(TreePath path) {
        setViewSelection(Collections.singletonList(path));
    }

    /**
     * Update the view selection.
     */
    protected void setViewSelection(java.util.List<TreePath> paths) {
        if (activeView != null) {
            ++isAdjusting;
            activeView.setSelection(paths);
            --isAdjusting;
            if (!paths.isEmpty()) {
                ensurePathIsVisible(paths.get(0));
            }
        }
    }

    /**
     * Update the sidebar selection.
     */
    protected void setSidebarSelection(TreePath path) {
        ++isAdjusting;
        if (path != null) {
            sidebarTree.setSelectionPath(path);
            if (sidebarTree.isVisible(path)) {
                sidebarTree.scrollPathToVisible(path);
            }
        } else {
            sidebarTree.clearSelection();
        }
        --isAdjusting;
    }

    /**
     * Update the contents of the filename text field.
     */
    protected void setFileNameTextField(String text) {
        ++isAdjusting;
        if (fileNameTextField != null && (text == null || !fileNameTextField.getText().equals(text))) {
            fileNameTextField.setText(text);
        }
        --isAdjusting;
    }

    /**
     * Update the combo box model based on the specified view selection.
     */
    protected void updateComboBoxModel(TreePath subtreePath) {

        /*
          The special case here is selecting a directory via a displayed smart folder (saved search). The items in the
          combo box should spell out the full file system path of the directory.
        */

        TreePath fullPath;
        if (model instanceof SavedSearchFileSystemTreeModel && subtreePath.getPathCount() > 1) {
            FileInfo info = (FileInfo) subtreePath.getLastPathComponent();
            File f = info.getResolvedFile();
            fullPath = fileSystemModel.toPath(f, null);
        } else {
            fullPath = subtreeModel.toFullPath(subtreePath);
        }

        ++isAdjusting;  // not really needed
        directoryComboBoxModel.setPath(fullPath);
        --isAdjusting;
    }

    private void ensurePathIsVisible(TreePath path) {

        /*
          Make sure the nodes are valid otherwise the scroll positions may be incorrect.
        */

        if (activeView != null && path != null) {
            Runnable r = () -> activeView.ensurePathIsVisible(path);
            runAfterValidation(path, 0, path.getPathCount() - 2, r);
        }
    }

    private void runAfterValidation(TreePath path, int index, int lastIndex, Runnable r) {
        if (index > lastIndex) {
            r.run();
        } else {
            FileSystemTreeModel.Node n = (FileSystemTreeModel.Node) path.getPathComponent(index);
            n.invokeWhenValid(() -> runAfterValidation(path, index+1, lastIndex, r));
        }
    }

    private DirectoryComboBoxRenderer createDirectoryComboBoxRenderer(JComboBox cb) {
        return new DirectoryComboBoxRenderer(cb);
    }

    private SidebarRenderer createSidebarCellRenderer(JFileChooser fc) {
        return new SidebarRenderer();
    }

    protected TreeSelectionListener createSidebarSelectionListener(JFileChooser fc) {
        return new SidebarSelectionListener();
    }

    /**
     * Renderer for DirectoryComboBox
     */
    static class DirectoryComboBoxRenderer extends AquaComboBoxRendererInternal {

        IndentIcon ii = new IndentIcon();
        private JSeparator separator = new JSeparator();

        public DirectoryComboBoxRenderer(JComboBox cb) {
            super(cb);
            separator.setPreferredSize(new Dimension(9, 9));
        }

        @Override
        public Insets getInsets(Insets insets) {
            Insets s = super.getInsets(insets);
            if (fInList) {
                s.top++;
                s.bottom++;
            }
            return s;
        }

        @Override
        public Component getListCellRendererComponent(JList list, Object value,
                                                      int index, boolean isSelected,
                                                      boolean cellHasFocus) {

            // String objects are used to denote delimiters.
            if (value instanceof String) {
                super.getListCellRendererComponent(list, value, index, false, cellHasFocus);
                setText((String) value);
                setPreferredSize(new Dimension(10, 14));
                return this;
            }
            super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            setPreferredSize(null);
            if (value instanceof File) {
                setText(value + " " + index);
                return this;
            }
            FileSystemTreeModel.Node node = (FileSystemTreeModel.Node) value;
            if (node == null) {
                return separator;
                /*
                File root = new File("/");
                setText(fc.getName(root));
                ii.icon = fc.getIcon(root);
                 */
            } else {
                setText(node.getUserName());
                ii.icon = node.getIcon();
            }
            ii.depth = 0;
            setIcon(ii);
            return this;
        }
    }

    /**
     * Selection model for the sidebar. Prevents category headings from being selected.
     */
    private static class SidebarTreeSelectionModel extends DefaultTreeSelectionModel {
        public SidebarTreeSelectionModel() {
            setSelectionMode(SINGLE_TREE_SELECTION);
        }

        @Override
        public void setSelectionPaths(TreePath[] paths) {
            if (paths != null && paths.length > 0) {
                TreePath path = paths[0];
                Object pc = path.getLastPathComponent();
                if (!(pc instanceof SidebarTreeNode)) {
                    return;
                }
            }
            super.setSelectionPaths(paths);
        }
    }

    private static class SidebarRenderer extends DefaultTreeCellRenderer {

        public SidebarRenderer() {
        }

        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                      boolean isSelected, boolean isExpanded, boolean isLeaf,
                                                      int row, boolean cellHasFocus) {
            super.getTreeCellRendererComponent(tree, value, isSelected,
                    isExpanded, isLeaf, row, false);

            if (value instanceof SidebarTreeNode) {
                SidebarTreeNode info = (SidebarTreeNode) value;
                setText(info.getUserName());
                AquaAppearance appearance = AppearanceManager.ensureAppearance(tree);
                setIcon(info.getIcon(appearance));
            }
            return this;
        }
    }

    public static class SidebarTreeUI extends AquaTreeUI {

        @Override
        protected void installKeyboardActions() {
            super.installKeyboardActions();

            // Avoid conflict with Cmd-Shift-A in the file chooser
            InputMap map = tree.getInputMap(JComponent.WHEN_FOCUSED).getParent();
            KeyStroke ks = KeyStroke.getKeyStroke("shift meta A");
            Object v = map.get(ks);
            if (v != null && v.equals("clearSelection")) {
                InputMap newMap = new InputMap();
                newMap.setParent(map);
                newMap.put(ks, "selectApplicationsFolder"); // dummy name for now
                SwingUtilities.replaceUIInputMap(tree, JComponent.WHEN_FOCUSED, newMap);
            }
        }
    }

    final static int space = 10;

    private static class IndentIcon implements Icon {

        Icon icon = null;
        int depth = 0;

        @Override
        public void paintIcon(Component c, Graphics g, int x, int y) {
            if (icon != null) {
                if (c.getComponentOrientation().isLeftToRight()) {
                    icon.paintIcon(c, g, x + depth * space, y);
                } else {
                    icon.paintIcon(c, g, x, y);
                }
            }
        }

        @Override
        public int getIconWidth() {
            return (icon == null) ? depth * space : icon.getIconWidth() + depth * space;
        }

        @Override
        public int getIconHeight() {
            return (icon == null) ? 0 : icon.getIconHeight();
        }
    }

    //
// DataModel for DirectoryComboxbox
//
    protected DirectoryComboBoxModel createDirectoryComboBoxModel(JFileChooser fc) {
        return new DirectoryComboBoxModel();
    }

    /**
     * Data model for a directory selection combo-box.
     * There is always one node in the tree model: the dir system root (aka
     * the computer).
     */
    protected class DirectoryComboBoxModel extends AbstractListModel
            implements ComboBoxModel {

        TreePath path;
        FileSystemTreeModel.Node selectedDirectory = null;

        public DirectoryComboBoxModel() {
        }

        /**
         * Sets the path of the directory combo box.
         * TreePath<FileSystemTreeModel.Node>
         */
        private void setPath(TreePath path) {
            if (this.path != null && this.path.getPathCount() > 0) {
                fireIntervalRemoved(this, 0, this.path.getPathCount() - 1);
            }
            this.path = path;
            if (this.path.getPathCount() > 0) {
                fireIntervalAdded(this, 0, this.path.getPathCount() - 1);
            }
            setSelectedItem(this.path.getLastPathComponent());
        }

        @Override
        public void setSelectedItem(Object selectedItem) {
            FileSystemTreeModel.Node node = (FileSystemTreeModel.Node) selectedItem;
            this.selectedDirectory = node;
            fireContentsChanged(this, -1, -1);
        }

        @Override
        public Object getSelectedItem() {
            return selectedDirectory;
        }

        public TreePath getSelectedPath() {
            int count = path.getPathCount();
            for (int i = 0; i < count; i++) {
                Object node = path.getPathComponent(i);
                if (node.equals(selectedDirectory)) {
                    return pathPrefix(path, i+1);
                }
            }
            return null;
        }

        private TreePath pathPrefix(TreePath base, int count) {
            Object[] nodes = new Object[count];
            for (int i = 0; i < count; i++) {
                nodes[i] = base.getPathComponent(i);
            }
            return new TreePath(nodes);
        }

        @Override
        public int getSize() {
            return (path == null) ? 0 : path.getPathCount();
        }

        @Override
        public Object getElementAt(int index) {
            return path.getPathComponent(path.getPathCount() - index - 1);
        }
    }

    //
// Renderer for Types ComboBox
//
    protected FilterComboBoxRenderer createFilterComboBoxRenderer(JComboBox cb) {
        return new FilterComboBoxRenderer(cb);
    }

    /**
     * Render different type sizes and styles.
     */
    protected static class FilterComboBoxRenderer extends AquaComboBoxRendererInternal {

        public FilterComboBoxRenderer(JComboBox cb) {
            super(cb);
        }

        @Override
        public Component getListCellRendererComponent(JList list,
                                                      Object value, int index, boolean isSelected,
                                                      boolean cellHasFocus) {

            if (value != null && value instanceof FileFilter) {
                value = ((FileFilter) value).getDescription();
            }

            super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

            return this;
        }
    }

    //
// DataModel for Types ComboBox
//
    protected FilterComboBoxModel createFilterComboBoxModel() {
        return new FilterComboBoxModel();
    }

    /**
     * Data model for a type-face selection combo-box.
     */
    protected class FilterComboBoxModel
            extends AbstractListModel
            implements ComboBoxModel, PropertyChangeListener {

        protected FileFilter[] filters;

        protected FilterComboBoxModel() {
            super();
            filters = fc.getChoosableFileFilters();
        }

        @Override
        public void propertyChange(PropertyChangeEvent e) {
            String prop = e.getPropertyName();
            if (prop == JFileChooser.CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY) {
                filters = (FileFilter[]) e.getNewValue();
                fireContentsChanged(this, -1, -1);
            } else if (prop == JFileChooser.FILE_FILTER_CHANGED_PROPERTY) {
                fireContentsChanged(this, -1, -1);
            }
        }

        @Override
        public void setSelectedItem(Object filter) {
            if (filter != null) {
                // Here we want to respond to a change event
                fc.setFileFilter((FileFilter) filter);
                // Don't clear the filename field, when the user changes
                // the filename filter.
                // FIXME - Maybe we should disable the save
                // button when the name is not matched by the filter?
                //setFileName(null);
                fireContentsChanged(this, -1, -1);
            }
        }

        @Override
        public Object getSelectedItem() {
            // Ensure that the current filter is in the list.
            // NOTE: we shouldn't have to do this, since JFileChooser adds
            // the filter to the choosable filters list when the filter
            // is set. Lets be paranoid just in case someone overrides
            // setFileFilter in JFileChooser.

            /*
              The above statement is widely copied but it is incorrect. There is no code in JFileChooser that adds a
              filter to the list of choosable filters when it is set. Perhaps there was at one time? The effect of this
              code is to make the notion of a non-choosable filter difficult to realize. The problem being solved is a
              real one: if the file chooser is displaying a set of choosable filters and the application (unwisely) sets
              a filter that is not on the list, the combo box should display something other than one of the choosable
              filters. A better solution would add an item to the combo box model rather than to the file chooser's list
              of choosable filters. At this point, however, the broken behavior is probably depended upon by lots of
              code, so I guess it is too late to change except as a configurable option.
            */

            // If no list of choosable filters is currently displayable, then we do not want to start displaying one.

            FileFilter[] filters = fc.getChoosableFileFilters();
            if (filters.length > 1) {
                FileFilter currentFilter = fc.getFileFilter();
                boolean found = false;
                if (currentFilter != null) {
                    for (int i = 0; i < filters.length; i++) {
                        if (filters[i] == currentFilter) {
                            found = true;
                        }
                    }
                    if (!found) {
                        // Here we want to respond to a change event
                        fc.addChoosableFileFilter(currentFilter);
                    }
                }
            }

            return fc.getFileFilter();
        }

        @Override
        public int getSize() {
            if (filters != null) {
                return filters.length;
            } else {
                return 0;
            }
        }

        @Override
        public Object getElementAt(int index) {
            if (index > getSize() - 1) {
                // This shouldn't happen. Try to recover gracefully.
                return fc.getFileFilter();
            }
            if (filters != null) {
                return filters[index];
            } else {
                return null;
            }
        }
    }

    protected class OptionsAction extends AbstractAction {
        @Override
        public void actionPerformed(ActionEvent e) {
            setOptionsPanelEnabledProperty(!isOptionsEnabled);
            doOptionsPanelEnabledChanged();
        }
    }

    /**
     * Acts when DirectoryComboBox has changed the selected item.
     */
    protected class DirectoryComboBoxAction extends AbstractAction {

        protected DirectoryComboBoxAction() {
            super("DirectoryComboBoxAction");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (isAdjusting != 0) {
                return;
            }

            /*
              The reason for using invokeLater: we want to transfer focus to the proper column list. However, when the
              pop up is dismissed (which has not happened yet), it will transfer the focus to the previous focus owner.
              Our request must come after that.
            */

            TreePath path = directoryComboBoxModel.getSelectedPath();
            if (path != null) {
                FileInfo info = (FileInfo) path.getLastPathComponent();
                File dir = info.getResolvedFile();
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        selectDirectory(dir, SELECT_DIRECTORY_FROM_COMBO_BOX);
                    }
                });
            }
        }
    }

    @Override
    protected JButton getApproveButton(JFileChooser fc) {
        return approveButton;
    }

    @Override
    public Action getApproveSelectionAction() {
        return approveSelectionAction;
    }

    /**
     * This method is called when the user double clicks an item in the view or clicks at the approve button.
     */
    private void maybeApproveSelection() {

        assert isAdjusting == 0;

        /*
          In general, the file chooser selection should already match the view selection and should be legitimate. The
          primary exception is where the file chooser allows the user to provide a file name (e.g. a Save dialog), in
          which case we need to update the file chooser selection prior to approving the selection. There are also
          obscure exceptional cases only some of which are checked for here.
        */

        String filename = null;
        if (isFileNameFieldVisible()) {
            filename = getFileName();
            if (filename.equals("")) {
                filename = null;
            }
        }

        java.util.List<File> selectedFiles = getUISelection(true);
        if (filename != null && selectedFiles.size() == 1) {
            File selectedFile = selectedFiles.get(0);

            /*
              In a list view, it is possible to select a directory in the list without making it the current directory
              (the one whose name is in the combo box). It might be confusing to the user to enable the Save button in
              that situation. NSSavePanel enables the Save button, but the action switches to that directory rather than
              accepting the selection.
            */

            if (isDirectorySelectedInSaveDialogListView(selectedFile)) {
                selectDirectory(selectedFile, SELECT_DIRECTORY_BY_DOUBLE_CLICK);
                return;
            }

            File parent = selectedFile.isDirectory() ? selectedFile : selectedFile.getParentFile();
            File f = new File(parent, filename);
            selectedFiles = new ArrayList<>();
            selectedFiles.add(f);
        }

        if (selectedFiles.isEmpty()) {
            return;
        }

        /*
          Transfer the view selection to the file chooser.
        */

        if (isMultipleSelection()) {
            updateSelectedFiles(selectedFiles);
        } else if (selectedFiles.size() == 1) {
            updateSelectedFile(selectedFiles.get(0));
        } else {
            return;
        }

        fc.approveSelection();
    }

    /**
     * Is a directory selected as an item in the list view? This method is called only for a Save or Custom dialog.
     * @param f The uniquely selected file.
     */
    private boolean isDirectorySelectedInSaveDialogListView(File f) {
        if (viewMode == ViewModeControl.LIST_VIEW && fc.isTraversable(f)) {
            File dir = fc.getCurrentDirectory();
            return !f.equals(dir);
        }
        return false;
    }

    /**
     * Return the selection from the active view normalized to be consistent with the file chooser configuration. If the
     * actual view selection is not consistent with the file chooser configuration, the view selection will be updated
     * to make it consistent. The returned paths are guaranteed to have an associated file.
     */
    protected java.util.List<TreePath> getNormalizedUISelection() {

        /*
          Multiple paths are valid only if the chooser is configured for multiple selection. An unacceptable traversable
          file can be selected, but it must be the unique selection.
        */

        java.util.List<TreePath> paths = activeView.getSelection();
        if (!paths.isEmpty()) {
            java.util.List<TreePath> acceptablePaths = new ArrayList<>();
            java.util.List<TreePath> traversablePaths = new ArrayList<>();    // traversable but not acceptable
            boolean isChanged = false;
            for (TreePath path : paths) {
                FileInfo info = (FileInfo) path.getLastPathComponent();
                if (info.getFile() == null) {
                    isChanged = true;
                } else if (info.isAcceptable()) {
                    acceptablePaths.add(path);
                } else if (info.isTraversable()) {
                    traversablePaths.add(path);
                } else {
                    isChanged = true;
                }
            }

            if (!traversablePaths.isEmpty()) {
                if (acceptablePaths.isEmpty()) {
                    // allow one traversable item
                    if (traversablePaths.size() > 1) {
                        traversablePaths = traversablePaths.subList(0, 1);
                        isChanged = true;
                    }
                } else {
                    // allow no traversable items
                    traversablePaths = new ArrayList<>();
                    isChanged = true;
                }
            }

            java.util.List<TreePath> result = new ArrayList<>();
            result.addAll(acceptablePaths);
            result.addAll(traversablePaths);
            if (result.size() > 1 && !isMultipleSelection()) {
                result = result.subList(0, 1);
                isChanged = true;
            }

            if (isChanged) {
                ++isAdjusting;
                activeView.setSelection(result);
                --isAdjusting;
            }

            return result;

        } else {
            return paths;
        }
    }

    /**
     * Return the files selected in the current view.
     * @param useDefault If true and no files are selected in the view, return the display subtree root.
     * @return the selected files.
     */
    protected java.util.List<File> getUISelection(boolean useDefault) {
        java.util.List<File> result = new ArrayList<>();

        java.util.List<TreePath> paths = activeView.getSelection();
        for (TreePath path : paths) {
            File f = ((FileSystemTreeModel.Node)path.getLastPathComponent()).getFile();
            result.add(f);
        }

        if (useDefault && result.isEmpty()) {
            FileInfo info = (FileInfo) subtreeModel.getRoot();
            File f = info.getFile();
            result.add(f);
        }

        return result;
    }

    /**
     * Return the file selection from the file chooser normalized to be consistent with the file chooser configuration.
     * If the actual selection is not consistent with the file chooser configuration, the selection will be updated to
     * make it consistent. The returned files are guaranteed to be absolute.
     */
    protected java.util.List<File> getNormalizedChooserSelection() {

        /*
          Multiple files are valid only if the chooser is configured for multiple selection. An unacceptable traversable
          file can be selected, but it must be the unique selection.
        */

        java.util.List<File> files = getChooserSelection();

        if (!files.isEmpty()) {
            java.util.List<File> acceptableFiles = new ArrayList<>();
            java.util.List<File> traversableFiles = new ArrayList<>();    // traversable but not acceptable
            boolean isChanged = false;
            for (File file : files) {
                if (isAcceptable(file)) {
                    acceptableFiles.add(file);
                } else if (fc.isTraversable(file)) {
                    traversableFiles.add(file);
                } else {
                    isChanged = true;
                }
            }

            if (!traversableFiles.isEmpty()) {
                if (acceptableFiles.isEmpty()) {
                    // allow one traversable file
                    if (traversableFiles.size() > 1) {
                        traversableFiles = traversableFiles.subList(0, 1);
                        isChanged = true;
                    }
                } else {
                    // allow no traversable files
                    traversableFiles = new ArrayList<>();
                    isChanged = true;
                }
            }

            java.util.List<File> result = new ArrayList<>();
            result.addAll(acceptableFiles);
            result.addAll(traversableFiles);
            if (result.size() > 1 && !isMultipleSelection()) {
                result = result.subList(0, 1);
                isChanged = true;
            }

            if (isChanged) {
                ++isAdjusting;
                fc.setSelectedFiles(result.toArray(new File[result.size()]));
                --isAdjusting;
            }

            return result;

        } else {
            return files;
        }
    }

    /**
     * Return the file selection from the file chooser.
     * The returned files are guaranteed to be absolute.
     */
    private java.util.List<File> getChooserSelection() {
        java.util.List<File> files = new ArrayList<>();
        File dir = fc.getCurrentDirectory();

        if (isMultipleSelection()) {
            File[] fs = fc.getSelectedFiles();
            for (File f : fs) {
                if (f != null) {
                    if (!f.isAbsolute()) {
                        f = new File(dir, f.getPath());
                    }
                    files.add(f);
                }
            }
        } else {
            File f = fc.getSelectedFile();
            if (f != null) {
                if (!f.isAbsolute()) {
                    f = new File(dir, f.getPath());
                }
                files.add(f);
            }
        }
        return files;
    }

    // *****************************
// ***** Directory Actions *****
// *****************************
    @Override
    public Action getNewFolderAction() {
        return newFolderAction;
    }

    /**
     * Creates a new folder.
     */
    protected class NewFolderAction extends AbstractAction {

        protected NewFolderAction() {
            super("New Folder");
        }

        private String showNewFolderDialog() {
            JOptionPane optionPane = new JOptionPane(
                    newFolderDialogPrompt,
                    JOptionPane.PLAIN_MESSAGE,
                    JOptionPane.OK_CANCEL_OPTION);
            // Setup Input
            optionPane.setWantsInput(true);
            optionPane.putClientProperty(AquaOptionPaneUI.TEXT_FIELD_DOCUMENT_KEY, new FilenameDocument());
            optionPane.setInitialSelectionValue(newFolderDefaultName);

            // Setup Options
            optionPane.setOptions(new Object[]{
                    UIManager.getString("FileChooser.createFolderButtonText"),
                    UIManager.getString("FileChooser.cancelButtonText")
            });
            optionPane.setInitialValue(UIManager.getString("FileChooser.createFolderButtonText"));

            // Show the dialog
            JDialog dialog = optionPane.createDialog(fc, newFolderTitleText);
            dialog.setVisible(true);
            dialog.dispose();

            return (optionPane.getValue() == UIManager.getString("FileChooser.createFolderButtonText"))
                    ? (String) optionPane.getInputValue() : null;
        }

        @Override
        public void actionPerformed(ActionEvent actionevent) {
            String newFolderName = showNewFolderDialog();

            if (newFolderName != null) {

                File newFolder;
                TreePath selection = first(activeView.getSelection());
                FileSystemTreeModel.Node node = (FileSystemTreeModel.Node) selection.getLastPathComponent();
                File currentFile = node.getResolvedFile();
                if (node.isLeaf()) {
                    currentFile = currentFile.getParentFile();
                }
                newFolder = new File(currentFile, newFolderName);
                if (newFolder.exists()) {
                    JOptionPane.showMessageDialog(
                            fc,
                            newFolderExistsErrorText,
                            newFolderTitleText, JOptionPane.ERROR_MESSAGE);
                    return;
                }

                try {
                    if (!newFolder.mkdir()) {
                        if (!newFolder.isDirectory()) {
                            throw new IOException("Couldn't create folder \"" + newFolder.getName() + "\".");
                        }
                    }
                    fc.rescanCurrentDirectory();
                    selectDirectory(newFolder, SELECT_DIRECTORY_BY_KEYSTROKE);
                } catch (Exception e) {
                    JOptionPane.showMessageDialog(
                            fc,
                            newFolderErrorText,
                            newFolderTitleText, JOptionPane.ERROR_MESSAGE);
                }
            }
        }
    }

    protected class SaveTextFocusListener implements FocusListener {

        @Override
        public void focusGained(FocusEvent focusevent) {
            updateApproveButtonState();
        }

        @Override
        public void focusLost(FocusEvent focusevent) {
            /* empty */
        }
    }

    protected class SaveTextDocumentListener implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent documentevent) {
            textChanged();
        }

        @Override
        public void removeUpdate(DocumentEvent documentevent) {
            textChanged();
        }

        @Override
        public void changedUpdate(DocumentEvent documentevent) {
            //textChanged();
        }

        private void textChanged() {
            if (isAdjusting != 0) {
                return;
            }

            SwingUtilities.invokeLater(new Runnable() {

                @Override
                public void run() {
                    TreePath selection = first(activeView.getSelection());
                    FileSystemTreeModel.Node node = (FileSystemTreeModel.Node) selection.getLastPathComponent();
                    File file = node.getResolvedFile();
                    if (fileNameTextField.getText().length() != 0) {
                        if (!node.isLeaf()) {
                            // Don't change the current directory when the user is entering
                            // text into the text field. It confuses our users!
                            // Instead, we update the state of the approve button
                            // only, and then we return!
                            // dir = new File(dir, fileNameTextField.getText());
                            updateApproveButtonState();
                            return;
                        } else {
                            file = new File(fc.getFileSystemView().getParentDirectory(file), fileNameTextField.getText());
                        }
                    }

                    /*
                      I believe that updating the file chooser can happen only in a custom dialog,
                      because a save dialog does not allow selecting files and an open dialog does not display the file
                      name text field.
                    */

                    updateSelectedFile(file);
                    updateApproveButtonState();
                }
            });
        }
    }

    protected class FileChooserHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            Component c = e.getChanged();
            long flags = e.getChangeFlags();
            if ((flags & HierarchyEvent.PARENT_CHANGED) != 0) {
                if (c.getParent() != null) {
                    configureDialog();
                }
            }
            if ((flags & HierarchyEvent.SHOWING_CHANGED) != 0) {
                if (c.isShowing()) {
                    configureForShowing();
                } else {
                    configureForHiding();
                }
            }
        }
    }

    /**
     * Determine if this file chooser is contained in a dialog created by JFileChooser.
     */
    protected JDialog getStandardDialog() {
        Window w = SwingUtilities.getWindowAncestor(fc);
        if (w instanceof JDialog) {
            JDialog d = (JDialog) w;

            // Note: It would be nice to use the window decoration style, but there are issues. JFileChooser sets the
            // style after installing the file chooser in the dialog, so a parent change event cannot be used as our
            // hook. A displayability change could be used, but our configuration of the dialog makes it displayable, so
            // protection against recursion is needed. Finally, when window decoration styles are enabled, some windows
            // are initialized as undecorated, which we do not want.

            JRootPane rp = d.getRootPane();
            int style = rp.getWindowDecorationStyle();
            if (style == JRootPane.FILE_CHOOSER_DIALOG) {
                return d;
            }

            Container contentPane = d.getContentPane();
            int componentCount = contentPane.getComponentCount();
            if (componentCount == 1) {
                Component c = contentPane.getComponent(0);
                if (c == fc) {
                    return d;
                }
            }
        }
        return null;
    }

    protected void configureForShowing() {
        installSelectedView(true, true);

        if (fileNameTextField != null && isFileNameFieldVisible()) {
            fileNameTextField.selectAll();
            fileNameTextField.requestFocusInWindow();
        } else {
            activeView.requestFocusInWindow();
        }

        if (model != null) {
            model.setAutoValidate(UIManager.getBoolean("FileChooser.autovalidate"));
            TreePath subtreePath = first(activeView.getSelection());
            TreePath fullPath = subtreeModel.toFullPath(subtreePath);
            model.validatePath(fullPath);
            if (sidebarTreeModel != null) {
                sidebarTreeModel.lazyValidate();
            }
        }
        // We update the approve button state here, because the approve button can only be made the default button if
        // it has a root pane ancestor.
        updateApproveButtonState();
        if (fc.getSelectedFile() != null) {
            ensureFileIsVisible(fc, fc.getSelectedFile());
        }

        reconfigure(optionsPanel);
    }

    protected void configureForHiding() {
        if (model != null) {
            model.setAutoValidate(false);
            model.stopValidation();
            if (activeView != null) {
                TreePath subtreePath = first(activeView.getSelection());
                TreePath fullPath = subtreeModel.toFullPath(subtreePath);
                model.invalidatePath(fullPath);
            }
            clearIconCache();
        }
    }

    /**
     * Reconfigure the file chooser presentation based on a change to the dialog type or the system appearance.
     */
    protected void reconfigureChooser() {
        reconfigure(navigationPanel);
        reconfigure(buttonsPanel);
        reconfigure(topPanel);
        reconfigure(optionsPanel);
        reconfigureView();
    }

    protected void reconfigureView() {
        if (activeView != null) {
            activeView.reconfigure();
        }
    }

    private void reconfigure(@Nullable JComponent c) {
        if (c instanceof Reconfigurable) {
            Reconfigurable r = (Reconfigurable) c;
            r.reconfigure();
        }
    }

    private void updateWindowStyleParameters() {
        JDialog d = getStandardDialog();
        if (d != null) {
            JRootPane rp = d.getRootPane();
            if (windowStyle != null) {
                Integer topMargin = topPanel.getHeight();
                Integer bottomMargin = controlsPanel.getHeight();
                rp.putClientProperty(AQUA_WINDOW_TOP_MARGIN_KEY, topMargin);
                rp.putClientProperty(AQUA_WINDOW_BOTTOM_MARGIN_KEY, bottomMargin);
            } else {
                rp.putClientProperty(AQUA_WINDOW_STYLE_KEY, null);
                rp.putClientProperty(AQUA_WINDOW_TOP_MARGIN_KEY, null);
            }
        }
    }

    /**
     * Configure the file chooser and its window ancestor.
     */
    protected void configureDialog() {

        boolean isStandardDialog = false;
        useToolBar = false;
        windowStyle = null;

        JDialog d = getStandardDialog();
        if (d != null) {
            isStandardDialog = true;
            JRootPane rp = d.getRootPane();

            // Choose a window style for the dialog.
            // However, if the dialog is being displayed as a sheet, do not change the style.

            if (fc.getDialogType() == JFileChooser.OPEN_DIALOG) {
                windowStyle = "texturedToolBar";
            } else {
                windowStyle = "overlayTitleBar";
            }

            useToolBar = true;

            String existingStyle = AquaRootPaneUI.getWindowStyleKey(rp);
            if (!"undecorated".equals(existingStyle)) {
                rp.putClientProperty(AQUA_WINDOW_STYLE_KEY, windowStyle);
                rp.revalidate();
                rp.repaint();
            }
        }

        configureTopPanel();

        AquaAppearance appearance = AppearanceManager.ensureAppearance(fc);
        if (isStandardDialog) {
            splitPane.setBorder(null);
        } else {
            Color divider = appearance.getColor("separator");
            splitPane.setBorder(BorderFactory.createMatteBorder(1, 0, 1, 0, divider));
        }

        configureDialogSize();
    }

    /**
     * Determine and install the minimum size for the enclosing window, if the minimum window size has not been specified
     * by the application. If the new minimum size is larger than the current window size, resize the window to conform
     * to the new minimum size.
     */
    public void configureDialogSize() {
        Window w = SwingUtilities.getWindowAncestor(fc);
        if (w != null && isDefaultWindowSize(w)) {
            w.setMinimumSize(null);
            Dimension minimumSize = w.getMinimumSize();
            installMinimumSize(w, minimumSize.width, minimumSize.height);
            Dimension size = w.getSize();
            if (size.width < minimumSize.width || size.height < minimumSize.height) {
                size = new Dimension(Math.max(minimumSize.width, size.width), Math.max(minimumSize.height, size.height));
                w.setSize(size);
                try {
                    Point location = w.getLocationOnScreen();
                    int x = location.x;
                    int y = location.y;
                    Rectangle screenBounds = getScreenBounds(w);
                    if (x + size.width > screenBounds.x + screenBounds.width) {
                        x = Math.max(screenBounds.x, screenBounds.x + screenBounds.width - size.width);
                    }
                    if (y + size.height > screenBounds.y + screenBounds.height) {
                        y = Math.max(screenBounds.y, screenBounds.y + screenBounds.height - size.height);
                    }
                    w.setLocation(x, y);
                } catch (IllegalComponentStateException ex) {
                }
            }
        }
    }

    protected void installMinimumSize(@NotNull Window w, int width, int height) {
        Dimension size = new DimensionUIResource(width, height);
        w.setMinimumSize(size);
    }

    protected boolean isDefaultWindowSize(@NotNull Window w) {
        if (!w.isMinimumSizeSet()) {
            return true;
        }

        // Here, I would like to determine whether the set minimum size is an instance of DimensionUIResource.
        // Unfortunately, there is no direct way to access the value that was set.
        // Asking for the minimum size always returns a new Dimension.
        // However, there is a tricky way to get the original, which is to set a new value and capture the old value
        // in a property change listener.

        MinimumSizeTester tester = new MinimumSizeTester(w);
        return tester.isUIResource();
    }

    private static class MinimumSizeTester implements PropertyChangeListener {

        private Component c;
        private Dimension value;

        public MinimumSizeTester(@NotNull Component c) {
            this.c = c;
        }

        public boolean isUIResource() {
            // This will set the "minimumSizeSet" attribute, so it should be used only when this attribute is already
            // true.

            Dimension existingSize = c.getMinimumSize();
            c.addPropertyChangeListener(this);
            // the new size must be different than the old size to get a property change event
            c.setMinimumSize(new Dimension(existingSize.width + 1, existingSize.height));
            c.removePropertyChangeListener(this);
            c.setMinimumSize(existingSize);
            return value instanceof DimensionUIResource;
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            value = (Dimension) evt.getOldValue();
        }
    }

    protected @NotNull Rectangle getScreenBounds(@NotNull Window w) {
        Point loc = w.getLocationOnScreen();
        return AquaUtils.getScreenBounds(loc, w);
    }

// *******************************************************
// ************ FileChooserUI PLAF methods ***************
// *******************************************************

    /**
     * API method of FileChooserUI.
     */
    @Override
    public void ensureFileIsVisible(JFileChooser fc, File f) {

        /*
          This method is called by the file chooser when the selected file is set.
          It may also be called by the application in which case the file may not be selected.
          My reading is that the purpose of this method is to adjust scroll positions to reveal the
          file, not to change the file chooser selection or current directory.
        */

        if (activeView != null) {
            TreePath viewPath = getSubtreePath(f);
            if (viewPath != null) {
                ensurePathIsVisible(viewPath);
            }
        }

//
//
//
//        /*
//          The previous code makes a special case for a file that is part of the selection. Not sure why.
//        */
//
//
//
//        if (browser.getSelectionPaths() != null) {
//            TreePath[] paths = browser.getSelectionPaths();
//            for (int i = 0; i < paths.length; i++) {
//                if (((FileSystemTreeModel.Node) paths[i].getLastPathComponent()).getFile().equals(f)) {
//                    browser.ensurePathIsVisible(paths[i]);
//                    return;
//                }
//            }
//        } else {
//            TreePath fullPath = model.toPath(f, subtreeModel.getPathToRoot());
//            TreePath subPath = subtreeModel.toSubPath(fullPath);
//            if (subPath == null) {
//                ++isAdjusting;
//                selectRoot(f);
//                --isAdjusting;
//            }
//            browser.ensurePathIsVisible(fullPath);
//        }
    }

    /**
     * API method of FileChooserUI.
     */
    @Override
    public String getApproveButtonText(JFileChooser fc) {
        String buttonText = fc.getApproveButtonText();
        if (buttonText != null) {
            return buttonText;
        } else if (fc.isDirectorySelectionEnabled() && chooseButtonText != null) {
            return chooseButtonText;
        } else if (fc.getDialogType() == JFileChooser.OPEN_DIALOG) {
            return openButtonText;
        } else if (fc.getDialogType() == JFileChooser.SAVE_DIALOG) {
            return saveButtonText;
        } else {
            return null;
        }
    }

    /**
     * API method of FileChooserUI.
     */
    @Override
    public FileView getFileView(JFileChooser fc) {
        return fileView;
    }

    /**
     * API method of FileChooserUI.
     */
    @Override
    public void rescanCurrentDirectory(JFileChooser fc) {
        // Validation is only necessary, when the JFileChooser is showing.
        if (fc.isShowing()) {
            //clearIconCache();
            TreePath subtreePath = first(activeView.getSelection());
            TreePath fullPath = subtreeModel.toFullPath(subtreePath);
            model.lazyInvalidatePath(fullPath);
            model.validatePath(fullPath);
        }
    }
// *******************************************************
// ******** End of FileChooserUI PLAF methods ************
// *******************************************************

// *******************************************************
// ********** BasicFileChooserUI PLAF methods ************
// *******************************************************

    @Override
    public void clearIconCache() {
        if (fileView instanceof BasicFileView) {
            BasicFileView fv = (BasicFileView) fileView;
            fv.clearIconCache();
        }
    }

// *******************************************************
// ******* End of BasicFileChooserUI PLAF methods ********
// *******************************************************

    private class SidebarSelectionListener implements TreeSelectionListener {

        @Override
        public void valueChanged(TreeSelectionEvent e) {
            if (isAdjusting != 0) {
                return;
            }

            if (sidebarTree != null) {
                selectRootFromSidebarSelection();
            }
        }
    }

    /**
     * Responds to an Open or Save request
     */
    protected class AquaApproveSelectionAction extends AbstractAction {

        protected AquaApproveSelectionAction() {
            super("approveSelection");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            maybeApproveSelection();
        }
    }
}
