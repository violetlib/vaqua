/*
 * Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.function.Consumer;
import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.FileChooserUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.violetlib.aqua.AquaUtils.execute;
import static org.violetlib.aqua.AquaUtils.syslog;

/**
 * Support for displaying windows as sheets.
 */
public class AquaSheetSupport {

    public static boolean isDebug = false;

    /**
     * Display an option pane in a document modal dialog as a sheet.
     * @param d The dialog.
     * @param pane The option pane.
     * @param resultConsumer If not null, this consumer will be called when the dialog is dismissed with an integer
     * indicating the option chosen by the user, or <code>CLOSED_OPTION</code> if the user dismissed the dialog without
     * choosing an option.
     * @throws HeadlessException if the graphics environment is headless.
     * @throws UnsupportedOperationException if it is not possible to display as a sheet.
     */
    public static void showOptionPaneAsSheet(JDialog d, JOptionPane pane, Consumer<Integer> resultConsumer)
            throws UnsupportedOperationException {
        Runnable closeHandler = null;
        if (resultConsumer != null) {
            closeHandler = new Runnable() {
                @Override
                public void run() {
                    resultConsumer.accept(getOption(pane));
                }
            };
        }
        displayAsSheet(d, closeHandler);
    }

    private static int getOption(JOptionPane pane) {
        Object selectedValue = pane.getValue();

        if (selectedValue == null) {
            return JOptionPane.CLOSED_OPTION;
        }

        Object[] options = pane.getOptions();
        if (options == null) {
            if(selectedValue instanceof Integer)
                return (Integer) selectedValue;
            return JOptionPane.CLOSED_OPTION;
        }

        for(int counter = 0, maxCounter = options.length;
            counter < maxCounter; counter++) {
            if(options[counter].equals(selectedValue))
                return counter;
        }
        return JOptionPane.CLOSED_OPTION;
    }

    /**
     * Display a file chooser as a document modal sheet.
     * @param owner The owner of the dialog.
     * @param fc The file chooser.
     * @param resultConsumer If not null, this object will be invoked upon dismissal of the dialog with the return state of
     * the file chooser.
     * @throws HeadlessException if the graphics environment is headless.
     * @throws UnsupportedOperationException if it is not possible to display as a sheet.
     */
    public static void showFileChooserAsSheet(Window owner, JFileChooser fc, Consumer<Integer> resultConsumer)
            throws UnsupportedOperationException {
        // We try to duplicate what JFileChooser does when showing a dialog
        // Cannot test for a dialog in progress the way that JFileChooser does...
        FileChooserUI ui = fc.getUI();
        String title = ui.getDialogTitle(fc);
        fc.putClientProperty(AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY, title);

        JDialog dialog;
        if (owner instanceof Frame) {
            dialog = new JDialog((Frame)owner, title, Dialog.ModalityType.MODELESS);
        } else {
            dialog = new JDialog((Dialog)owner, title, Dialog.ModalityType.MODELESS);
        }
        dialog.setComponentOrientation(fc.getComponentOrientation());

        Container contentPane = dialog.getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(fc, BorderLayout.CENTER);

        dialog.pack();
        dialog.setLocationRelativeTo(owner);

        FileChooserActionListener listener = new FileChooserActionListener(dialog);
        fc.addActionListener(listener);
        fc.rescanCurrentDirectory();

        Runnable closeHandler = new Runnable() {
            @Override
            public void run() {
                int returnValue = listener.returnValue;
                fc.removeActionListener(listener);

                //fc.firePropertyChange("JFileChooserDialogIsClosingProperty", dialog, null);

                // Remove all components from dialog. The MetalFileChooserUI.installUI() method (and other LAFs)
                // registers AWT listener for dialogs and produces memory leaks. It happens when
                // installUI invoked after the showDialog method.
                dialog.getContentPane().removeAll();
                dialog.dispose();
                if (resultConsumer != null) {
                    resultConsumer.accept(returnValue);
                }
            }
        };

        try {
            displayAsSheet(dialog, closeHandler);
        } catch (UnsupportedOperationException ex) {
            dialog.getContentPane().removeAll();
            dialog.dispose();
            throw ex;
        }
    }

    private static class FileChooserActionListener implements ActionListener {
        private JDialog d;

        public FileChooserActionListener(JDialog d) {
            this.d = d;
        }

        int returnValue = JFileChooser.ERROR_OPTION;
        public void actionPerformed(ActionEvent e) {
            String s = e.getActionCommand();
            if (s.equals(JFileChooser.APPROVE_SELECTION)) {
                returnValue = JFileChooser.APPROVE_OPTION;
                d.setVisible(false);
            } else if (s.equals(JFileChooser.CANCEL_SELECTION)) {
                returnValue = JFileChooser.CANCEL_OPTION;
                d.setVisible(false);
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
    public static void displayAsSheet(Window w, Runnable closeHandler) throws UnsupportedOperationException {
        Window owner = w.getOwner();
        if (owner == null) {
            throw new UnsupportedOperationException("Unable to display as sheet: no owner window");
        }

        if (!owner.isVisible()) {
            throw new UnsupportedOperationException("Unable to display as sheet: owner window is not visible");
        }

        if (w.isVisible()) {
            throw new UnsupportedOperationException("Unable to display as sheet: the window must not be visible");
        }

        if (w instanceof Dialog) {
            Dialog d = (Dialog) w;
            d.setModalityType(Dialog.ModalityType.MODELESS);
        }

        AquaUtils.ensureWindowPeer(w);

        JRootPane rp = AquaUtils.getRootPane(w);
        Object oldBackgroundStyle = null;
        Dimension originalSize = w.getSize();

        if (rp != null) {
            oldBackgroundStyle = rp.getClientProperty(AquaVibrantSupport.BACKGROUND_STYLE_KEY);
        }

        // The window should not be decorated. If it is decorated, the initial painting will go in the wrong place.
        // Unfortunately, Java is very picky about when setUndecorated() can be called. So we just munge the style bits
        // directly.

        boolean needToUndecorate = false;
        if (w instanceof Dialog) {
            Dialog d = (Dialog) w;
            if (!d.isUndecorated()) {
                needToUndecorate = true;
            }
        } else if (w instanceof Frame) {
            Frame fr = (Frame) w;
            if (!fr.isUndecorated()) {
                needToUndecorate = true;
            }
        }

        int oldTop = 0;
        AquaCustomStyledWindow sw;
        String windowStyle = null;

        if (needToUndecorate) {
            sw = AquaUtils.getCustomStyledWindow(w);
            if (sw != null && sw.getStyle() != AquaCustomStyledWindow.STYLE_UNDECORATED) {
                // remove the decorated style
                assert rp != null;
                windowStyle = AquaRootPaneUI.getWindowStyleKey(rp);
                debug("Setting window style client property to undecorated");
                rp.putClientProperty(AquaRootPaneUI.AQUA_WINDOW_STYLE_KEY, null);
            }
        }

        if (needToUndecorate) {
            try {
                debug("Setting native window style to undecorated");
                oldTop = AquaUtils.unsetTitledWindowStyle(w);
            } catch (UnsupportedOperationException ex) {
                throw new UnsupportedOperationException("Unable to display as sheet: " + ex.getMessage());
            }
        }

        if (rp != null) {

            debug("Setting background style client property to vibrantSheet");
            rp.putClientProperty(AquaVibrantSupport.BACKGROUND_STYLE_KEY, "vibrantSheet");

            if (windowStyle != null) {
                // replace the decorated style with the undecorated style
                debug("Setting window style client property to undecorated");
                rp.putClientProperty(AquaRootPaneUI.AQUA_WINDOW_STYLE_KEY, "undecorated");
            }

            w.validate();
            debug("Painting the sheet");
            AquaUtils.paintImmediately(w, rp);
        }

        // It would be better to dismiss a sheet by calling endSheet. Using endSheet supports deferred and critical
        // sheets. However, existing dialogs all dismiss themselves by calling setVisible(false) and we have no way to
        // alter what that does.

        SheetCloser closer = new SheetCloser(w, closeHandler, oldBackgroundStyle, windowStyle, oldTop, originalSize);
        int result;
        if ("true".equals(System.getProperty("VAqua.injectSheetDisplayFailure"))) {
            // inject failure for testing
            Utils.logDebug("Injected failure to display sheet");
            result = -1;
        } else {
            result = (int) execute(w, ptr -> displayAsSheet(ptr, owner));
        }

        if (result != 0) {
            closer.dispose();
            throw new UnsupportedOperationException("Unable to display as sheet");
        }

        w.setVisible(true); // cause the lightweight components to be painted -- this method blocks on a modal dialog
    }

    private static long displayAsSheet(long wptr, Window owner) {
        return execute(owner, owner_wptr -> nativeDisplayAsSheet(wptr, owner_wptr));
    }

    /**
     * Determine whether a window is being displayed as a sheet.
     */
    public static boolean isSheet(Window w) {
        JRootPane rp = AquaUtils.getRootPane(w);
        return rp != null && isSheet(rp);
    }

    /**
     * Determine whether a component is being displayed in a sheet.
     */
    public static boolean isSheet(@NotNull JComponent c) {
        JRootPane rp = c.getRootPane();
        if (rp != null) {
            Object style = rp.getClientProperty(AquaVibrantSupport.BACKGROUND_STYLE_KEY);
            return isSheetFromBackgroundStyle(style);
        } else {
            return false;
        }
    }

    public static void registerIsSheetChangeListener(JRootPane rp, ChangeListener l) {
        rp.addPropertyChangeListener(AquaVibrantSupport.BACKGROUND_STYLE_KEY, new SheetPropertyChangeListener(rp, l));
    }

    public static void unregisterIsSheetChangeListener(JRootPane rp, ChangeListener l) {
        PropertyChangeListener[] pcls = rp.getPropertyChangeListeners(AquaVibrantSupport.BACKGROUND_STYLE_KEY);
        for (PropertyChangeListener pcl : pcls) {
            if (pcl instanceof SheetPropertyChangeListener) {
                SheetPropertyChangeListener spcl = (SheetPropertyChangeListener) pcl;
                if (spcl.rp == rp && spcl.l == l) {
                    rp.removePropertyChangeListener(AquaVibrantSupport.BACKGROUND_STYLE_KEY, spcl);
                }
            }
        }
    }

    private static class SheetPropertyChangeListener implements PropertyChangeListener {
        private JRootPane rp;
        private ChangeListener l;

        public SheetPropertyChangeListener(JRootPane rp, ChangeListener l) {
            this.rp = rp;
            this.l = l;
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            boolean wasSheet = isSheetFromBackgroundStyle(evt.getOldValue());
            boolean isSheet = isSheetFromBackgroundStyle(evt.getNewValue());
            if (isSheet != wasSheet) {
                l.stateChanged(new ChangeEvent(rp));
            }
        }
    }

    private static boolean isSheetFromBackgroundStyle(Object o) {
        return "vibrantSheet".equals(o);
    }

    /**
     * A sheet closer performs the necessary operations when a sheet is dismissed.
     */
    private static class SheetCloser extends WindowAdapter implements HierarchyListener {
        private final @NotNull Window w;
        private final @Nullable Runnable closeHandler;
        private final @Nullable Object oldBackgroundStyle;
        private final @Nullable String windowStyle;
        private final int oldTop;
        private final @NotNull Dimension originalSize;
        private boolean hasClosed = false;

        public SheetCloser(@NotNull Window w,
                           @Nullable Runnable closeHandler,
                           @Nullable Object oldBackgroundStyle,
                           @Nullable String windowStyle,
                           int oldTop,
                           @NotNull Dimension originalSize) {
            this.w = w;
            this.closeHandler = closeHandler;
            this.oldBackgroundStyle = oldBackgroundStyle;
            this.windowStyle = windowStyle;
            this.oldTop = oldTop;
            this.originalSize = originalSize;
            w.addWindowListener(this);
            w.addHierarchyListener(this);
        }

        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0 && !w.isVisible()) {
                completed();
            }
        }

        @Override
        public void windowClosed(WindowEvent e) {
            completed();
        }

        private void completed() {
            if (!hasClosed) {
                hasClosed = true;
                dispose();
                if (closeHandler != null) {
                    closeHandler.run();
                }
            }
        }

        public void dispose() {
            w.removeWindowListener(this);
            w.removeHierarchyListener(this);
            JRootPane rp = AquaUtils.getRootPane(w);
            if (rp != null) {
                rp.putClientProperty(AquaVibrantSupport.BACKGROUND_STYLE_KEY, oldBackgroundStyle);
                if (oldTop > 0) {
                    AquaUtils.restoreTitledWindowStyle(w, oldTop, originalSize);
                    AquaUtils.syncAWTView(w);
                }
                if (windowStyle != null) {
                    rp.putClientProperty(AquaRootPaneUI.AQUA_WINDOW_STYLE_KEY, windowStyle);
                }
            }
        }
    }

    private static void debug(@NotNull String message) {
        if (isDebug) {
            syslog(message);
        }
    }

    private static native int nativeDisplayAsSheet(long wptr, long owner_wptr);
}
