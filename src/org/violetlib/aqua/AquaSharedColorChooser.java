/*
 * Copyright (c) 2015-2017 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.colorchooser.ColorSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.*;

/**
 * Simulates the AppKit shared color panel, which can be connected to a color well.
 */
public class AquaSharedColorChooser {

    private static boolean useNativeChooser = true;

    private static AquaSharedColorChooser INSTANCE;

    /**
     * Connect the shared color chooser to the specified owner and display the chooser.
     * @param o The owner to be connected to the chooser. The owner will be called to apply the color when the user
     *          makes a selection. The user may make multiple selections while the chooser is connected to the owner.
     *          The owner will be called if the user dismisses the chooser or if the chooser is connected to a new
     *          owner. Otherwise, the owner should disconnect from the chooser when the connection is no longer desired.
     * @return true if successful, false otherwise.
     */
    public static boolean connect(SharedColorChooserOwner o, Color initialColor, boolean enableTranslucentColors) {
        AquaSharedColorChooser c = getInstance();
        return c.connectToOwner(o, initialColor, enableTranslucentColors);
    }

    /**
      * Disconnect the shared color chooser from the specified owner.
      * This method has no effect if the shared color chooser is not currently connected to the specified owner.
      * @param o The owner to be disconnected from the chooser.
      * @return true if successful, false otherwise.
      */
     public static void disconnect(SharedColorChooserOwner o) {
         AquaSharedColorChooser c = getInstance();
         c.disconnectOwner(o);
     }

    private static AquaSharedColorChooser getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new AquaSharedColorChooser();
        }
        return INSTANCE;
    }

    private boolean isInitialized;
    private JColorChooser sharedChooser;
    private JDialog sharedDialog;
    private WindowListener windowListener;
    private ComponentListener componentListener;
    private ChangeListener changeListener;

    private SharedColorChooserOwner currentOwner;

    private AquaSharedColorChooser() {
    }

    /**
     * Connect the shared color chooser to the specified owner and display the chooser.
     * @param o The owner to be connected to the chooser. The owner will be called to apply the color when the user
     *          makes a selection. The user may make multiple selections while the chooser is connected to the owner.
     *          The owner will be called if the user dismisses the chooser or if the chooser is connected to a new
     *          owner. Otherwise, the owner should disconnect from the chooser when the connection is no longer desired.
     * @return true if successful, false otherwise.
     */
    public boolean connectToOwner(SharedColorChooserOwner o, Color initialColor, boolean enableTranslucentColors) {
        if (currentOwner == o) {
            return true;
        }

        if (currentOwner != null) {
            SharedColorChooserOwner old = currentOwner;
            currentOwner = null;
            old.disconnected();
        }

        if (!isInitialized) {

            isInitialized = true;

            if (!useNativeChooser) {

                windowListener = new WindowAdapter() {
                    @Override
                    public void windowClosed(WindowEvent e) {
                        dismiss();
                    }

                    @Override
                    public void windowIconified(WindowEvent e) {
                        dismiss();
                    }
                };

                componentListener = new ComponentAdapter() {
                    @Override
                    public void componentHidden(ComponentEvent e) {
                        super.componentHidden(e);
                        dismiss();
                    }
                };

                changeListener = new ChangeListener() {
                    @Override
                    public void stateChanged(ChangeEvent e) {
                        apply();
                    }
                };

                sharedChooser = new JColorChooser();

                ColorSelectionModel model = sharedChooser.getSelectionModel();
                model.addChangeListener(changeListener);

                sharedDialog = new AquaColorChooserDialog((JFrame) null, "", false, null, sharedChooser);
                sharedDialog.addWindowListener(windowListener);
                sharedDialog.addComponentListener(componentListener);
            }
        }

        currentOwner = o;

        if (sharedDialog != null) {
            sharedDialog.setVisible(true);
            return true;
        }

        return AquaNativeColorChooser.display(o, initialColor, enableTranslucentColors);
    }

    /**
     * Disconnect the shared color chooser from the specified owner.
     * This method has no effect if the shared color chooser is not currently connected to the specified owner.
     * @param o The owner to be disconnected from the chooser.
     * @return true if successful, false otherwise.
     */
    public void disconnectOwner(SharedColorChooserOwner o) {
        if (o == currentOwner) {
            currentOwner = null;
            if (useNativeChooser && isInitialized) {
                AquaNativeColorChooser.disconnect();
            }
        }
    }

    private void apply() {
        if (currentOwner != null) {
            currentOwner.applyColor(sharedChooser.getColor());
        }
    }

    private void dismiss() {
        if (currentOwner != null) {
            SharedColorChooserOwner old = currentOwner;
            currentOwner = null;
            old.disconnected();
        }
    }
}
