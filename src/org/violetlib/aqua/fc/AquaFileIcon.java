/*
 * Copyright (c) 2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaIcon;

/**
 * An object that represents the possibly evolving state of an icon for a file.
 */
public class AquaFileIcon implements Icon, ChangeListener {

    private final int width;
    private final int height;
    private @Nullable AquaFileIcon replacement;  // synchronized
    private @Nullable Image image;  // synchronized
    private final @NotNull EventListenerList listenerList = new EventListenerList();

    public AquaFileIcon(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public synchronized void installIcon(@NotNull Icon icon)
    {
        image = AquaIcon.getImageForIcon(icon);
        if (icon instanceof AquaFileIcon) {
            if (replacement != null) {
                replacement.removeChangeListener(this);
            }
            replacement = (AquaFileIcon) icon;
            image = replacement.getCurrentImage();
            replacement.addChangeListener(this);
        }

        iconChanged();
    }

    public void addChangeListener(@NotNull ChangeListener listener)
    {
        listenerList.add(ChangeListener.class, listener);
    }

    public void removeChangeListener(@NotNull ChangeListener listener)
    {
        listenerList.remove(ChangeListener.class, listener);
    }

    @Override
    public synchronized void stateChanged(ChangeEvent e) {
        // The replacement image has been updated
        if (replacement != null) {
            image = replacement.getCurrentImage();
            iconChanged();
        }
    }

    private synchronized void iconChanged() {
        if (SwingUtilities.isEventDispatchThread()) {
            fireChangeEvent();
        } else {
            SwingUtilities.invokeLater(() -> fireChangeEvent());
        }
    }

    // This method is called only on the UI thread
    private void fireChangeEvent() {
        Object[] listeners = listenerList.getListenerList();
        ChangeEvent e = null;
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == ChangeListener.class) {
                if (e == null) {
                    e = new ChangeEvent(this);
                }
                ((ChangeListener) listeners[i+1]).stateChanged(e);
            }
        }
    }

    public synchronized @Nullable Image getCurrentImage()
    {
        return image;
    }

    @Override
    public synchronized void paintIcon(Component c, Graphics g, int x, int y) {
        Image image = getCurrentImage();
        if (image != null) {
            g.drawImage(image, x, y, width, height, c);
        }
    }

    @Override
    public int getIconWidth() {
        return width;
    }

    @Override
    public int getIconHeight() {
        return height;
    }
}
