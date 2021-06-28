/*
 * Copyright (c) 2019-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.violetlib.aqua.fc.FileIconService.debugFlag;

/**
 * An icon that can be updated with new images.
 */
public class DynamicIcon implements Icon, ChangeListener {

    private final int width;
    private final int height;
    private @Nullable DynamicIcon replacement;  // synchronized
    private @Nullable Image image;  // synchronized
    private final @NotNull EventListenerList listenerList = new EventListenerList();

    public DynamicIcon(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public void installIcon(@NotNull Icon icon) {
        boolean isChanged = false;
        synchronized (this) {
            if (icon instanceof DynamicIcon) {
                if (replacement != null) {
                    replacement.removeChangeListener(this);
                }
                replacement = (DynamicIcon) icon;
                image = replacement.getCurrentImage();
                replacement.addChangeListener(this);
                isChanged = true;
            } else {
                Image im = AquaIcon.getImageForIcon(icon);
                if (im != null) {
                    image = im;
                    isChanged = true;
                } else {
                    Utils.logError("No image for icon: " + icon);
                }
            }
        }
        if (isChanged) {
            if (debugFlag) {
                Utils.logDebug("Icon updated");
            }
            iconChanged();
        }
    }

    public void addChangeListener(@NotNull ChangeListener listener) {
        listenerList.add(ChangeListener.class, listener);
    }

    public void removeChangeListener(@NotNull ChangeListener listener) {
        listenerList.remove(ChangeListener.class, listener);
    }

    @Override
    public synchronized void stateChanged(ChangeEvent e) {
        // The replacement image has been updated
        if (replacement != null) {
            if (debugFlag) {
                Utils.logDebug("Replacement icon updated");
            }
            image = replacement.getCurrentImage();
            iconChanged();
        }
    }

    private synchronized void iconChanged() {
        if (SwingUtilities.isEventDispatchThread()) {
            fireChangeEvent();
        } else {
            SwingUtilities.invokeLater(this::fireChangeEvent);
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

    public synchronized @Nullable Image getCurrentImage() {
        return image;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
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
