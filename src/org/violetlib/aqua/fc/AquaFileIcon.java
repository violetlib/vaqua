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
import javax.swing.event.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * An object that represents the possibly evolving state of an icon for a file.
 *
 * On macOS, there are two sources of icons for files used by a file chooser. One source is Launch Services, which
 * provides generic icons based on file type. The other source is Quick Look, which provides custom icons for certain
 * types of files. For example, the Quick Look icon for an image file is a reduced version of the image.
 *
 * Quick Look icons are preferred, but are not always immediately available. This class supports the strategy used by
 * native file choosers, which is to display the Launch Services icon until the Quick Look icon becomes available.
 */

public class AquaFileIcon implements Icon {

    private final int width;
    private final int height;
    private @Nullable Image image;
    private int iconPriority;
    private final @NotNull EventListenerList listenerList = new EventListenerList();

    public static final int ICON_LAUNCH_SERVICES = 1;
    public static final int ICON_QUICK_LOOK = 2;

    public AquaFileIcon(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public synchronized void installImage(@NotNull Image image, int priority)
    {
        if (priority > iconPriority) {
            this.image = image;
            this.iconPriority = priority;
            iconChanged();
        }
    }

    public void addChangeListener(@NotNull ChangeListener listener)
    {
        listenerList.add(ChangeListener.class, listener);
    }

    public void removeChangeListener(@NotNull ChangeListener listener)
    {
        listenerList.remove(ChangeListener.class, listener);
    }

    private void iconChanged() {
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
