/*
 * Copyright (c) 2014-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.io.File;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaAppearance;
import org.violetlib.aqua.AquaImageFactory;

/**
 * A node in the sidebar tree model.
 */
public class SidebarTreeNode
    extends DefaultMutableTreeNode {

    /*
     * Note: SidebarTreeNode must not implement Comparable and must not override equals()/hashCode(), because this
     * confuses the layout algorithm in JTree.
     */

    private @NotNull OSXFile.SystemItemInfo info;
    private @NotNull String name;
    private @Nullable File file;
    private @NotNull Icon icon;
    private @Nullable Icon darkIcon;    // cached icon for use in dark mode

    public SidebarTreeNode(@NotNull OSXFile.SystemItemInfo info, @Nullable Icon icon) {
        this.info = info;
        this.name = info.getName();
        this.file = new File(info.getPath());
        this.icon = determineIcon(info.getIcon(), icon);
    }

    private static @NotNull Icon determineIcon(@Nullable Icon systemIcon, @Nullable Icon icon) {
        if (icon != null) {
            return icon;
        }
        if (systemIcon == null) {
            throw new IllegalArgumentException("Icon required");
        }
        return systemIcon;
    }

    /**
     * Return the file identified by this node, if any.
     */
    public @Nullable File getResolvedFile() {
        return file;
    }

    /**
     * Return the name to display.
     */
    @NotNull String getUserName() {
        return name;
    }

    /**
     * Return the icon to display.
     */
    @NotNull Icon getIcon(@NotNull AquaAppearance appearance) {
        if (appearance.isDark()) {
            if (darkIcon == null) {
                Image im = AquaImageFactory.getProcessedImage(icon, AquaImageFactory.INVERT_FOR_DARK_MODE);
                if (im != null) {
                    darkIcon = new ImageIcon(im);
                }
            }
            if (darkIcon != null) {
                return darkIcon;
            }
        }

        return icon;
    }

    /**
     * Return the basic icon. Used only to identify changes to the basic icon.
     */
    @NotNull Icon getBasicIcon() {
        return icon;
    }

    public void update(@NotNull OSXFile.SystemItemInfo info) {
        this.info = info;
        if (info.getIcon() != null) {
            this.icon = info.getIcon();
            this.darkIcon = null;
        }
    }

    public int getSequenceNumber() {
        return info.getSequenceNumber();
    }

    @Override
    public boolean getAllowsChildren() {
        return false;
    }
}
