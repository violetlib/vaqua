/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneDivider;

import org.jetbrains.annotations.*;

/**
 *
 */

public final class AquaBorderSupport
{
    public static @Nullable ContextBorder getContextBorder(@NotNull JComponent jc)
    {
        Border b = jc.getBorder();
        if (b instanceof ContextBorder) {
            return (ContextBorder) b;
        }
        return null;
    }

    public static @Nullable ContextBorderWrapper getContextBorderWrapper(@NotNull JComponent jc)
    {
        Border b = jc.getBorder();
        if (b instanceof ContextBorderWrapper) {
            return (ContextBorderWrapper) b;
        }
        return null;
    }

    /**
     * Return the component border. If the border is a ContextBorder, return the delegate.
     */
    public static @Nullable Border getBorder(@NotNull JComponent component)
    {
        return get(component, Border.class);
    }

    public static <B> @Nullable B get(@NotNull JComponent component, @NotNull Class<B> c)
    {
        Border b = component.getBorder();
        return b != null ? get(b, c) : null;
    }

    public static <B> @Nullable B get(@NotNull BasicSplitPaneDivider d, @NotNull Class<B> c)
    {
        Border b = d.getBorder();
        return b != null ? get(b, c) : null;
    }

    public static <B> @Nullable B get(@NotNull Border b, @NotNull Class<B> c)
    {
        if (c.isInstance(b)) {
            return c.cast(b);
        }
        if (b instanceof ContextBorderWrapper) {
            ContextBorderWrapper cb = (ContextBorderWrapper) b;
            return cb.get(c);
        }
        return null;
    }

    public static @Nullable Boolean isBorderPainted(@NotNull JComponent jc)
    {
        if (jc instanceof AbstractButton) {
            AbstractButton ab = (AbstractButton) jc;
            return ab.isBorderPainted();
        } else if (jc instanceof JMenuBar) {
            JMenuBar mb = (JMenuBar) jc;
            return mb.isBorderPainted();
        } else if (jc instanceof JPopupMenu) {
            JPopupMenu pm = (JPopupMenu) jc;
            return pm.isBorderPainted();
        } else if (jc instanceof JProgressBar) {
            JProgressBar pb = (JProgressBar) jc;
            return pb.isBorderPainted();
        } else if (jc instanceof JToolBar) {
            JToolBar tb = (JToolBar) jc;
            return tb.isBorderPainted();
        } else {
            return null;
        }
    }

    public static void setBorderPainted(@NotNull JComponent jc, boolean b)
    {
        if (jc instanceof AbstractButton) {
            AbstractButton ab = (AbstractButton) jc;
            ab.setBorderPainted(b);
        } else if (jc instanceof JMenuBar) {
            JMenuBar mb = (JMenuBar) jc;
            mb.setBorderPainted(b);
        } else if (jc instanceof JPopupMenu) {
            JPopupMenu pm = (JPopupMenu) jc;
            pm.setBorderPainted(b);
        } else if (jc instanceof JProgressBar) {
            JProgressBar pb = (JProgressBar) jc;
            pb.setBorderPainted(b);
        } else if (jc instanceof JToolBar) {
            JToolBar tb = (JToolBar) jc;
            tb.setBorderPainted(b);
        } else {
            Utils.logError("setBorderPainted unexpected on " + jc.getClass().getSimpleName());
        }
    }

    public static void installBorder(@NotNull JComponent jc, @Nullable Border b)
    {
        Border oldBorder = jc.getBorder();
        if (oldBorder instanceof ContextBorderWrapper) {
            ContextBorderWrapper contextBorder = (ContextBorderWrapper) oldBorder;
            contextBorder.setDelegate(b);
            installBorderQuietly(jc, contextBorder);
        } else {
            jc.setBorder(b);
        }
    }

    public static void installBorder(@NotNull BasicSplitPaneDivider d, @Nullable Border b)
    {
        Border oldBorder = d.getBorder();
        if (oldBorder instanceof ContextBorderWrapper) {
            ContextBorderWrapper contextBorder = (ContextBorderWrapper) oldBorder;
            contextBorder.setDelegate(b);
            installBorderQuietly(d, contextBorder);
        } else {
            d.setBorder(b);
        }
    }

    public static void installBorderQuietly(@NotNull JComponent jc, @Nullable Border b)
    {
        AppearanceManager.installBorderQuietly(jc, b);
    }

    public static void installBorderQuietly(@NotNull BasicSplitPaneDivider d, @Nullable Border b)
    {
        AppearanceManager.installBorderQuietly(d, b);
    }
}
