/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.Border;

import org.jetbrains.annotations.*;

/**
 * A border wrapper that can be used to support the context border behavior on a component with an application specified
 * border.
 */
public class ContextBorderWrapper
  implements Border, ContextBorder
{
    /**
     * Install a wrapper border on the component that delegates to the existing border of the component, if any.
     */
    public static void install(@NotNull JComponent owner)
    {
        new ContextBorderWrapper(owner);
    }

    private final @NotNull JComponent owner;
    private @Nullable Border delegate;
    private boolean isBorderPainted = true;

    private ContextBorderWrapper(@NotNull JComponent owner)
    {
        this.owner = owner;
        delegate = owner.getBorder();
        AppearanceManager.installBorderQuietly(owner, this);
    }

    public void setBorderPainted(boolean b)
    {
        isBorderPainted = b;
    }

    @Override
    public void paintBorder(@NotNull Component c, @NotNull Graphics g, int x, int y, int width, int height)
    {
        if (delegate != null && isBorderPainted) {
            delegate.paintBorder(c, g, x, y, width, height);
        }
        PaintingContext.pop(owner);
    }

    @Override
    public @NotNull Insets getBorderInsets(@NotNull Component c)
    {
        if (delegate != null) {
            return delegate.getBorderInsets(c);
        } else {
            return new Insets(0, 0, 0, 0);
        }
    }

    @Override
    public boolean isBorderOpaque()
    {
        return delegate != null && delegate.isBorderOpaque();
    }

    public void setDelegate(@Nullable Border border)
    {
        this.delegate = border;
    }

    public <B> @Nullable B get(@NotNull Class<B> c)
    {
        if (c.isInstance(delegate)) {
            return c.cast(delegate);
        }
        return null;
    }

    public void uninstall()
    {
        AppearanceManager.installBorderQuietly(owner, delegate);
        if (!isBorderPainted) {
            // If the border was not painted, restore that behavior.
            AquaBorderSupport.setBorderPainted(owner, false);
        }
    }
}
