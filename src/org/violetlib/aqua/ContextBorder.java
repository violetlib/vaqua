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
 *
 */

public class ContextBorder
  implements Border
{
    public static void install(@NotNull JComponent owner)
    {
        new ContextBorder(owner);
    }

    private final @NotNull JComponent owner;
    private @Nullable Border delegate;

    // Information to be restored when the context is no longer valid

    private @Nullable PaintingContext oldPaintingContext;
    private @Nullable Boolean oldIsBorderPainted;

    private ContextBorder(@NotNull JComponent owner)
    {
        this.owner = owner;
        delegate = owner.getBorder();
        oldIsBorderPainted = AquaBorderSupport.isBorderPainted(owner);
        AppearanceManager.installBorderQuietly(owner, this);
    }

    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height)
    {
        if (delegate != null && !Boolean.FALSE.equals(oldIsBorderPainted)) {
            delegate.paintBorder(c, g, x, y, width, height);
        }
    }

    @Override
    public Insets getBorderInsets(Component c)
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

    public @Nullable PaintingContext extractOldPaintingContext()
    {
        PaintingContext pc = oldPaintingContext;
        oldPaintingContext = null;
        return pc;
    }

    public void setOldPaintingContext(@Nullable PaintingContext context)
    {
        this.oldPaintingContext = context;
    }

    public void setOldIsBorderPainted(boolean b)
    {
        oldIsBorderPainted = b;
    }

    public void uninstall()
    {
        AppearanceManager.installBorderQuietly(owner, delegate);
        if (oldIsBorderPainted != null) {
            AquaBorderSupport.setBorderPainted(owner, oldIsBorderPainted);
        }
    }
}
