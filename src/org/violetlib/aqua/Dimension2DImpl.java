/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.geom.Dimension2D;

public final class Dimension2DImpl
  extends Dimension2D
{
    private final float width;
    private final float height;

    public Dimension2DImpl(float width, float height)
    {
        this.width = width;
        this.height = height;
    }

    @Override
    public double getWidth()
    {
        return width;
    }

    @Override
    public double getHeight()
    {
        return height;
    }

    @Override
    public void setSize(Dimension2D d)
    {
        throw new UnsupportedOperationException("Dimension2DImpl is immutable");
    }

    @Override
    public void setSize(double width, double height)
    {
        throw new UnsupportedOperationException("Dimension2DImpl is immutable");
    }
}
