/*
 * Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImageObserver;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.Map;

import org.jetbrains.annotations.*;

/**
 *
 */

public abstract class DelegatedGraphicsBase
  extends Graphics2D
{
    protected final @NotNull Graphics2D delegate;

    protected DelegatedGraphicsBase(@NotNull Graphics g)
    {
        this.delegate = (Graphics2D) g;
    }

    @Override
    public void draw3DRect(int x, int y, int width, int height, boolean raised)
    {
        delegate.draw3DRect(x, y, width, height, raised);
    }

    @Override
    public void fill3DRect(int x, int y, int width, int height, boolean raised)
    {
        delegate.fill3DRect(x, y, width, height, raised);
    }

    @Override
    public void draw(@NotNull Shape s)
    {
        delegate.draw(s);
    }

    @Override
    public boolean drawImage(@NotNull Image img, AffineTransform xform, ImageObserver obs)
    {
        return delegate.drawImage(img, xform, obs);
    }

    @Override
    public void drawImage(@NotNull BufferedImage img, BufferedImageOp op, int x, int y)
    {
        delegate.drawImage(img, op, x, y);
    }

    @Override
    public void drawRenderedImage(@NotNull RenderedImage img, AffineTransform xform)
    {
        delegate.drawRenderedImage(img, xform);
    }

    @Override
    public void drawRenderableImage(@NotNull RenderableImage img, AffineTransform xform)
    {
        delegate.drawRenderableImage(img, xform);
    }

    @Override
    public void drawString(@NotNull String str, int x, int y)
    {
        delegate.drawString(str, x, y);
    }

    @Override
    public void drawString(@NotNull String str, float x, float y)
    {
        delegate.drawString(str, x, y);
    }

    @Override
    public void drawString(@NotNull AttributedCharacterIterator iterator, int x, int y)
    {
        delegate.drawString(iterator, x, y);
    }

    @Override
    public void drawString(@NotNull AttributedCharacterIterator iterator, float x, float y)
    {
        delegate.drawString(iterator, x, y);
    }

    @Override
    public void drawGlyphVector(@NotNull GlyphVector g, float x, float y)
    {
        delegate.drawGlyphVector(g, x, y);
    }

    @Override
    public void fill(Shape s)
    {
        delegate.fill(s);
    }

    @Override
    public boolean hit(@NotNull Rectangle rect, Shape s, boolean onStroke)
    {
        return delegate.hit(rect, s, onStroke);
    }

    @Override
    public @NotNull GraphicsConfiguration getDeviceConfiguration()
    {
        return delegate.getDeviceConfiguration();
    }

    @Override
    public void setComposite(@NotNull Composite comp)
    {
        delegate.setComposite(comp);
    }

    @Override
    public void setPaint(@NotNull Paint paint)
    {
        delegate.setPaint(paint);
    }

    @Override
    public void setStroke(@NotNull Stroke s)
    {
        delegate.setStroke(s);
    }

    @Override
    public void setRenderingHint(@NotNull RenderingHints.Key hintKey, Object hintValue)
    {
        delegate.setRenderingHint(hintKey, hintValue);
    }

    @Override
    public Object getRenderingHint(@NotNull RenderingHints.Key hintKey)
    {
        return delegate.getRenderingHint(hintKey);
    }

    @Override
    public void setRenderingHints(@NotNull Map<?, ?> hints)
    {
        delegate.setRenderingHints(hints);
    }

    @Override
    public void addRenderingHints(@NotNull Map<?, ?> hints)
    {
        delegate.addRenderingHints(hints);
    }

    @Override
    public @NotNull RenderingHints getRenderingHints()
    {
        return delegate.getRenderingHints();
    }

    @Override
    public void translate(int x, int y)
    {
        delegate.translate(x, y);
    }

    @Override
    public void translate(double tx, double ty)
    {
        delegate.translate(tx, ty);
    }

    @Override
    public void rotate(double theta)
    {
        delegate.rotate(theta);
    }

    @Override
    public void rotate(double theta, double x, double y)
    {
        delegate.rotate(theta, x, y);
    }

    @Override
    public void scale(double sx, double sy)
    {
        delegate.scale(sx, sy);
    }

    @Override
    public void shear(double shx, double shy)
    {
        delegate.shear(shx, shy);
    }

    @Override
    public void transform(@NotNull AffineTransform Tx)
    {
        delegate.transform(Tx);
    }

    @Override
    public void setTransform(@NotNull AffineTransform Tx)
    {
        delegate.setTransform(Tx);
    }

    @Override
    public @NotNull AffineTransform getTransform()
    {
        return delegate.getTransform();
    }

    @Override
    public @NotNull Paint getPaint()
    {
        return delegate.getPaint();
    }

    @Override
    public @NotNull Composite getComposite()
    {
        return delegate.getComposite();
    }

    @Override
    public void setBackground(@NotNull Color color)
    {
        delegate.setBackground(color);
    }

    @Override
    public @NotNull Color getBackground()
    {
        return delegate.getBackground();
    }

    @Override
    public @NotNull Stroke getStroke()
    {
        return delegate.getStroke();
    }

    @Override
    public void clip(@NotNull Shape s)
    {
        delegate.clip(s);
    }

    @Override
    public @NotNull FontRenderContext getFontRenderContext()
    {
        return delegate.getFontRenderContext();
    }

    @Override
    public abstract @NotNull Graphics create();

    @Override
    public abstract @NotNull Graphics create(int x, int y, int width, int height);

    @Override
    public @NotNull Color getColor()
    {
        return delegate.getColor();
    }

    @Override
    public void setColor(@NotNull Color c)
    {
        delegate.setColor(c);
    }

    @Override
    public void setPaintMode()
    {
        delegate.setPaintMode();
    }

    @Override
    public void setXORMode(@NotNull Color c1)
    {
        delegate.setXORMode(c1);
    }

    @Override
    public @NotNull Font getFont()
    {
        return delegate.getFont();
    }

    @Override
    public void setFont(@Nullable Font font)
    {
        delegate.setFont(font);
    }

    @Override
    public @NotNull FontMetrics getFontMetrics()
    {
        return delegate.getFontMetrics();
    }

    @Override
    public @NotNull FontMetrics getFontMetrics(Font f)
    {
        return delegate.getFontMetrics(f);
    }

    @Override
    public @NotNull Rectangle getClipBounds()
    {
        return delegate.getClipBounds();
    }

    @Override
    public void clipRect(int x, int y, int width, int height)
    {
        delegate.clipRect(x, y, width, height);
    }

    @Override
    public void setClip(int x, int y, int width, int height)
    {
        delegate.setClip(x, y, width, height);
    }

    @Override
    public @NotNull Shape getClip()
    {
        return delegate.getClip();
    }

    @Override
    public void setClip(@NotNull Shape clip)
    {
        delegate.setClip(clip);
    }

    @Override
    public void copyArea(int x, int y, int width, int height, int dx, int dy)
    {
        delegate.copyArea(x, y, width, height, dx, dy);
    }

    @Override
    public void drawLine(int x1, int y1, int x2, int y2)
    {
        delegate.drawLine(x1, y1, x2, y2);
    }

    @Override
    public void fillRect(int x, int y, int width, int height)
    {
        delegate.fillRect(x, y, width, height);
    }

    @Override
    public void drawRect(int x, int y, int width, int height)
    {
        delegate.drawRect(x, y, width, height);
    }

    @Override
    public void clearRect(int x, int y, int width, int height)
    {
        delegate.clearRect(x, y, width, height);
    }

    @Override
    public void drawRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight)
    {
        delegate.drawRoundRect(x, y, width, height, arcWidth, arcHeight);
    }

    @Override
    public void fillRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight)
    {
        delegate.fillRoundRect(x, y, width, height, arcWidth, arcHeight);
    }

    @Override
    public void drawOval(int x, int y, int width, int height)
    {
        delegate.drawOval(x, y, width, height);
    }

    @Override
    public void fillOval(int x, int y, int width, int height)
    {
        delegate.fillOval(x, y, width, height);
    }

    @Override
    public void drawArc(int x, int y, int width, int height, int startAngle, int arcAngle)
    {
        delegate.drawArc(x, y, width, height, startAngle, arcAngle);
    }

    @Override
    public void fillArc(int x, int y, int width, int height, int startAngle, int arcAngle)
    {
        delegate.fillArc(x, y, width, height, startAngle, arcAngle);
    }

    @Override
    public void drawPolyline(int @NotNull [] xPoints, int @NotNull [] yPoints, int nPoints)
    {
        delegate.drawPolyline(xPoints, yPoints, nPoints);
    }

    @Override
    public void drawPolygon(int @NotNull [] xPoints, int @NotNull [] yPoints, int nPoints)
    {
        delegate.drawPolygon(xPoints, yPoints, nPoints);
    }

    @Override
    public void drawPolygon(@NotNull Polygon p)
    {
        delegate.drawPolygon(p);
    }

    @Override
    public void fillPolygon(int @NotNull [] xPoints, int @NotNull [] yPoints, int nPoints)
    {
        delegate.fillPolygon(xPoints, yPoints, nPoints);
    }

    @Override
    public void fillPolygon(@NotNull Polygon p)
    {
        delegate.fillPolygon(p);
    }

    @Override
    public void drawChars(char @NotNull [] data, int offset, int length, int x, int y)
    {
        delegate.drawChars(data, offset, length, x, y);
    }

    @Override
    public void drawBytes(byte @NotNull [] data, int offset, int length, int x, int y)
    {
        delegate.drawBytes(data, offset, length, x, y);
    }

    @Override
    public boolean drawImage(@Nullable Image img, int x, int y, ImageObserver observer)
    {
        return delegate.drawImage(img, x, y, observer);
    }

    @Override
    public boolean drawImage(@Nullable Image img, int x, int y, int width, int height, @Nullable ImageObserver observer)
    {
        return delegate.drawImage(img, x, y, width, height, observer);
    }

    @Override
    public boolean drawImage(@Nullable Image img, int x, int y, Color bgcolor, @Nullable ImageObserver observer)
    {
        return delegate.drawImage(img, x, y, bgcolor, observer);
    }

    @Override
    public boolean drawImage(@Nullable Image img, int x, int y, int width, int height, Color bgcolor, @Nullable ImageObserver observer)
    {
        return delegate.drawImage(img, x, y, width, height, bgcolor, observer);
    }

    @Override
    public boolean drawImage(@Nullable Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, @Nullable ImageObserver observer)
    {
        return delegate.drawImage(img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, observer);
    }

    @Override
    public boolean drawImage(@Nullable Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, Color bgcolor, @Nullable ImageObserver observer)
    {
        return delegate.drawImage(img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, bgcolor, observer);
    }

    @Override
    public void dispose()
    {
        delegate.dispose();
    }

    @Override
    public @NotNull String toString()
    {
        return delegate.toString();
    }

    @Deprecated
    public @NotNull Rectangle getClipRect()
    {
        return delegate.getClipRect();
    }

    @Override
    public boolean hitClip(int x, int y, int width, int height)
    {
        return delegate.hitClip(x, y, width, height);
    }

    @Override
    public @NotNull Rectangle getClipBounds(@NotNull Rectangle r)
    {
        return delegate.getClipBounds(r);
    }
}
