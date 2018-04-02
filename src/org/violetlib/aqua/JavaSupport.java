/*
 * Changes copyright (c) 2016-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.ImageFilter;
import java.util.function.Function;
import javax.swing.*;

/**
 * Platform support that varies based on the Java version.
 */

public class JavaSupport {

    public interface JavaSupportImpl {
        int getScaleFactor(Graphics g);
        boolean hasOpaqueBeenExplicitlySet(final JComponent c);
        Image getDockIconImage();
        void drawString(JComponent c, Graphics2D g, String string, float x, float y);
        void drawStringUnderlineCharAt(JComponent c, Graphics2D g, String string, int underlinedIndex,
                                       float x, float y);
        String getClippedString(JComponent c, FontMetrics fm, String string, int availTextWidth);
        float getStringWidth(JComponent c, FontMetrics fm, String string);
        void installAATextInfo(UIDefaults table);
        AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im);
        AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im1, BufferedImage im2);
        Image applyFilter(Image image, ImageFilter filter);
        Image applyMapper(Image source, Function<Image,Image> mapper);
        Image applyMapper(Image source, AquaMultiResolutionImage.Mapper mapper);
        BufferedImage createImage(int width, int height, int[] data);
        void preload(Image image, int availableInfo);
        void lockRenderQueue();
        void unlockRenderQueue();
        AquaPopupFactory createPopupFactory();
    }

    private final static JavaSupportImpl impl = findImpl();

    public static int getScaleFactor(Graphics g)
    {
        return impl.getScaleFactor(g);
    }

    public static boolean hasOpaqueBeenExplicitlySet(final JComponent c) {
        return impl.hasOpaqueBeenExplicitlySet(c);
    }

    public static Image getDockIconImage() {
        return impl.getDockIconImage();
    }

    public static void drawString(JComponent c, Graphics2D g, String string, float x, float y) {
        impl.drawString(c, g, string, x, y);
    }

    public static void drawStringUnderlineCharAt(JComponent c, Graphics2D g, String string, int underlinedIndex,
                                                 float x, float y) {
        impl.drawStringUnderlineCharAt(c, g, string, underlinedIndex, x, y);
    }

    public static String getClippedString(JComponent c, FontMetrics fm, String string, int availTextWidth) {
        return impl.getClippedString(c, fm, string, availTextWidth);
    }

    public static float getStringWidth(JComponent c, FontMetrics fm, String string) {
        return impl.getStringWidth(c, fm, string);
    }

    public static void installAATextInfo(UIDefaults table) {
        impl.installAATextInfo(table);
    }

    public static AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im) {
        return impl.createMultiResolutionImage(im);
    }

    public static AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im1, BufferedImage im2) {
        return impl.createMultiResolutionImage(im1, im2);
    }

    public static Image applyFilter(Image image, ImageFilter filter) {
        return impl.applyFilter(image, filter);
    }

    public static Image applyMapper(Image source, Function<Image,Image> mapper) {
        return impl.applyMapper(source, mapper);
    }

    public static Image applyMapper(Image source, AquaMultiResolutionImage.Mapper mapper) {
        return impl.applyMapper(source, mapper);
    }

    /**
     * Create a buffered image from a raster (created by native code).
     */
    public static BufferedImage createImage(int width, int height, int[] data) {
        return impl.createImage(width, height, data);
    }

    public static void preload(Image image, int availableInfo) {
        impl.preload(image, availableInfo);
    }

    // This method supports a work around for bug JDK-8046290, which causes the transient display of garbage pixels.
    public static void lockRenderQueue() {
        impl.lockRenderQueue();
    }

    public static void unlockRenderQueue() {
        impl.unlockRenderQueue();
    }

    public static AquaPopupFactory createPopupFactory() {
        return impl.createPopupFactory();
    }

    private static JavaSupportImpl findImpl() {
        int version = AquaUtils.getJavaVersion();
        String className;
        if (version >= 900000) {
            className = "Java9Support";
        } else {
            className = "Java8Support";
        }
        try {
            Class c = Class.forName("org.violetlib.aqua." + className);
            return (JavaSupportImpl) c.getDeclaredConstructor().newInstance();
        } catch (Exception ex) {
            throw new UnsupportedOperationException("Unsupported Java version: " + version, ex);
        }
    }
}
