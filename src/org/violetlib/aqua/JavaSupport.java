/*
 * Changes copyright (c) 2016-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.image.BufferedImage;
import java.awt.image.ImageFilter;
import java.util.function.Function;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;

/**
 * Platform support that varies based on the Java version.
 */

public class JavaSupport {

    public interface FocusEventCause
    {
        int UNKNOWN = 0;
        int MOUSE_EVENT = 1;
        int TRAVERSAL = 2;
        int TRAVERSAL_UP = 3;
        int TRAVERSAL_DOWN = 4;
        int TRAVERSAL_FORWARD = 5;
        int TRAVERSAL_BACKWARD = 6;
        int ROLLBACK = 7;
        int UNEXPECTED = 8;
        int ACTIVATION = 9;
        int CLEAR_GLOBAL_FOCUS_OWNER = 10;
    }

    public interface JavaSupportImpl {
        int getScaleFactor(Graphics g);
        boolean hasOpaqueBeenExplicitlySet(JComponent c);
        Image getDockIconImage();
        void drawString(JComponent c, Graphics2D g, String string, float x, float y);
        void drawStringUnderlineCharAt(JComponent c, Graphics2D g, String string, int underlinedIndex,
                                       float x, float y);
        String getClippedString(JComponent c, FontMetrics fm, String string, int availTextWidth);
        float getStringWidth(JComponent c, FontMetrics fm, String string);
        void installAATextInfo(UIDefaults table);
        AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im);
        AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im1, BufferedImage im2);
        @NotNull AquaMultiResolutionImage createImage(int rasterWidth, int rasterHeight, int[] data, float scale);
        Image applyFilter(Image image, ImageFilter filter);
        Image applyMapper(Image source, Function<Image,Image> mapper);
        Image applyMapper(Image source, AquaMultiResolutionImage.Mapper mapper);
        @NotNull Image getResolutionVariant(@NotNull Image source, double width, double height);
        @NotNull BufferedImage createImage(int width, int height, int[] data);
        void preload(Image image, int availableInfo);
        void lockRenderQueue();
        void unlockRenderQueue();
        AquaPopupFactory createPopupFactory();
        int getFocusEventCause(@NotNull FocusEvent e);
    }

    private final static JavaSupportImpl impl = findImpl();

    public static int getScaleFactor(Graphics g)
    {
        return impl.getScaleFactor(g);
    }

    public static boolean hasOpaqueBeenExplicitlySet(JComponent c) {
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

    /**
     * Create an image from a scaled raster (created by native code).
     */
    public static @NotNull AquaMultiResolutionImage createImage(int rasterWidth, int rasterHeight, int[] data, float scale) {
        return impl.createImage(rasterWidth, rasterHeight, data, scale);
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

    public static @NotNull Image getResolutionVariant(@NotNull Image source, double width, double height) {
        return impl.getResolutionVariant(source, width, height);
    }

    /**
     * Create a buffered image from a raster (created by native code).
     */
    public static @NotNull BufferedImage createImage(int width, int height, int[] data) {
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

    public static int getFocusEventCause(@NotNull FocusEvent e) {
        return impl.getFocusEventCause(e);
    }

    private static JavaSupportImpl findImpl() {
        int version = Utils.getJavaVersion();
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
