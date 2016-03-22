/*
 * Changes copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.BufferedImage;
import javax.swing.*;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.UIResource;

import com.apple.eio.FileManager;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.jnr.Painter;

public class AquaIcon {
    interface InvertableIcon extends Icon {
        Icon getInvertedIcon();
    }

    public static Icon loadResource(String resource) {
      Image im = AquaImageFactory.loadResource(resource);
        return new ImageIcon(im);
    }

    public static final IconImageCreator alertCautionImageCreator = new IconImageCreator("caut", 64);
    public static final IconImageCreator alertStopImageCreator = new IconImageCreator("stop", 64);
    public static final IconImageCreator openFolderImageCreator = new IconImageCreator("ofld", 20);

    private static class RecyclableIconImageSingleton extends RecyclableSingleton<Image> {

        final IconImageCreator imageCreator;

        public RecyclableIconImageSingleton(IconImageCreator imageCreator) {
            this.imageCreator = imageCreator;
        }

        protected Image getInstance() {
            return imageCreator.getImage();
        }
    }

    private static final RecyclableIconImageSingleton cautionIcon = new RecyclableIconImageSingleton(alertCautionImageCreator);
    private static final RecyclableIconImageSingleton stopIcon = new RecyclableIconImageSingleton(alertStopImageCreator);
    private static final RecyclableIconImageSingleton openFolderIcon = new RecyclableIconImageSingleton(openFolderImageCreator);

    /**
     * Create an image for an icon, if possible.
     * @param i The icon (may be null).
     * @return the corresponding image, or null if not possible.
     */
    public static Image getImageForIcon(final Icon i) {
        if (i == null) {
            return null;
        }

        if (i instanceof ImageIcon) return ((ImageIcon)i).getImage();

        final int w = i.getIconWidth();
        final int h = i.getIconHeight();

        if (w <= 0 || h <= 0) return null;

        // This could be any kind of icon, so we need to make a buffer for it, draw it and then pass the new image off to appkit.
        final BufferedImage image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB_PRE);
        final Graphics g = image.getGraphics();
        i.paintIcon(null, g, 0, 0);
        g.dispose();
        return image;
    }

    public static Icon createPressedDarkIcon(Icon ic) {
        Image im = getImageForIcon(ic);
        if (im != null) {
            Image pressedImage = AquaImageFactory.generatePressedDarkImage(im);
            return new ImageIcon(pressedImage);
        } else {
            return ic;
        }
    }

    public static abstract class CachingScalingIcon implements Icon, UIResource {
        int width;
        int height;
        Image image;

        public CachingScalingIcon(final int width, final int height) {
            this.width = width;
            this.height = height;
        }

        void setSize(final int width, final int height) {
            this.width = width;
            this.height = height;
            this.image = null;
        }

        Image getImage() {
            if (image != null) return image;

            if (!GraphicsEnvironment.isHeadless()) {
                image = createImage();
            }

            return image;
        }

        abstract Image createImage();

        public boolean hasIconRef() {
            return getImage() != null;
        }

        public void paintIcon(final Component c, Graphics g, final int x, final int y) {
            g = g.create();

            if (g instanceof Graphics2D) {
                // improves icon rendering quality in Quartz
                ((Graphics2D)g).setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            }

            final Image myImage = getImage();
            if (myImage != null) {
                g.drawImage(myImage, x, y, getIconWidth(), getIconHeight(), null);
            }

            g.dispose();
        }

        public int getIconWidth() {
            return width;
        }

        public int getIconHeight() {
            return height;
        }
    }

    public static abstract class ScalingNativeRenderedIcon implements Icon, UIResource {
        final int width;
        final int height;

        public ScalingNativeRenderedIcon(final int width, final int height) {
            this.width = width;
            this.height = height;
        }

        @Override
        public void paintIcon(final Component c, Graphics g,
                final int x, final int y) {
            if (GraphicsEnvironment.isHeadless()) {
                return;
            }

            g = g.create();

            if (g instanceof Graphics2D) {
                // improves icon rendering quality in Quartz
                ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_RENDERING,
                        RenderingHints.VALUE_RENDER_QUALITY);
            }

            final org.violetlib.jnr.Painter painter = getPainter(width, height);

            g.clipRect(x, y, width, height);
            painter.paint(g, x, y);
            g.dispose();
        }

        public abstract Painter getPainter(int width, int height);

        @Override
        public int getIconWidth() {
            return width;
        }

        @Override
        public int getIconHeight() {
            return height;
        }
    }

    public static Image getStopIconImage() {
        return stopIcon.getInstance();
    }

    public static Image getCautionIconImage() {
        return cautionIcon.getInstance();
    }

    public static IconUIResource getOpenFolderIcon() {
        return new IconUIResource(new ImageIcon(openFolderIcon.getInstance()));
    }

    private static class IconImageCreator {
        private final String osTypeName;
        private final int osType;
        private final int size;
        private Image result;

        public IconImageCreator(String osTypeName, int size) {
            this.osTypeName = osTypeName;
            this.osType = FileManager.OSTypeToInt(osTypeName);
            this.size = size;
        }

        public Image getImage() {
            if (result == null) {
                AquaNativeSupport.load();
                int[][] buffers = new int[2][];
                if (!nativeRenderIcon(osType, buffers, size)) {
                    if (AquaImageFactory.debugNativeRendering) {
                        System.err.println("Failed to render image for icon " + osTypeName);
                    }
                    throw new UnsupportedOperationException();
                }

                if (AquaImageFactory.debugNativeRendering) {
                    System.err.println("Rendered image for icon " + osTypeName);
                }
                result = AquaMultiResolutionImage.createImage(size, size, buffers[0], buffers[1]);
            }
            return result;
        }
    }

    /**
     * Render a system icon.
     *
     * @param osType The OSType code that identifies the icon.
     * @param buffers 1x and 2x rasters are stored here (2x is optional)
     * @param size The width and height of the icon.
     * @return true if successful, false otherwise.
     */
    private static native boolean nativeRenderIcon(int osType, int[][] buffers, int size);
}
