/*
 * Copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2013, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import java.awt.image.*;
import java.lang.ref.SoftReference;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.WeakHashMap;
import java.util.function.Supplier;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.text.View;

import org.violetlib.aqua.AquaImageFactory.SlicedImageControl;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static javax.swing.SwingConstants.*;
import static javax.swing.SwingConstants.RIGHT;

final public class AquaUtils {

    private static final String ANIMATIONS_PROPERTY = "swing.enableAnimations";

    private static final int javaVersion = obtainJavaVersion();

    /**
     * Suppresses default constructor, ensuring non-instantiability.
     */
    private AquaUtils() {
    }

    public static int getJavaVersion() {
        return javaVersion;
    }

    private static int obtainJavaVersion()
    {
        String s = System.getProperty("java.version");
        if (s.startsWith("1.")) {
            s = s.substring(2);
        }
        int version = 0;
        int tokenCount = 0;
        StringTokenizer st = new StringTokenizer(s, "._");
        try {
            while (st.hasMoreTokens()) {
                String token = st.nextToken();
                if (token.endsWith("-internal")) {
                    token = token.substring(0, token.length() - 9);
                }
                int n = Integer.parseInt(token);
                ++tokenCount;
                int limit = tokenCount < 3 ? 100 : 1000;
                if (n < 0 || n >= limit) {
                    return 0;
                }
                version = version * limit + n;
            }
        } catch (NumberFormatException ex) {
            return 0;
        }

        while (tokenCount < 3) {
            ++tokenCount;
            int limit = tokenCount < 3 ? 100 : 1000;
            version = version * limit;
        }

        if (tokenCount != 3) {
            return 0;
        }
        return version;
    }

    /**
     * Return the UI of a component if it satisfies the specified class or interface.
     */
    public static <T> T getUI(JComponent c, Class<T> requestedClass) {
        try {
            final Class<? extends JComponent> clazz = c.getClass();
            final Method getUIMethod = clazz.getMethod("getUI");
            final Object ui = getUIMethod.invoke(c);
            return requestedClass.cast(ui);
        } catch (Throwable th) {
            return null;
        }
    }

    public static Insets combineAsInsets(Insetter s, Insets adjustments) {
        if (s != null) {
            Insets n = s.asInsets();
            return combineInsets(n, adjustments);
        } else {
            return adjustments != null ? adjustments : new Insets(0, 0, 0, 0);
        }
    }

    public static Insets2D combineAsInsets2D(Insetter s, Insets adjustments) {
        if (s != null) {
            Insets2D n = s.asInsets2D();
            return combineInsets(n, adjustments);
        } else {
            return adjustments != null ? asInsets2D(adjustments) : new Insets2D(0, 0, 0, 0);
        }
    }

    public static Insets combineInsets(Insets s1, Insets s2) {
        if (s1 != null) {
            if (s2 != null) {
                return new Insets(s1.top + s2.top, s1.left + s2.left, s1.bottom + s2.bottom, s1.right + s2.right);
            } else {
                return s1;
            }
        } else if (s2 != null) {
            return s2;
        } else {
            return new Insets(0, 0, 0, 0);
        }
    }

    public static Insets2D combineInsets(Insets2D s1, Insets s2) {
        if (s1 != null) {
            if (s2 != null) {
                return new Insets2D(s1.getTop() + s2.top, s1.getLeft() + s2.left, s1.getBottom() + s2.bottom, s1.getRight() + s2.right);
            } else {
                return s1;
            }
        } else if (s2 != null) {
            return asInsets2D(s2);
        } else {
            return new Insets2D(0, 0, 0, 0);
        }
    }

    public static Insets2D asInsets2D(Insets s) {
        return new Insets2D(s.top, s.left, s.bottom, s.right);
    }

    public static Rectangle toRectangle(Rectangle2D r) {
        if (r instanceof Rectangle) {
            return (Rectangle) r;
        }
        int x = (int) Math.floor(r.getX());
        int y = (int) Math.floor(r.getY());
        int width = (int) Math.ceil(r.getWidth());
        int height = (int) Math.ceil(r.getHeight());
        return new Rectangle(x, y, width, height);
    }

    public static Rectangle toMinimumRectangle(Rectangle2D r) {
        if (r instanceof Rectangle) {
            return (Rectangle) r;
        }

        double xx = r.getX();
        double ww = r.getWidth();
        int x = (int) Math.ceil(xx);
        ww -= (x - xx);
        int w = (int) Math.floor(ww);

        double yy = r.getY();
        double hh = r.getHeight();
        int y = (int) Math.ceil(yy);
        hh -= (y - yy);
        int h = (int) Math.floor(hh);

        return new Rectangle(x, y, w, h);
    }

    /**
     * Return the location of a component in screen coordinates.
     */
    public static Point getScreenLocation(Component c) {
        Point p = new Point(0, 0);
        SwingUtilities.convertPointToScreen(p, c);
        return p;
    }

    /**
     * Obtain the usable bounds of the screen at a given location.
     * This code taken from JPopupMenu.
     */
    public static Rectangle getScreenBounds(Point location, Component invoker) {
        Rectangle bounds;
        GraphicsConfiguration gc = getCurrentGraphicsConfiguration(location, invoker);
        Toolkit toolkit = Toolkit.getDefaultToolkit();
        if (gc != null) {
            // If we have GraphicsConfiguration use it to get screen bounds
            bounds = gc.getBounds();
        } else {
            // If we don't have GraphicsConfiguration use primary screen
            bounds = new Rectangle(toolkit.getScreenSize());
        }

        Insets insets = toolkit.getScreenInsets(gc);
        int top = insets.top / 2;
        int bottom = insets.bottom;
        int left = insets.left;
        int right = insets.right;
        return new Rectangle(bounds.x + left, bounds.y + top, bounds.width - left - right,
                bounds.height - top - bottom);
    }

    /**
     * Tries to find GraphicsConfiguration
     * that contains the mouse cursor position.
     * Can return null.
     */
    private static GraphicsConfiguration getCurrentGraphicsConfiguration(Point location, Component invoker) {
        GraphicsConfiguration gc = null;
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] gds = ge.getScreenDevices();
        for (GraphicsDevice gd : gds) {
            if (gd.getType() == GraphicsDevice.TYPE_RASTER_SCREEN) {
                GraphicsConfiguration dgc = gd.getDefaultConfiguration();
                if (dgc.getBounds().contains(location)) {
                    gc = dgc;
                    break;
                }
            }
        }
        // If not found and we have invoker, ask invoker about his gc
        if (gc == null && invoker != null) {
            gc = invoker.getGraphicsConfiguration();
        }
        return gc;
    }

    /**
     * Convenience method to get the root pane of a window.
     */
    public static JRootPane getRootPane(Window w) {
        if (w instanceof RootPaneContainer) {
            RootPaneContainer rpc = (RootPaneContainer) w;
            return rpc.getRootPane();
        }
        return null;
    }

    /**
     * Convenience function for determining ComponentOrientation.  Helps us
     * avoid having Munge directives throughout the code.
     */
    public static boolean isLeftToRight(Component c) {
        return c == null || c.getComponentOrientation().isLeftToRight();
    }

    public static AquaUIPainter.UILayoutDirection getLayoutDirection(Component c) {
        return c == null || c.getComponentOrientation().isLeftToRight() ? AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT : AquaUIPainter.UILayoutDirection.RIGHT_TO_LEFT;
    }

    static void enforceComponentOrientation(final Component c, final ComponentOrientation orientation) {
        c.setComponentOrientation(orientation);
        if (c instanceof Container) {
            for (final Component child : ((Container) c).getComponents()) {
                enforceComponentOrientation(child, orientation);
            }
        }
    }

    public static boolean isOnToolbar(JComponent b) {
        Component parent = b.getParent();
        while (parent != null) {
            if (parent instanceof JToolBar) {
                return true;
            }
            parent = parent.getParent();
        }
        return false;
    }

    public static boolean isFullScreenWindow(Window w) {
        GraphicsConfiguration gc = w.getGraphicsConfiguration();
        GraphicsDevice gd = gc.getDevice();
        Window fsw = gd.getFullScreenWindow();
        if (fsw != null) {
            return fsw == w;
        }
        // It seems that windows made full screen using a title bar button do not get registered as such with Java...
        return nativeIsFullScreenWindow(w);
    }

    // The following are copied from SwingUtililties, with modification.

    /**
     * Compute and return the location of the icons origin, the
     * location of origin of the text baseline, and a possibly clipped
     * version of the compound labels string.  Locations are computed
     * relative to the viewR rectangle.
     * The JComponents orientation (LEADING/TRAILING) will also be taken
     * into account and translated into LEFT/RIGHT values accordingly.
     *
     * @param c the component
     * @param fm the instance of {@code FontMetrics}
     * @param text the text
     * @param icon the icon
     * @param verticalAlignment the vertical alignment
     * @param horizontalAlignment the horizontal alignment
     * @param verticalTextPosition the vertical text position
     * @param horizontalTextPosition the horizontal text position
     * @param viewR the available rectangle
     * @param iconR the rectangle for the icon
     * @param textR the rectangle for the text
     * @param textIconGap the gap between text and icon
     *
     * @return the possibly clipped version of the compound labels string
     */
    public static String layoutCompoundLabel(JComponent c,
            FontMetrics fm,
            String text,
            Icon icon,
            int verticalAlignment,
            int horizontalAlignment,
            int verticalTextPosition,
            int horizontalTextPosition,
            Rectangle viewR,
            Rectangle iconR,
            Rectangle textR,
            int textIconGap)
    {
        boolean orientationIsLeftToRight = true;
        int hAlign = horizontalAlignment;
        int hTextPos = horizontalTextPosition;

        if (c != null) {
            if (!(c.getComponentOrientation().isLeftToRight())) {
                orientationIsLeftToRight = false;
            }
        }

        // Translate LEADING/TRAILING values in horizontalAlignment
        // to LEFT/RIGHT values depending on the components orientation
        switch (horizontalAlignment) {
            case LEADING:
                hAlign = (orientationIsLeftToRight) ? LEFT : RIGHT;
                break;
            case TRAILING:
                hAlign = (orientationIsLeftToRight) ? RIGHT : LEFT;
                break;
        }

        // Translate LEADING/TRAILING values in horizontalTextPosition
        // to LEFT/RIGHT values depending on the components orientation
        switch (horizontalTextPosition) {
            case LEADING:
                hTextPos = (orientationIsLeftToRight) ? LEFT : RIGHT;
                break;
            case TRAILING:
                hTextPos = (orientationIsLeftToRight) ? RIGHT : LEFT;
                break;
        }

        return layoutCompoundLabelImpl(c,
                fm,
                text,
                icon,
                verticalAlignment,
                hAlign,
                verticalTextPosition,
                hTextPos,
                viewR,
                iconR,
                textR,
                textIconGap);
    }

    /**
     * Compute and return the location of the icons origin, the
     * location of origin of the text baseline, and a possibly clipped
     * version of the compound labels string.  Locations are computed
     * relative to the viewR rectangle.
     * This layoutCompoundLabel() does not know how to handle LEADING/TRAILING
     * values in horizontalTextPosition (they will default to RIGHT) and in
     * horizontalAlignment (they will default to CENTER).
     * Use the other version of layoutCompoundLabel() instead.
     *
     * This is the same as SwingUtilities.layoutCompoundLabelImpl, except for
     * the algorithm for clipping the text. If a text is too long, "..." are
     * inserted at the middle of the text instead of at the end.
     */
    private static String layoutCompoundLabelImpl(
            JComponent c,
            FontMetrics fm,
            String text,
            Icon icon,
            int verticalAlignment,
            int horizontalAlignment,
            int verticalTextPosition,
            int horizontalTextPosition,
            Rectangle viewR,
            Rectangle iconR,
            Rectangle textR,
            int textIconGap)
    {
        /* Initialize the icon bounds rectangle iconR.
         */

        if (icon != null) {
            iconR.width = icon.getIconWidth();
            iconR.height = icon.getIconHeight();
        } else {
            iconR.width = iconR.height = 0;
        }

        /* Initialize the text bounds rectangle textR.  If a null
         * or and empty String was specified we substitute "" here
         * and use 0,0,0,0 for textR.
         */

        boolean textIsEmpty = (text == null) || text.equals("");
        int lsb = 0;

        View v = null;
        if (textIsEmpty) {
            textR.width = textR.height = 0;
            text = "";
        } else {
            v = (c != null) ? (View) c.getClientProperty("html") : null;
            if (v != null) {
                textR.width = (int) v.getPreferredSpan(View.X_AXIS);
                textR.height = (int) v.getPreferredSpan(View.Y_AXIS);
            } else {
                textR.width = SwingUtilities.computeStringWidth(fm, text);

                lsb = getLeftSideBearing(c, fm, text);
                if (lsb < 0) {
                    // If lsb is negative, add it to the width, the
                    // text bounds will later be adjusted accordingly.
                    textR.width -= lsb;
                }
                textR.height = fm.getHeight();
            }
        }

        /* Unless both text and icon are non-null, we effectively ignore
         * the value of textIconGap.  The code that follows uses the
         * value of gap instead of textIconGap.
         */

        int gap = (textIsEmpty || (icon == null)) ? 0 : textIconGap;

        if (!textIsEmpty) {

            /* If the label text string is too wide to fit within the available
             * space "..." and as many characters as will fit will be
             * displayed instead.
             */

            int availTextWidth;

            if (horizontalTextPosition == CENTER) {
                availTextWidth = viewR.width;
            } else {
                availTextWidth = viewR.width - (iconR.width + gap);
            }

            if (textR.width > availTextWidth) {
                if (v != null) {
                    textR.width = availTextWidth;
                } else {
                    String clipString = "...";
                    int totalWidth = SwingUtilities.computeStringWidth(fm, clipString);
                    int nChars;
                    int len = text.length();
                    for (nChars = 0; nChars < len; nChars++) {
                        int charIndex = (nChars % 2 == 0) ? nChars / 2 : len - 1 - nChars / 2;
                        totalWidth += fm.charWidth(text.charAt(charIndex));
                        if (totalWidth > availTextWidth) {
                            break;
                        }
                    }
                    text = text.substring(0, nChars / 2) + clipString + text.substring(len - nChars / 2);
                    textR.width = SwingUtilities.computeStringWidth(fm, text);
                }
            }
        }


        /* Compute textR.x,y given the verticalTextPosition and
         * horizontalTextPosition properties
         */

        if (verticalTextPosition == TOP) {
            if (horizontalTextPosition != CENTER) {
                textR.y = 0;
            } else {
                textR.y = -(textR.height + gap);
            }
        } else if (verticalTextPosition == CENTER) {
            textR.y = (iconR.height / 2) - (textR.height / 2);
        } else { // (verticalTextPosition == BOTTOM)

            if (horizontalTextPosition != CENTER) {
                textR.y = iconR.height - textR.height;
            } else {
                textR.y = (iconR.height + gap);
            }
        }

        if (horizontalTextPosition == LEFT) {
            textR.x = -(textR.width + gap);
        } else if (horizontalTextPosition == CENTER) {
            textR.x = (iconR.width / 2) - (textR.width / 2);
        } else { // (horizontalTextPosition == RIGHT)

            textR.x = (iconR.width + gap);
        }

        /* labelR is the rectangle that contains iconR and textR.
         * Move it to its proper position given the labelAlignment
         * properties.
         *
         * To avoid actually allocating a Rectangle, Rectangle.union
         * has been inlined below.
         */
        int labelR_x = Math.min(iconR.x, textR.x);
        int labelR_width = Math.max(iconR.x + iconR.width,
                textR.x + textR.width) - labelR_x;
        int labelR_y = Math.min(iconR.y, textR.y);
        int labelR_height = Math.max(iconR.y + iconR.height,
                textR.y + textR.height) - labelR_y;

        int dx, dy;

        if (verticalAlignment == TOP) {
            dy = viewR.y - labelR_y;
        } else if (verticalAlignment == CENTER) {
            dy = (viewR.y + (viewR.height / 2)) - (labelR_y + (labelR_height / 2));
        } else { // (verticalAlignment == BOTTOM)

            dy = (viewR.y + viewR.height) - (labelR_y + labelR_height);
        }

        if (horizontalAlignment == LEFT) {
            dx = viewR.x - labelR_x;
        } else if (horizontalAlignment == RIGHT) {
            dx = (viewR.x + viewR.width) - (labelR_x + labelR_width);
        } else { // (horizontalAlignment == CENTER)

            dx = (viewR.x + (viewR.width / 2))
                    - (labelR_x + (labelR_width / 2));
        }

        /* Translate textR and glypyR by dx,dy.
         */

        textR.x += dx;
        textR.y += dy;

        iconR.x += dx;
        iconR.y += dy;

        if (lsb < 0) {
            // lsb is negative. We previously adjusted the bounds by lsb,
            // we now need to shift the x location so that the text is
            // drawn at the right location. The result is textR does not
            // line up with the actual bounds (on the left side), but we will
            // have provided enough space for the text.
            textR.width += lsb;
            textR.x -= lsb;
        }

        return text;
    }

    private static int getLeftSideBearing(JComponent c, FontMetrics fm, String string) {
        if ((string == null) || (string.length() == 0)) {
            return 0;
        }
        return nativeGetLeftSideBearing(c, fm, string.charAt(0));
    }

    public static void paintImmediately(Window w, JComponent c) {
        // a possible workaround... the goal is to paint to the AWT view before the window becomes visible
        // Note that the paintImmediately() method does nothing if it believes that the component is not visible.
        nativeSetWindowVisibleField(w, true);
        c.paintImmediately(0, 0, c.getWidth(), c.getHeight());
        nativeSetWindowVisibleField(w, false);
    }

    abstract static class RecyclableObject<T> {
        private SoftReference<T> objectRef;

        T get() {
            T referent;
            if (objectRef != null && (referent = objectRef.get()) != null) return referent;
            referent = create();
            objectRef = new SoftReference<T>(referent);
            return referent;
        }

        protected abstract T create();
    }

    public abstract static class RecyclableSingleton<T> {
        final T get() {
            return getSoftReferenceValue(this, () -> getInstance());
        }

        void reset() {
            put(this, null);
        }

        protected abstract T getInstance();
    }

    static class RecyclableSingletonFromDefaultConstructor<T> extends RecyclableSingleton<T> {
        private final Class<T> clazz;

        RecyclableSingletonFromDefaultConstructor(final Class<T> clazz) {
            this.clazz = clazz;
        }

        @Override
        protected T getInstance() {
            try {
                //ReflectUtil.checkPackageAccess(clazz);
                return clazz.newInstance();
            } catch (InstantiationException | IllegalAccessException ignored) {
            }
            return null;
        }
    }

    abstract static class LazyKeyedSingleton<K, V> {
        private Map<K, V> refs;

        V get(final K key) {
            if (refs == null) refs = new HashMap<>();

            final V cachedValue = refs.get(key);
            if (cachedValue != null) return cachedValue;

            final V value = getInstance(key);
            refs.put(key, value);
            return value;
        }

        protected abstract V getInstance(K key);
    }

    private static final RecyclableSingleton<Boolean> enableAnimations = new RecyclableSingleton<Boolean>() {
        @Override
        protected Boolean getInstance() {
            final String sizeProperty = (String) AccessController.doPrivileged((PrivilegedAction<String>) () -> System.getProperty(
                    ANIMATIONS_PROPERTY));
            return !"false".equals(sizeProperty); // should be true by default
        }
    };

    private static boolean animationsEnabled() {
        return enableAnimations.get();
    }

    private static final int MENU_BLINK_DELAY = 50; // 50ms == 3/60 sec, according to the spec

    static void blinkMenu(final Selectable selectable) {
        if (!animationsEnabled()) return;
        try {
            selectable.paintSelected(false);
            Thread.sleep(MENU_BLINK_DELAY);
            selectable.paintSelected(true);
            Thread.sleep(MENU_BLINK_DELAY);
        } catch (final InterruptedException ignored) {
        }
    }

    interface Selectable {
        void paintSelected(boolean selected);
    }

    interface JComponentPainter {
        void paint(JComponent c, Graphics g, int x, int y, int w, int h);
    }

    interface Painter {
        void paint(Graphics g, int x, int y, int w, int h);
    }

    static class ShadowBorder implements Border {
        private final Painter prePainter;
        private final Painter postPainter;

        private final int offsetX;
        private final int offsetY;
        private final float distance;
        private final int blur;
        private final Insets insets;
        private final ConvolveOp blurOp;

        ShadowBorder(final Painter prePainter, final Painter postPainter, final int offsetX, final int offsetY, final float distance, final float intensity, final int blur) {
            this.prePainter = prePainter;
            this.postPainter = postPainter;
            this.offsetX = offsetX;
            this.offsetY = offsetY;
            this.distance = distance;
            this.blur = blur;
            final int halfBlur = blur / 2;
            insets = new Insets(halfBlur - offsetY, halfBlur - offsetX, halfBlur + offsetY, halfBlur + offsetX);

            final float blurry = intensity / (blur * blur);
            final float[] blurKernel = new float[blur * blur];
            for (int i = 0; i < blurKernel.length; i++) blurKernel[i] = blurry;
            blurOp = new ConvolveOp(new Kernel(blur, blur, blurKernel));
        }

        @Override
        public final boolean isBorderOpaque() {
            return false;
        }

        @Override
        public final Insets getBorderInsets(final Component c) {
            return insets;
        }

        @Override
        public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width, final int height) {
            final BufferedImage img = new BufferedImage(width + blur * 2, height + blur * 2, BufferedImage.TYPE_INT_ARGB_PRE);
            paintToImage(img, x, y, width, height);
//            debugFrame("border", img);
            g.drawImage(img, -blur, -blur, null);
        }

        private void paintToImage(final BufferedImage img, final int x, final int y, final int width, final int height) {
            // clear the prior image
            Graphics2D imgG = (Graphics2D) img.getGraphics();
            imgG.setComposite(AlphaComposite.Clear);
            imgG.setColor(Color.black);
            imgG.fillRect(0, 0, width + blur * 2, height + blur * 2);

            final int adjX = (int) (x + blur + offsetX + (insets.left * distance));
            final int adjY = (int) (y + blur + offsetY + (insets.top * distance));
            final int adjW = (int) (width - (insets.left + insets.right) * distance);
            final int adjH = (int) (height - (insets.top + insets.bottom) * distance);

            // let the delegate paint whatever they want to be blurred
            imgG.setComposite(AlphaComposite.DstAtop);
            if (prePainter != null) prePainter.paint(imgG, adjX, adjY, adjW, adjH);
            imgG.dispose();

            // blur the prior image back into the same pixels
            imgG = (Graphics2D) img.getGraphics();
            imgG.setComposite(AlphaComposite.DstAtop);
            imgG.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            imgG.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
            imgG.drawImage(img, blurOp, 0, 0);

            if (postPainter != null) postPainter.paint(imgG, adjX, adjY, adjW, adjH);
            imgG.dispose();
        }
    }

    static class SlicedShadowBorder extends ShadowBorder {
        private final SlicedImageControl slices;

        SlicedShadowBorder(final Painter prePainter, final Painter postPainter, final int offsetX, final int offsetY, final float distance, final float intensity, final int blur, final int templateWidth, final int templateHeight, final int leftCut, final int topCut, final int rightCut, final int bottomCut) {
            super(prePainter, postPainter, offsetX, offsetY, distance, intensity, blur);

            final BufferedImage i = new BufferedImage(templateWidth, templateHeight, BufferedImage.TYPE_INT_ARGB_PRE);
            super.paintBorder(null, i.getGraphics(), 0, 0, templateWidth, templateHeight);
//            debugFrame("slices", i);
            slices = new SlicedImageControl(i, leftCut, topCut, rightCut, bottomCut, false);
        }

        @Override
        public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width, final int height) {
            slices.paint(g, x, y, width, height);
        }
    }

    private static final RecyclableSingleton<Method> getJComponentGetFlagMethod = new RecyclableSingleton<Method>() {
        @Override
        protected Method getInstance() {
            return AccessController.doPrivileged(
                    new PrivilegedAction<Method>() {
                        @Override
                        public Method run() {
                            try {
                                final Method method = JComponent.class.getDeclaredMethod(
                                        "getFlag", new Class<?>[]{int.class});
                                method.setAccessible(true);
                                return method;
                            } catch (final Throwable ignored) {
                                return null;
                            }
                        }
                    }
            );
        }
    };

    private static final Integer OPAQUE_SET_FLAG = 24; // private int JComponent.OPAQUE_SET

    static boolean hasOpaqueBeenExplicitlySet(final JComponent c) {
        final Method method = getJComponentGetFlagMethod.get();
        if (method == null) return false;
        try {
            return Boolean.TRUE.equals(method.invoke(c, OPAQUE_SET_FLAG));
        } catch (final Throwable ignored) {
            return false;
        }
    }

    // options for when to use a magic eraser
    public final static int ERASE_IF_TEXTURED = 1<<0;
    public final static int ERASE_IF_VIBRANT = 1<<1;
    public final static int ERASE_ALWLAYS = 1<<2;

    /**
     * Fill the component bounds with the appropriate fill color or magic eraser.
     * @param c The component.
     */
    public static void fillRect(Graphics g, Component c, int eraserMode) {
        fillRect(g, c, eraserMode, 0, 0, c.getWidth(), c.getHeight());
    }

    /**
     * Fill the specified rectangle with the appropriate fill color or magic eraser.
     * @param c The component.
     */
    public static void fillRect(Graphics g, Component c, int eraserMode, int x, int y, int w, int h) {
        Color color = getFillColor(c, eraserMode);
        fillRect(g, color, x, y, w, h);
    }

    /**
     * Determine the fill color to use for a component.
     * @param c The component.
     * @return the fill color, or null to use the magic eraser.
     */
    private static Color getFillColor(Component c, int eraserMode) {
        if ((eraserMode & ERASE_ALWLAYS) != 0) {
            return null;
        }

        Color bc = c.getBackground();
        if (bc != null && !(bc instanceof UIResource)) {
            return bc;
        }

        return !isMagicEraser(c, eraserMode) ? bc : null;
    }

    /**
     * Determine whether a component should use a magic eraser instead of painting a background.
     * A magic eraser erases the current contents of the frame buffer so that the native window or vibrant view
     * background shows through.
     * @param c The component.
     * @param eraserMode The eraser mode, which selects the features that are tested for.
     * @return true if the component should use a magic eraser.
     */
    private static boolean isMagicEraser(Component c, int eraserMode) {

        boolean isTextured = (eraserMode & ERASE_IF_TEXTURED) != 0;
        boolean isVibrant = (eraserMode & ERASE_IF_VIBRANT) != 0;

        while (c != null) {
            if (c instanceof JRootPane) {
                JRootPane rp = (JRootPane) c;

                Object prop = rp.getClientProperty("apple.awt.brushMetalLook");
                if (prop != null && isTextured) {
                    if (Boolean.parseBoolean(prop.toString())) {
                        return true;
                    }
                }

                prop = rp.getClientProperty("Window.style");
                if (prop != null) {
                    if (prop.equals("textured") && isTextured) {
                        return true;
                    }
                }
            }

            if (c instanceof JComponent) {
                JComponent jc = (JComponent) c;
                if (isVibrant && AquaVibrantSupport.isVibrant(jc)) {
                    return true;
                }
            }

            c = c.getParent();
        }

        return false;
    }

    /**
     * Fill with specified color or erase.
     * @param g The graphics context.
     * @param color The color to fill, or null to erase
     */
    public static void fillRect(Graphics g, Color color, int x, int y, int w, int h) {
        final Graphics cg = g.create();
        try {
            if (color != null) {
                cg.setColor(color);
                cg.fillRect(x, y, w, h);
            } else if (cg instanceof Graphics2D) {
                ((Graphics2D) cg).setComposite(AlphaComposite.Src);
                cg.setColor(new Color(0, 0, 0, 0));
                cg.fillRect(x, y, w, h);
            }
        } finally {
            cg.dispose();
        }
    }

    public static Graphics2D toGraphics2D(Graphics g) {
        try {
            return (Graphics2D) g;
        } catch (ClassCastException ex) {
            return null;
        }
    }

    static final Map<Object, Object> appContextMap = new HashMap<Object, Object>();

    public static boolean isProperty(String s, String... props) {
        if (s != null) {
            for (String prop : props) {
                if (prop.equals(s)) {
                    return true;
                }
            }
        }
        return false;
    }

    public static String getProperty(JComponent c, String... props) {
        for (String prop : props) {
            Object o = c.getClientProperty(prop);
            if (o != null) {
                return toString(o);
            }
        }
        return null;
    }

    public static String toString(Object o) {
        if (o instanceof String) {
            return (String) o;
        }
        return null;
    }

    public static Boolean getBooleanProperty(JComponent c, String... props) {
        for (String prop : props) {
            Object o = c.getClientProperty(prop);
            if (o != null) {
                return toBoolean(o);
            }
        }
        return null;
    }

    public static Boolean toBoolean(Object o) {
        if (o instanceof Boolean) {
            return (Boolean) o;
        }
        return null;
    }

    public static Integer getIntegerProperty(JComponent c, String... props) {
        for (String prop : props) {
            Object o = c.getClientProperty(prop);
            if (o instanceof String) {
                String s = (String) o;
                try {
                    return Integer.parseInt(s);
                } catch (NumberFormatException ex) {
                }
            } else if (o instanceof Integer) {
                return (Integer) o;
            }
        }
        return null;
    }

    public static String getProperty(String key) {
        try {
            return System.getProperty(key);
        } catch (SecurityException ex) {
            return null;
        }
    }

    public static String getProperty(String key, String def) {
        try {
            return System.getProperty(key, def);
        } catch (SecurityException ex) {
            return def;
        }
    }

    /*
     * For use outside of the JDK in place of AppContext.get()
     */

    public static Object get(Object key) {
        synchronized (appContextMap) {
            return appContextMap.get(key);
        }
    }

    public static Object put(Object key, Object value) {
        synchronized (appContextMap) {
            return appContextMap.put(key, value);
        }
    }

    public static <T> T getSoftReferenceValue(Object key, Supplier<T> supplier) {
        @SuppressWarnings("unchecked")
        SoftReference<T> ref = (SoftReference<T>) get(key);
        if (ref != null) {
            final T object = ref.get();
            if (object != null) {
                return object;
            }
        }
        final T object = supplier.get();
        ref = new SoftReference<>(object);
        put(key, ref);
        return object;
    }

    /** Turns on common rendering hints for UI delegates. */
    public static Object beginGraphics(Graphics2D graphics2d) {
        Object object = graphics2d.getRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING);
        graphics2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        graphics2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        return object;
    }

    /** Restores rendering hints for UI delegates. */
    public static void endGraphics(Graphics2D graphics2d, Object oldHints) {
        if (oldHints != null) {
            graphics2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                    oldHints);
        }
    }

    private static final WeakHashMap<Graphics,Integer> scaleMap = new WeakHashMap<>();

    public static void drawHLine(Graphics g, int x1, int x2, int y) {
         if (x2 < x1) {
             final int temp = x2;
             x2 = x1;
             x1 = temp;
         }
         g.fillRect(x1, y, x2 - x1 + 1, 1);
     }

    @SuppressWarnings("deprecation")
    public static FontMetrics getFontMetrics(JComponent c, Graphics g, Font font) {
        if (c != null) {
            // Note: We assume that we're using the FontMetrics
            // from the widget to layout out text, otherwise we can get
            // mismatches when printing.
            return c.getFontMetrics(font);
        }
        return Toolkit.getDefaultToolkit().getFontMetrics(font);
    }

    /**
     * Return the scroll pane ancestor of a scroll pane view. This method supports the workaround that inserts a
     * container between the scroll pane and the viewport.
     * @param c The scroll pane view.
     * @return the scroll pane or null.
     */
    public static JScrollPane getScrollPaneAncestor(Component c) {
        Container p = c.getParent();
        if (p instanceof JViewport) {
            JViewport v = (JViewport) p;
            p = v.getParent();
            if (p instanceof JScrollPane) {
                return verifyScrollPaneAncestor(c, (JScrollPane) p);
            }
            if (p instanceof JComponent && !((JComponent) p).isOptimizedDrawingEnabled()) {
                p = p.getParent();
                if (p instanceof JScrollPane) {
                    return verifyScrollPaneAncestor(c, (JScrollPane) p);
                }
            }
        }
        return null;
    }

    private static JScrollPane verifyScrollPaneAncestor(Component view, JScrollPane p) {
        // Make certain we are the viewPort's view and not, for
        // example, the rowHeaderView of the scrollPane -
        // an implementor of fixed columns might do this.
        JViewport v = p.getViewport();
        return v != null && v.getView() == view ? p : null;
    }

    public final static int TITLE_BAR_NONE = 0;
    public final static int TITLE_BAR_ORDINARY = 1;
    public final static int TITLE_BAR_TRANSPARENT = 2;
    public final static int TITLE_BAR_HIDDEN = 3;
    public final static int TITLE_BAR_OVERLAY = 4;

    /**
     * Set the title bar style for a window.
     * <p>
     * Supported styles:
     * <ul>
     *     <li>TITLE_BAR_NONE - The window has no title bar.</li>
     *     <li>TITLE_BAR_ORDINARY - The window has an ordinary title bar.</li>
     *     <li>TITLE_BAR_TRANSPARENT - The window has a title bar with a transparent background. The content view
     *      occupies the entire window frame, including under the title bar.</li>
     *     <li>TITLE_BAR_HIDDEN - The window has a title bar, but it is fully transparent. The content view
     *      occupies the entire window frame. Unlike having no title bar, this option creates a window with rounded
     *      corners. The window title will be set to the empty string.</li>
     *      <li>TITLE_BAR_OVERLAY - The window has a normal title bar. The context view occupies the entire window
     *      frame.</li>
     * </ul>
     * <p>
     * When the title bar is transparent or hidden, it is the responsibility of the application to implement window
     * dragging.
     * <p>
     * Transparent and hidden title bars are incorrectly implemented on OS X 10.10 (Yosemite). If the window is
     * textured, a solid background is painted instead of the expected gradient. Window dragging by the title bar
     * is implemented natively (the Java program will not receive events). For this reason, on OS X 10.10, Java
     * controls should not be located in the title bar area.
     *
     * @param w The window.
     * @param style The title bar style.
     * @throws UnsupportedOperationException if the title bar style could not be changed.
     */
    public static void setTitleBarStyle(Window w, int style) {
        ensureWindowPeer(w);
        int result = nativeSetTitleBarStyle(w, style);
        if (result != 0) {
            throw new UnsupportedOperationException("Unable to set window title bar style");
        } else if (style == TITLE_BAR_HIDDEN) {
            setWindowTitle(w, "");
        }
    }

    public static void setWindowTitle(Window w, String title) {
        if (w instanceof Frame) {
            ((Frame) w).setTitle(title);
        } else if (w instanceof Dialog) {
            ((Dialog) w).setTitle(title);
        }
    }

    public static void addNativeToolbarToWindow(Window w) throws UnsupportedOperationException {
        ensureWindowPeer(w);
        int result = nativeAddToolbarToWindow(w);
        if (result != 0) {
            throw new UnsupportedOperationException("Unable to add native toolbar to window");
        }
    }

    /**
     * Ensure that the window peer has been created, as a prerequisite for calling native code that operates on the
     * native window.
     * @param w The window.
     */
    public static void ensureWindowPeer(Window w) {
        if (!w.isDisplayable()) {
            w.setSize(w.getPreferredSize());
            w.addNotify();
        }
    }

    /**
     * Make a titled window untitled. This method is used when displaying a window as a sheet.
     * @param w The window.
     * @throws UnsupportedOperationException on error.
     */
    public static void unsetTitledWindowStyle(Window w) throws UnsupportedOperationException {
        Rectangle oldBounds = w.getBounds();
        Insets oldInsets = w.getInsets();
        int top = oldInsets.top;

        if (top == 0) {
            // This method should be called only on a decorated window.
            throw new UnsupportedOperationException("Unable to unset titled window style: no top inset");
        }

        try {
            int DECORATED = 1 << 1;
            int newHeight = oldBounds.height - top;
            nativeSetTitledWindowStyle(w, false, newHeight, new Insets(0, 0, 0, 0));
            w.invalidate();
            w.validate();
        } catch (Exception ex) {
            String errorMessage = ex.toString();
            throw new UnsupportedOperationException(errorMessage);
        }
    }

    public static void restoreTitledWindowStyle(Window w, int top) {
        Rectangle oldBounds = w.getBounds();
        int newHeight = oldBounds.height + top;

        try {
            int DECORATED = 1 << 1;
            nativeSetTitledWindowStyle(w, true, newHeight, new Insets(top, 0, 0, 0));
            w.invalidate();
            w.validate();
        } catch (Exception ex) {
            System.err.println("Unable to restore titled window style: " + ex);
        }
    }

    /**
     * Return the object representing a custom styled window.
     * @param w The window.
     * @return the custom styled window object for {@code w}, or null if {@code w} does not use one of our custom window
     * styles.
     */
    public static AquaCustomStyledWindow getCustomStyledWindow(Window w) {
        JRootPane rp = getRootPane(w);
        if (rp != null) {
            AquaRootPaneUI ui = getUI(rp, AquaRootPaneUI.class);
            if (ui != null) {
                return ui.getCustomStyledWindow();
            }
        }
        return null;
    }

    /**
     * Enable or disable a clear window background. This method alters the background of the AWTView. A clear background
     * allows the native window background and NSVisualEffectViews behind the AWTView to be visible.
     * @param w The window
     * @param isClear True to make the window background clear, false to restore the default window background color.
     */
    public static void setWindowBackgroundClear(Window w, boolean isClear) {

        Color c = isClear ? new Color(0, 0, 0, 0) : AquaImageFactory.getWindowBackgroundColorUIResource();

        // The following may be necessary to properly calculate the window shadow.
        setWindowTextured(w, isClear);
        setWindowBackground(w, c);

        new ShadowMaker(w);
    }

    /**
     * I have not found a reliable way to ensure that enough opaque pixels are present to allow AppKit to compute
     * the window shadow for a vibrant popup. This class is a workaround for that problem.
     *
     * See bug JDK-7124236.
     */
    private static class ShadowMaker implements ActionListener, Runnable {
        private final Window w;

        public ShadowMaker(Window w) {
            this.w = w;
            SwingUtilities.invokeLater(this);
            Timer t = new Timer(100, this);
            t.setRepeats(false);
            t.start();
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            run();
        }

        @Override
        public void run() {
            if (w instanceof RootPaneContainer) {
                AquaUtils.syncAWTView(w);
                JRootPane rp = ((RootPaneContainer) w).getRootPane();
                rp.putClientProperty("apple.awt.windowShadow.revalidateNow", Math.random());
            }
        }
    }

    /**
     * Set the textured attribute of the window peer. This method has no other side effects.
     * @param w The window.
     * @param isTextured The new value of the textured attribute.
     */
    private static void setWindowTextured(Window w, boolean isTextured) {

        try {
            nativeSetWindowTextured(w, isTextured);
        } catch (Throwable ex) {
            System.err.println("Unable to set textured: " + ex);
        }
    }

    /**
     * Set the background color of a window. Unlike the public methods for setting the window background, this method
     * does not reject setting a transparent or translucent background on a decorated window. This method may not work
     * if the window does not have a peer.
     * @param w The window.
     * @param c The color.
     */
    public static void setWindowBackground(Window w, Color c) {

        if (c.equals(w.getBackground())) {
            return;
        }

        // The following lock is an attempt to work around bug JDK-8046290, which causes the transient display of
        // garbage pixels.
        JavaSupport.lockRenderQueue();

        try {
            // The following is possible only on undecorated windows
            w.setBackground(c);
        } catch (Throwable e) {
            try {
                nativeSetWindowBackground(w, c);
            } catch (Throwable th) {
                System.err.println("Unable to set window background: " + th);
            }
        } finally {
            JavaSupport.unlockRenderQueue();
        }
    }

    public static void setCornerRadius(Window w, float radius) {
        nativeSetWindowCornerRadius(w, radius);
    }

    // for debugging
    public static void setAWTViewVisibility(Window w, boolean isVisible) {
        nativeSetAWTViewVisibility(w, isVisible);
    }

    public static void syncAWTView(Window w) {
        // Both calls appear to be necessary to ensure that the pixels are ready when the window is made visible.
        Toolkit.getDefaultToolkit().sync();
        nativeSyncAWTView(w);
    }

    private static native void nativeSetTitledWindowStyle(Window w, boolean isDecorated, int height, Insets insets);
    private static native void nativeSetWindowTextured(Window w, boolean isTextured);
    private static native void nativeSetWindowBackground(Window w, Color color);
    private static native boolean nativeIsFullScreenWindow(Window w);
    private static native int nativeSetTitleBarStyle(Window w, int style);
    private static native int nativeAddToolbarToWindow(Window w);
    private static native int nativeSetWindowCornerRadius(Window w, float radius);
    private static native void nativeSetAWTViewVisibility(Window w, boolean isVisible);
    private static native void nativeSyncAWTView(Window w);
    private static native int nativeGetLeftSideBearing(JComponent c, FontMetrics fm, char firstChar);
    public static native void nativeInstallAATextInfo(UIDefaults table);
    private static native void nativeSetWindowVisibleField(Window w, boolean isVisible);

    public static native void debugWindow(Window w);
    public static native void syslog(String msg);

    public static class GrayUIResource extends ColorUIResource {
        public GrayUIResource(int c) {
            super(new Color(c, c, c));
        }
    }
}
