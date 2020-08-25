/*
 * Copyright (c) 2015 Alan Snyder.
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

import org.violetlib.aqua.AquaImageFactory.SlicedImageControl;
import org.violetlib.aqua.fc.AbstractFileChooserBrowserListUI;
import org.violetlib.jnr.aqua.AquaUIPainter;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Rectangle2D;
import java.awt.image.*;
import java.lang.ref.SoftReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.function.Supplier;

final public class AquaUtils extends SwingUtilitiesModified {

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
        int version = 0;
        int tokenCount = 0;
        StringTokenizer st = new StringTokenizer(s, "._");
        try {
            while (st.hasMoreTokens()) {
                String token = st.nextToken();
                int n = Integer.parseInt(token);
                ++tokenCount;
                int limit = tokenCount < 4 ? 100 : 1000;
                if (n < 0 || n >= limit) {
                    return 0;
                }
                version = version * limit + n;
            }
        } catch (NumberFormatException ex) {
            return 0;
        }
        if (tokenCount != 4) {
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

    static Image generateSelectedDarkImage(final Image image) {
        final ImageProducer prod = new FilteredImageSource(image.getSource(), new IconImageFilter() {
            @Override
            int getGreyFor(final int gray) {
                return gray * 75 / 100;
            }
        });
        return Toolkit.getDefaultToolkit().createImage(prod);
    }

    static Image generateDisabledImage(final Image image) {
        final ImageProducer prod = new FilteredImageSource(image.getSource(), new IconImageFilter() {
            @Override
            int getGreyFor(final int gray) {
                return 255 - ((255 - gray) * 65 / 100);
            }
        });
        return Toolkit.getDefaultToolkit().createImage(prod);
    }

    static Image generateLightenedImage(final Image image, final int percent) {
        final GrayFilter filter = new GrayFilter(true, percent);
        return AquaMultiResolutionImage.apply(image, filter);
    }

    private abstract static class IconImageFilter extends RGBImageFilter {
        IconImageFilter() {
            super();
            canFilterIndexColorModel = true;
        }

        @Override
        public final int filterRGB(final int x, final int y, final int rgb) {
            final int red = (rgb >> 16) & 0xff;
            final int green = (rgb >> 8) & 0xff;
            final int blue = rgb & 0xff;
            final int gray = getGreyFor((int) ((0.30 * red + 0.59 * green + 0.11 * blue) / 3));

            return (rgb & 0xff000000) | (grayTransform(red, gray) << 16) | (grayTransform(green, gray) << 8) | (grayTransform(blue, gray) << 0);
        }

        private static int grayTransform(final int color, final int gray) {
            int result = color - gray;
            if (result < 0) result = 0;
            if (result > 255) result = 255;
            return result;
        }

        abstract int getGreyFor(int gray);
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

    public static boolean isWindowTextured(final Component c) {
        if (!(c instanceof JComponent)) {
            return false;
        }
        final JRootPane pane = ((JComponent) c).getRootPane();
        if (pane == null) {
            return false;
        }

        Object prop = pane.getClientProperty("apple.awt.brushMetalLook");
        if (prop != null) {
            return Boolean.parseBoolean(prop.toString());
        }
        prop = pane.getClientProperty("Window.style");
        return prop != null && "textured".equals(prop);
    }

    private static Color resetAlpha(final Color color) {
        return new Color(color.getRed(), color.getGreen(), color.getBlue(), 0);
    }

    static void fillRect(final Graphics g, final Component c) {
        fillRect(g, c, c.getBackground(), 0, 0, c.getWidth(), c.getHeight());
    }

    static void fillRect(final Graphics g, final Component c, final Color color,
                         final int x, final int y, final int w, final int h) {
        if (!(g instanceof Graphics2D)) {
            return;
        }

        final Graphics2D cg = (Graphics2D) g.create();
        try {
            if (color instanceof UIResource
                    && isWindowTextured(c)
                    && color.equals(SystemColor.window)) {
                cg.setComposite(AlphaComposite.Src);
                cg.setColor(resetAlpha(color));
            } else {
                cg.setColor(color);
            }
            cg.fillRect(x, y, w, h);
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

    public static void drawString(JComponent c, Graphics g,
                           String text,
                           int x,
                           int y) {
        SwingUtilities2.drawString(c, g, text, x, y);
    }

    public static void drawStringUnderlineCharAt(JComponent c, Graphics g, String text, int underlinedIndex, int x, int y) {
        SwingUtilities2.drawStringUnderlineCharAt(c, g, text, underlinedIndex, x, y);
    }

    public static String clipStringIfNecessary(JComponent c, FontMetrics fm,
                                                String string,
                                                int availTextWidth) {
        return SwingUtilities2.clipStringIfNecessary(c, fm, string, availTextWidth);
    }

    public static int stringWidth(JComponent c, FontMetrics fm, String string){
        return SwingUtilities2.stringWidth(c, fm, string);
    }

    public static void installAATextInfo(UIDefaults table) {
        Object aaTextInfo = SwingUtilities2.AATextInfo.getAATextInfo(true);
        table.put(SwingUtilities2.AA_TEXT_PROPERTY_KEY, aaTextInfo);
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
        w.addNotify();
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
        w.addNotify();
        int result = nativeAddToolbarToWindow(w);
        if (result != 0) {
            throw new UnsupportedOperationException("Unable to add native toolbar to window");
        }
    }

    /**
     * Display a window as a sheet, if possible. A sheet is dismissed when the window is hidden or disposed.
     * <p>
     * The behavior of a sheet is similar to a document modal dialog in that it prevents user interaction with the
     * existing windows in the hierarchy of the owner. Unlike {@code setVisible(true)} on a model dialog, however, this
     * method does not block waiting for the sheet to be dismissed.
     *
     * @param w the window. The window must have a visible owner. The window must not be visible. If the window is a
     * dialog, its modality will be set to modeless.
     * @param closeHandler If not null, this object will be invoked when the sheet is dismissed.
     * @throws UnsupportedOperationException if the window could not be displayed as a sheet.
     */
    public static void displayAsSheet(Window w, Runnable closeHandler) throws UnsupportedOperationException {
        Window owner = w.getOwner();
        if (owner == null) {
            throw new UnsupportedOperationException("Unable to display as sheet: no owner window");
        }

        if (!owner.isVisible()) {
            throw new UnsupportedOperationException("Unable to display as sheet: owner window is not visible");
        }

        if (w.isVisible()) {
            throw new UnsupportedOperationException("Unable to display as sheet: the window must not be visible");
        }

        if (w instanceof Dialog) {
            Dialog d = (Dialog) w;
            d.setModalityType(Dialog.ModalityType.MODELESS);
        }

        if (!w.isDisplayable()) {
            w.addNotify();   // force the native peer to be created
        }

        // TBD: is there a way to paint the lightweight components without displaying the dialog?
        // Would like the window to be painted while it is expanding

        //w.validate();

        SheetCloser closer = new SheetCloser(w, closeHandler);
        int result = nativeDisplayAsSheet(w);
        if (result != 0) {
            closer.dispose();
            throw new UnsupportedOperationException("Unable to display as sheet");
        }

        w.setVisible(true); // cause the lightweight components to be painted -- this method blocks on a modal dialog
    }

    private static class SheetCloser extends WindowAdapter implements HierarchyListener {
        private final Window w;
        private final Runnable closeHandler;
        private boolean hasClosed = false;

        public SheetCloser(Window w, Runnable closeHandler) {
            this.w = w;
            this.closeHandler = closeHandler;
            w.addWindowListener(this);
            w.addHierarchyListener(this);
        }

        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            if (e.getChangeFlags() == HierarchyEvent.SHOWING_CHANGED && !w.isVisible()) {
                completed();
            }
        }

        @Override
        public void windowClosed(WindowEvent e) {
            completed();
        }

        private void completed() {
            if (!hasClosed) {
                hasClosed = true;
                dispose();
                if (closeHandler != null) {
                    closeHandler.run();
                }
            }
        }

        public void dispose() {
            w.removeWindowListener(this);
            w.removeHierarchyListener(this);
        }
    }

    private static native int nativeSetTitleBarStyle(Window w, int style);
    private static native int nativeAddToolbarToWindow(Window w);
    private static native int nativeDisplayAsSheet(Window w);

    public static native void syslog(String msg);
}
