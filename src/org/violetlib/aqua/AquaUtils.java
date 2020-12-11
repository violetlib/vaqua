/*
 * Copyright (c) 2015-2020 Alan Snyder.
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
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ConvolveOp;
import java.awt.image.Kernel;
import java.lang.ref.SoftReference;
import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.concurrent.locks.Lock;
import java.util.function.Supplier;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.MenuBarUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.View;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaImageFactory.SlicedImageControl;
import org.violetlib.geom.GeneralRoundRectangle;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static javax.swing.SwingConstants.*;

final public class AquaUtils {

    public static final String TOOLBAR_PANEL_PROPERTY = "Aqua.isToolBarPanel";

    private static final String ANIMATIONS_PROPERTY = "swing.enableAnimations";

    private static final int javaVersion = obtainJavaVersion();

    private static final HierarchyListener toolbarStatusListener = new HierarchyListener() {
        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            toolbarStatusChanged(e);
        }
    };

    private static final HierarchyListener insetViewHierarchyListener = new InsetViewHierarchyListener();

    public static @NotNull String capitalize(@NotNull String s)
    {
        if (!s.isEmpty()) {
            char first = s.charAt(0);
            if (!Character.isUpperCase(first)) {
                return Character.toUpperCase(first) + s.substring(1);
            }
        }
        return s;
    }

    private interface WindowAppearanceChangedCallback {
        void windowAppearanceChanged(@NotNull Window w, @NotNull String appearanceName);
    }

    /**
     * Suppresses default constructor, ensuring non-instantiability.
     */
    private AquaUtils() {
    }

    static {
        registerWindowChangedAppearanceCallback(new WindowAppearanceChangedCallback() {
            @Override
            public void windowAppearanceChanged(@NotNull Window w, @NotNull String appearanceName) {
                if (w instanceof RootPaneContainer) {
                    RootPaneContainer rpc = (RootPaneContainer) w;
                    SwingUtilities.invokeLater(() -> {
                        JRootPane rootPane = rpc.getRootPane();
                        AquaRootPaneUI ui = getUI(rootPane, AquaRootPaneUI.class);
                        if (ui != null) {
                            ui.windowAppearanceChanged(appearanceName);
                        }
                    });
                }
            }
        });
    };

    public static void logError(@NotNull String message) {
        System.err.println(message);
    }

    public static void logError(@NotNull String message, @NotNull Throwable th) {
        System.err.println(message + ": " + th);
    }

    public static void logDebug(@NotNull String message) {
        System.err.println(message);
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
                int pos = token.indexOf("-");
                if (pos > 0) {
                    token = token.substring(0, pos);
                }
                int n = Integer.parseInt(token);
                ++tokenCount;
                int limit = tokenCount < 3 ? 100 : 1000;
                if (n < 0 || n >= limit) {
                    return 0;
                }
                version = version * limit + n;
                if (tokenCount == 3) {
                    return version;
                }
            }
        } catch (NumberFormatException ex) {
            return 0;
        }

        while (tokenCount < 3) {
            ++tokenCount;
            int limit = tokenCount < 3 ? 100 : 1000;
            version = version * limit;
        }
        return version;
    }

    /**
     * Return the UI of a component if it satisfies the specified class or interface.
     */
    public static @Nullable <T> T getUI(@NotNull JComponent c, Class<T> requestedClass) {
        // The getUI() method is public as of Java 9
        try {
            Class<? extends JComponent> clazz = c.getClass();
            Method getUIMethod = clazz.getMethod("getUI");
            Object ui = getUIMethod.invoke(c);
            return requestedClass.cast(ui);
        } catch (Throwable th) {
            return null;
        }
    }

    public static @NotNull String getWindowNameForDebugging(@NotNull Window w) {
        String name = w.getName();
        if (name == null) {
            name = w.getClass().getName() + "@" + Integer.toHexString(w.hashCode());
        }
        if (w instanceof Frame) {
            Frame fr = (Frame) w;
            String title = fr.getTitle();
            if (title != null) {
                if (title.length() > 20) {
                    title = title.substring(0, 20) + "...";
                }
                name += " \"" + title + "\"";
            }
        }
        return name;
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
    public static Rectangle getScreenBounds(@NotNull Point location, @Nullable Component invoker) {
        GraphicsConfiguration gc = getCurrentGraphicsConfiguration(location, invoker);
        return getScreenBounds(gc);
    }

    /**
     * Obtain the usable bounds of the specified screen or the primary screen.
     * This code taken from JPopupMenu.
     */
    public static @NotNull Rectangle getScreenBounds(@Nullable GraphicsConfiguration gc) {
        Rectangle bounds;
        Toolkit toolkit = Toolkit.getDefaultToolkit();
        if (gc != null) {
            // If we have GraphicsConfiguration use it to get screen bounds
            bounds = gc.getBounds();
        } else {
            // If we don't have GraphicsConfiguration use primary screen
            bounds = new Rectangle(toolkit.getScreenSize());
        }

        Insets insets = toolkit.getScreenInsets(gc);
        int top = insets.top;
        int bottom = insets.bottom;
        int left = insets.left;
        int right = insets.right;
        return new Rectangle(bounds.x + left, bounds.y + top, bounds.width - left - right,
                bounds.height - top - bottom);
    }

    /**
     * Try to find GraphicsConfiguration that contains the specified mouse sprite position.
     * This code taken from JPopupMenu.
     */
    private static @Nullable GraphicsConfiguration getCurrentGraphicsConfiguration(@NotNull Point location,
                                                                                   @Nullable Component invoker) {
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
    public static @Nullable JRootPane getRootPane(@NotNull Window w) {
        RootPaneContainer rpc = getRootPaneContainer(w);
        return rpc != null ? rpc.getRootPane() : null;
    }

    /**
     * Convenience method to get the layered pane of a window.
     */
    public static @Nullable JLayeredPane getLayeredPane(@NotNull Window w) {
        RootPaneContainer rpc = getRootPaneContainer(w);
        return rpc != null ? rpc.getLayeredPane() : null;
    }

    /**
     * Convenience method to map a window to a root pane container.
     */
    public static @Nullable RootPaneContainer getRootPaneContainer(@NotNull Window w) {
        if (w instanceof RootPaneContainer) {
            return (RootPaneContainer) w;
        }
        // Special case for EmbeddedFrame
        if (w.getComponentCount() == 1) {
            Component c = w.getComponent(0);
            if (c instanceof RootPaneContainer) {
                return (RootPaneContainer) c;
            }
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

    // A component is toolbar sensitive if its layout and/or rendering is different when it is contained in a toolbar or
    // a toolbar panel. Changes in toolbar status may occur when the ancestor hierarchy changes. It also may change if
    // a container dynamically changes its toolbar panel status. Dynamic changes to toolbar panel status are not
    // automatically recognized. The file chooser supports its dynamic toolbar panel status by generating hierarchy
    // changed events (using the generateToolbarStatusEvents method, below).

    public static void installToolbarSensitivity(@NotNull JComponent c) {
        ToolbarSensitiveUI ui = getUI(c, ToolbarSensitiveUI.class);
        if (ui != null) {
            c.addHierarchyListener(toolbarStatusListener);
        }
    }

    public static void uninstallToolbarSensitivity(@NotNull JComponent c) {
        c.removeHierarchyListener(toolbarStatusListener);
    }

    private static void toolbarStatusChanged(@NotNull HierarchyEvent e) {
        Component target = e.getComponent();
        if (target instanceof JComponent) {
            JComponent jc = (JComponent) target;
            ToolbarSensitiveUI ui = getUI(jc, ToolbarSensitiveUI.class);
            if (ui != null) {
                ui.toolbarStatusChanged(jc);
            }
        }
    }

    public static void generateToolbarStatusEvents(@NotNull JComponent c) {
        // For a toolbar status change to be recognized by subcomponents, a hierarchy changed event must be delivered
        // to each component.
        generateToolbarStatusEvents(c, c);
    }

    private static void generateToolbarStatusEvents(@NotNull JComponent top, @NotNull Component c) {
        HierarchyEvent e = new HierarchyEvent(c, HierarchyEvent.HIERARCHY_CHANGED, top, top.getParent());
        HierarchyListener[] listeners = c.getHierarchyListeners();
        for (HierarchyListener listener : listeners) {
            listener.hierarchyChanged(e);
        }
        if (c instanceof Container) {
            Container cc = (Container) c;
            int componentCount = cc.getComponentCount();
            for (int i = 0; i < componentCount; i++) {
                Component child = cc.getComponent(i);
                generateToolbarStatusEvents(top, child);
            }
        }
    }

    // Any component UI that uses this method should install toolbar sensitivity.
    public static boolean isOnToolbar(JComponent b) {
        Component parent = b.getParent();
        while (parent != null) {
            if (isToolBar(parent)) {
                return true;
            }
            parent = parent.getParent();
        }
        return false;
    }

    public static boolean isToolBar(@NotNull Component c) {
        if (c instanceof JToolBar) {
            JToolBar tb = (JToolBar) c;
            return !tb.isFloatable();
        }

        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            Object o = jc.getClientProperty(TOOLBAR_PANEL_PROPERTY);
            return Boolean.TRUE.equals(o);
        }

        return false;
    }

    public static boolean isDecorated(@NotNull Window w) {
        if (w instanceof Frame) {
            return !((Frame) w).isUndecorated();
        }

        if (w instanceof Dialog) {
            return !((Dialog) w).isUndecorated();
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
        long result = execute(w, nw -> nativeIsFullScreenWindow(nw) ? 1 : 0);
        return result != 0;
    }

    public static boolean isInsetViewSupported() {
        return OSXSystemProperties.OSVersion >= 1016;
    }

    // The following are copied from SwingUtilities, with modification.

    /**
     * Compute and return the location of the icons origin, the
     * location of origin of the text baseline, and a possibly clipped
     * version of the compound labels string.  Locations are computed
     * relative to the viewR rectangle.
     * The JComponents orientation (LEADING/TRAILING) will also be taken
     * into account and translated into LEFT/RIGHT values accordingly.
     *
     * @param c the component
     * @param fm the instance of {@code FontMetrics}, may be null if the text does not matter
     * @param text the text
     * @param iconSize the size of the icon, or null if there is no icon
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
                                             @Nullable FontMetrics fm,
                                             String text,
                                             @Nullable Dimension iconSize,
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
                iconSize,
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
     * This is the same as SwingUtilities.layoutCompoundLabelImpl, except:
     * An icon size is provided instead of the actual icon.
     * The algorithm for clipping the text is different. If a text is too long, "..." is
     * inserted at the middle of the text instead of at the end.
     */
    private static String layoutCompoundLabelImpl(
            JComponent c,
            @Nullable FontMetrics fm,
            String text,
            @Nullable Dimension iconSize,  // null if no icon
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

        if (iconSize != null) {
            iconR.width = iconSize.width;
            iconR.height = iconSize.height;
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
            } else if (fm != null) {
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

        int gap = (textIsEmpty || (iconSize == null)) ? 0 : textIconGap;

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
                if (v != null || fm == null) {
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
        boolean wasVisible = w.isVisible();
        if (!wasVisible) {
            nativeSetWindowVisibleField(w, true);
        }
        try {
            c.paintImmediately(0, 0, c.getWidth(), c.getHeight());
        } finally {
            if (!wasVisible) {
                nativeSetWindowVisibleField(w, false);
            }
        }
    }

    public abstract static class RecyclableSingleton<T> {
        final @Nullable T get() {
            return getSoftReferenceValue(this, () -> getInstance());
        }

        void reset() {
            put(this, null);
        }

        protected abstract @Nullable T getInstance();
    }

    static class RecyclableSingletonFromDefaultConstructor<T> extends RecyclableSingleton<T> {
        private final Class<T> clazz;

        RecyclableSingletonFromDefaultConstructor(Class<T> clazz) {
            this.clazz = clazz;
        }

        @Override
        protected @Nullable T getInstance() {
            return instantiate(clazz);
        }
    }

    abstract static class LazyKeyedSingleton<K, V> {
        private Map<K, V> refs;

        V get(K key) {
            if (refs == null) refs = new HashMap<>();

            V cachedValue = refs.get(key);
            if (cachedValue != null) return cachedValue;

            V value = getInstance(key);
            refs.put(key, value);
            return value;
        }

        protected abstract V getInstance(K key);
    }

    private static final RecyclableSingleton<Boolean> enableAnimations = new RecyclableSingleton<Boolean>() {
        @Override
        protected Boolean getInstance() {
            String sizeProperty = (String) AccessController.doPrivileged((PrivilegedAction<String>) () -> System.getProperty(
                    ANIMATIONS_PROPERTY));
            return !"false".equals(sizeProperty); // should be true by default
        }
    };

    private static boolean animationsEnabled() {
        return enableAnimations.get();
    }

    private static final int MENU_BLINK_DELAY = 50; // 50ms == 3/60 sec, according to the spec

    static void blinkMenu(Selectable selectable) {
        if (!animationsEnabled()) return;
        try {
            selectable.paintSelected(false);
            Thread.sleep(MENU_BLINK_DELAY);
            selectable.paintSelected(true);
            Thread.sleep(MENU_BLINK_DELAY);
        } catch (InterruptedException ignored) {
        }
    }

    interface Selectable {
        void paintSelected(boolean selected);
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

        ShadowBorder(Painter prePainter, Painter postPainter, int offsetX, int offsetY, float distance, float intensity, int blur) {
            this.prePainter = prePainter;
            this.postPainter = postPainter;
            this.offsetX = offsetX;
            this.offsetY = offsetY;
            this.distance = distance;
            this.blur = blur;
            int halfBlur = blur / 2;
            insets = new Insets(halfBlur - offsetY, halfBlur - offsetX, halfBlur + offsetY, halfBlur + offsetX);

            float blurry = intensity / (blur * blur);
            float[] blurKernel = new float[blur * blur];
            for (int i = 0; i < blurKernel.length; i++) blurKernel[i] = blurry;
            blurOp = new ConvolveOp(new Kernel(blur, blur, blurKernel));
        }

        @Override
        public final boolean isBorderOpaque() {
            return false;
        }

        @Override
        public final Insets getBorderInsets(Component c) {
            return insets;
        }

        @Override
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
            BufferedImage img = new BufferedImage(width + blur * 2, height + blur * 2, BufferedImage.TYPE_INT_ARGB_PRE);
            paintToImage(img, x, y, width, height);
//            debugFrame("border", img);
            g.drawImage(img, -blur, -blur, null);
        }

        private void paintToImage(BufferedImage img, int x, int y, int width, int height) {
            // clear the prior image
            Graphics2D imgG = (Graphics2D) img.getGraphics();
            imgG.setComposite(AlphaComposite.Clear);
            imgG.setColor(Color.black);
            imgG.fillRect(0, 0, width + blur * 2, height + blur * 2);

            int adjX = (int) (x + blur + offsetX + (insets.left * distance));
            int adjY = (int) (y + blur + offsetY + (insets.top * distance));
            int adjW = (int) (width - (insets.left + insets.right) * distance);
            int adjH = (int) (height - (insets.top + insets.bottom) * distance);

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

        SlicedShadowBorder(Painter prePainter, Painter postPainter, int offsetX, int offsetY, float distance, float intensity, int blur, int templateWidth, int templateHeight, int leftCut, int topCut, int rightCut, int bottomCut) {
            super(prePainter, postPainter, offsetX, offsetY, distance, intensity, blur);

            BufferedImage i = new BufferedImage(templateWidth, templateHeight, BufferedImage.TYPE_INT_ARGB_PRE);
            super.paintBorder(null, i.getGraphics(), 0, 0, templateWidth, templateHeight);
//            debugFrame("slices", i);
            slices = new SlicedImageControl(i, leftCut, topCut, rightCut, bottomCut, false);
        }

        @Override
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
            slices.paint(g, x, y, width, height);
        }
    }

    // options for when to use a magic eraser
    public final static int ERASE_IF_TEXTURED = 1<<0;   // erase if the window is natively textured
    public final static int ERASE_IF_VIBRANT = 1<<1;    // erase if the window is vibrant
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
        if (AquaColors.isPriority(bc)) {
            return bc;
        }

        return !isMagicEraser(c, eraserMode) ? bc : null;
    }

    /**
     * Fill the component bounds with the specified fill color or magic eraser.
     * @param c The component.
     */
    public static void fillRect(Graphics g, Component c, @NotNull Color color, int eraserMode) {
        fillRect(g, c, color, eraserMode, 0, 0, c.getWidth(), c.getHeight());
    }

    /**
     * Fill the specified rectangle with the specified fill color or magic eraser.
     * @param c The component.
     */
    public static void fillRect(Graphics g, Component c, @NotNull Color color, int eraserMode, int x, int y, int w, int h) {
        color = getFillColor(c, color, eraserMode);
        fillRect(g, color, x, y, w, h);
    }

    /**
     * Determine the fill color to use for a component.
     * @param c The component.
     * @return the fill color, or null to use the magic eraser.
     */
    private static Color getFillColor(Component c, @NotNull Color color, int eraserMode) {
        if ((eraserMode & ERASE_ALWLAYS) != 0) {
            return null;
        }

        Color bc = c.getBackground();
        if (AquaColors.isPriority(bc)) {
            return bc;
        }

        return !isMagicEraser(c, eraserMode) ? color : null;
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

        if ((eraserMode & ERASE_IF_TEXTURED) != 0) {
            JRootPane rootPane = SwingUtilities.getRootPane(c);
            if (rootPane != null && isNativeTextured(rootPane)) {
                return true;
            }
        }

        if ((eraserMode & ERASE_IF_VIBRANT) != 0) {
            if (c instanceof JComponent) {
                JComponent jc = (JComponent) c;
                if (AquaVibrantSupport.isVibrant(jc)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Determine the visible bounds of the specified component in the coordinate space of the AWT content view.
     * The bounds are normally the bounds of the component. However, if the component is within a viewport view, then
     * the bounds are constrained by the viewport.
     *
     * @param c The component.
     * @return the visible bounds, as defined above, in the coordinate space of the top component, or null if the
     *   component is not visible or not in a window.
     */

    public static @Nullable VisibleBounds getVisibleBoundsInContentView(@NotNull Component c) {
        if (c.getWidth() > 0 && c.getHeight() > 0) {
            Window w = SwingUtilities.getWindowAncestor(c);
            if (w != null) {
                VisibleBounds bounds = getVisibleBoundsInWindow(c, w);
                if (bounds != null) {
                    Insets s = w.getInsets();
                    int x = s.left;
                    int y = s.top;
                    int sw = w.getWidth() - (s.left + s.right);
                    int sh = w.getHeight() - (s.top + s.bottom);
                    Rectangle clip = new Rectangle(x, y, sw, sh);
                    Rectangle visibleBounds = bounds.visibleBounds.intersection(clip);
                    Rectangle oldFrame = bounds.frame;
                    Rectangle frame = new Rectangle(oldFrame.x - x, oldFrame.y - y, oldFrame.width, oldFrame.height);
                    return new VisibleBounds(visibleBounds, frame);
                }
            }
        }
        return null;
    }

    /**
     * Determine the visible bounds of the specified component in the coordinate space of the window.
     * The bounds are normally the bounds of the component. However, if the component is within a viewport view, then
     * the bounds are constrained by the viewport.
     *
     * @param c The component.
     * @return the visible bounds, as defined above, in the coordinate space of the window, or null if the
     *   component is not visible or not in a window.
     */
    public static @Nullable VisibleBounds getVisibleBoundsInWindow(@NotNull Component c) {
        if (c.getWidth() > 0 && c.getHeight() > 0) {
            Window w = SwingUtilities.getWindowAncestor(c);
            if (w != null) {
                return getVisibleBoundsInWindow(c, w);
            }
        }
        return null;
    }

    private static @Nullable VisibleBounds getVisibleBoundsInWindow(@NotNull Component c, @NotNull Window w) {
        if (w.isVisible()) {
            Rectangle visibleRegion = new Rectangle();
            if (computeVisibleRegion(c, visibleRegion) && !visibleRegion.isEmpty()) {
                Rectangle vr = SwingUtilities.convertRectangle(c, visibleRegion, w);
                Rectangle frame = SwingUtilities.convertRectangle(c.getParent(), c.getBounds(), w);
                return new VisibleBounds(vr, frame);
            }
        }
        return null;
    }

    private static boolean computeVisibleRegion(@NotNull Component c,
                                                @NotNull Rectangle result) {
        if (!c.isVisible()) {
            return false;
        }

        Container p = c.getParent();
        if (p == null || !p.isVisible()) {
            return false;
        }

        Rectangle bounds = c.getBounds();
        if (p instanceof Window) {
            result.setBounds(0, 0, bounds.width, bounds.height);
            return true;
        } else {
            if (!computeVisibleRegion(p, result)) {
                return false;
            }
            result.x -= bounds.x;
            result.y -= bounds.y;
            SwingUtilities.computeIntersection(0, 0, bounds.width, bounds.height, result);
            return true;
        }
    }

    /**
     * Determine the appropriate background for a component that displays the window content background color.
     * @param c A component in the window.
     * @return the color.
     */
    public static @NotNull Color getWindowBackground(@NotNull JComponent c) {
        EffectName effect = AquaFocusHandler.isActive(c) ? EffectName.EFFECT_NONE : EffectName.EFFECT_DISABLED;
        String baseColor = "windowBackground";
        JRootPane rp = c.getRootPane();
        if (rp != null && isTextured(rp)) {
            baseColor = "texturedWindowBackground";
        }
        return AquaColors.getBackground(c, baseColor, effect);
    }

    /**
     * Determine the appropriate background for a window top or bottom margin.
     * This method is not used when the margin is painted with a gradient.
     * @param rp The root pane the window.
     * @param isTop True for the top margin, false for the bottom margin.
     * @return the color.
     */
    public static @NotNull Color getWindowMarginBackground(@NotNull JRootPane rp, boolean isTop) {
        // In most cases, the margin color when flat matches the content area color.
        // One exception is a non-textured window in light mode.
        // The other is a dark mode non-textured unified title/tool bar (top margin).

        String base = isTextured(rp) ? "TexturedWindowMarginBackground" : "WindowMarginBackground";
        String prefix = isTop ? "top" : "bottom";
        String suffix = AquaFocusHandler.isActive(rp) ? "" : "_disabled";
        String colorName = prefix + base + suffix;
        AquaAppearance appearance = AppearanceManager.getAppearance(rp);
        Color bc = appearance.getColor(colorName);
        if (bc == null) {
            // should not happen
            logError("Undefined window margin background color: " + colorName);
            return AquaColors.CLEAR;
        } else {
            return bc;
        }
    }

    public static @NotNull Color getWindowMarginDividerColor(@NotNull JRootPane rp, boolean isTop) {
        String base = isTextured(rp) ? "TexturedWindowDivider" : "WindowDivider";
        String prefix = isTop ? "top" : "bottom";
        String suffix = AquaFocusHandler.isActive(rp) ? "" : "_disabled";
        String colorName = prefix + base + suffix;
        AquaAppearance appearance = AppearanceManager.getAppearance(rp);
        Color color = appearance.getColor(colorName);
        if (color == null) {
            // should not happen
            logError("Undefined window divider color: " + colorName);
            return AquaColors.CLEAR;
        } else {
            return color;
        }
    }

    public static boolean isTextured(@NotNull JRootPane rp) {
        if (isNativeTextured(rp)) {
            return true;
        }

        AquaRootPaneUI ui = getUI(rp, AquaRootPaneUI.class);
        if (ui != null) {
            AquaCustomStyledWindow customStyledWindow = ui.getCustomStyledWindow();
            if (customStyledWindow != null) {
                return customStyledWindow.isTextured();
            }
        }

        return false;
    }

    public static boolean isNativeTextured(@NotNull Window w) {
        if (w instanceof RootPaneContainer) {
            RootPaneContainer rpc = (RootPaneContainer) w;
            JRootPane rootPane = rpc.getRootPane();
            return isNativeTextured(rootPane);
        }
        return false;
    }

    public static boolean isNativeTextured(@NotNull JRootPane rp) {
        Object prop = rp.getClientProperty("apple.awt.brushMetalLook");
        if (prop != null) {
            if (Boolean.parseBoolean(prop.toString())) {
                return true;
            }
        }

        prop = rp.getClientProperty("Window.style");
        if (prop != null) {
            if (prop.equals("textured")) {
                return true;
            }
        }

        return false;
    }

    private static class InsetViewHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(@NotNull HierarchyEvent e) {
            JComponent c = (JComponent) e.getComponent();
            AquaViewStyleContainerUI ui = AquaUtils.getUI(c, AquaViewStyleContainerUI.class);
            if (ui != null) {
                JScrollPane sp = findScrollPaneAncestor(c);
                ui.scrollPaneAncestorChanged(sp);
            }
        }
    }

    private static @Nullable JScrollPane findScrollPaneAncestor(@NotNull JComponent c) {
        Container parent = c.getParent();
        if (parent instanceof JViewport) {
            parent = parent.getParent();
            if (parent instanceof OverlayScrollPaneHack.AquaOverlayViewportHolder) {
                parent = parent.getParent();
            }
            if (parent instanceof JScrollPane) {
                return (JScrollPane) parent;
            }
        }
        return null;
    }

    /**
     * Install event listeners to help manage the automatic use of the inset selection style on the specified component.
     * @param c The component.
     * @throws IllegalArgumentException if {@code c} does not support {@link AquaViewStyleContainerUI}.
     */
    public static void installInsetViewListener(@NotNull JComponent c) {
        AquaViewStyleContainerUI ui = AquaUtils.getUI(c, AquaViewStyleContainerUI.class);
        if (ui != null) {
            c.addHierarchyListener(insetViewHierarchyListener);
        } else {
            throw new IllegalArgumentException("Component must support AquaViewStyleContainerUI");
        }
    }

    /**
     * Uninstall the event listeners installed by {@link #installInsetViewListener}.
     * @param c The component.
     */
    public static void uninstallInsetViewListener(@NotNull Component c) {
        c.removeHierarchyListener(insetViewHierarchyListener);
    }

    /**
     * Apply equal side margins to the specified bounds.
     */
    public static @NotNull Rectangle applySideMargins(@NotNull Rectangle r, int margin) {
        if (margin > 0) {
            r = new Rectangle(r.x + margin, r.y, r.width - 2 * margin, r.height);
        }
        return r.isEmpty() ? new Rectangle(0, 0, 0, 0) : r;
    }

    /**
     * Paint the isolated inset style highlight for a striped row.
     */
    public static void paintInsetStripedRow(@NotNull Graphics2D g, int cx, int cy, int cw, int ch) {
        int top = 0;
        int side = 10;
        int radius = 8;
        RoundRectangle2D r = new RoundRectangle2D.Float(cx + side, cy + top, cw - 2 * side, ch - 2 * top, radius, radius);
        g.fill(r);
    }

    /**
     * Paint the isolated inset style highlight for a selected cell.
     */
    public static void paintInsetCellSelection(@NotNull Graphics2D g, int cx, int cy, int cw, int ch) {
        int top = 3;
        int side = 10;
        int radius = 8;
        RoundRectangle2D r = new RoundRectangle2D.Float(cx + side, cy + top, cw - 2 * side, ch - 2 * top, radius, radius);
        g.fill(r);
    }

    /**
     * Paint the inset style highlight for a selected cell that might be part of a vertical group.
     */
    public static void paintInsetCellSelection(@NotNull Graphics2D g, boolean isSelectedAbove, boolean isSelectedBelow,
                                               int cx, int cy, int cw, int ch) {
        int top = 0;
        int side = 10;
        int r = 8;
        int x = cx + side;
        int w = cw - 2 * side;

        Shape s;
        if (isSelectedAbove && isSelectedBelow) {
            s = new Rectangle(x, cy, w, ch);
        } else if (isSelectedAbove) {
            s = new GeneralRoundRectangle(x, cy, w, ch - top, 0, 0, 0, 0, r, r, r, r);
        } else if (isSelectedBelow) {
            s = new GeneralRoundRectangle(x, cy + top, w, ch - top, r, r, r, r, 0, 0, 0, 0);
        } else {
            s = new RoundRectangle2D.Float(cx + side, cy + top, cw - 2 * side, ch - 2 * top, r, r);
        }
        g.fill(s);
    }

    /**
     * Paint the inset style highlight for a selected menu item.
     */
    public static void paintInsetMenuItemSelection(@NotNull Graphics2D g, int cx, int cy, int cw, int ch) {
        int top = 0;
        int side = 4;
        int radius = 8;
        RoundRectangle2D r = new RoundRectangle2D.Float(cx + side, cy + top, cw - 2 * side, ch - 2 * top, radius, radius);
        g.fill(r);
    }

    /**
     * Fill with specified color or erase.
     * @param g The graphics context.
     * @param color The color to fill, or null to erase
     */
    public static void fillRect(Graphics g, @Nullable Color color, int x, int y, int w, int h) {
        Graphics cg = g.create();

        try {
            if (color instanceof GradientColor && cg instanceof Graphics2D) {
                GradientColor gradientColor = (GradientColor) color;
                Graphics2D gg = (Graphics2D) cg;
                if (gradientColor.useMagicEraser()) {
                    gg.setComposite(AlphaComposite.Src);
                    gg.setColor(AquaColors.CLEAR);
                    gg.fillRect(x, y, w, h);
                }
                Color start = gradientColor.getStart();
                Color finish = gradientColor.getFinish();
                GradientPaint gp = new GradientPaint(0, y, start, 0, y + h, finish);
                gg.setPaint(gp);
                gg.fillRect(x, y, w, h);
            } else if (color instanceof TintedEraser && cg instanceof Graphics2D) {
                TintedEraser tintedEraser = (TintedEraser) color;
                Graphics2D gg = (Graphics2D) cg;
                gg.setComposite(AlphaComposite.Src);
                gg.setColor(AquaColors.CLEAR);
                gg.fillRect(x, y, w, h);
                cg.setColor(tintedEraser);
                cg.fillRect(x, y, w, h);
            } else if (color != null && color != AquaColors.MAGIC_ERASER) {
                cg.setColor(color);
                cg.fillRect(x, y, w, h);
            } else if (cg instanceof Graphics2D) {
                ((Graphics2D) cg).setComposite(AlphaComposite.Src);
                cg.setColor(AquaColors.CLEAR);
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

    static final Map<Object,Object> appContextMap = new HashMap<>();

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

    public static @Nullable String getProperty(JComponent c, String... props) {
        for (String prop : props) {
            Object o = c.getClientProperty(prop);
            if (o != null) {
                return toString(o);
            }
        }
        return null;
    }

    public static @Nullable String toString(@Nullable Object o) {
        if (o instanceof String) {
            return (String) o;
        }
        return null;
    }

    public static @Nullable Boolean getBooleanProperty(JComponent c, String... props) {
        for (String prop : props) {
            Object o = c.getClientProperty(prop);
            if (o != null) {
                return toBoolean(o);
            }
        }
        return null;
    }

    public static @Nullable Boolean toBoolean(@Nullable Object o) {
        if (o instanceof Boolean) {
            return (Boolean) o;
        }
        return null;
    }

    public static @Nullable Integer getIntegerProperty(JComponent c, String... props) {
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

    public static @Nullable String getProperty(@NotNull String key) {
        try {
            return System.getProperty(key);
        } catch (SecurityException ex) {
            return null;
        }
    }

    public static @Nullable String getProperty(@NotNull String key, @Nullable String def) {
        try {
            return System.getProperty(key, def);
        } catch (SecurityException ex) {
            return def;
        }
    }

    /*
     * For use outside of the JDK in place of AppContext.get()
     */

    public static @Nullable Object get(@NotNull Object key) {
        synchronized (appContextMap) {
            return appContextMap.get(key);
        }
    }

    public static @Nullable Object put(@NotNull Object key, @Nullable Object value) {
        synchronized (appContextMap) {
            return appContextMap.put(key, value);
        }
    }

    public static <T> T getSoftReferenceValue(Object key, Supplier<T> supplier) {
        @SuppressWarnings("unchecked")
        SoftReference<T> ref = (SoftReference<T>) get(key);
        if (ref != null) {
            T object = ref.get();
            if (object != null) {
                return object;
            }
        }
        T object = supplier.get();
        ref = new SoftReference<>(object);
        put(key, ref);
        return object;
    }

    public static @Nullable <T> T instantiate(@NotNull Class<T> c) {
        try {
            return c.getDeclaredConstructor().newInstance();
        } catch (Exception ex) {
            return null;
        }
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
            int temp = x2;
            x2 = x1;
            x1 = temp;
        }
        g.fillRect(x1, y, x2 - x1 + 1, 1);
    }

    public static void installFont(@NotNull JComponent c, @NotNull String defaultFontName) {
        Font f = c.getFont();
        if (f == null || f instanceof UIResource) {
            c.setFont(UIManager.getFont(defaultFontName));
        }
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
     * Return the layer number where the specified component is painted.
     * @param c The component.
     * @return the layer number, or zero if the component is not contained in a JLayeredPane.
     */
    public static int getComponentLayer(@NotNull Component c) {
        Container parent = c.getParent();
        if (parent instanceof JLayeredPane) {
            JLayeredPane lp = (JLayeredPane) parent;
            return lp.getLayer(c);
        } else if (parent != null) {
            return getComponentLayer(parent);
        } else {
            return 0;
        }
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
        // Make certain we are the viewport's view and not, for example, the rowHeaderView of the scrollPane - an
        // implementor of fixed columns might do this.
        JViewport v = p.getViewport();
        return v != null && v.getView() == view ? p : null;
    }

    /**
     * Return the scroll pane that contains the specified component as the sole component of the viewport.
     */
    public static @Nullable JScrollPane getScrollPaneContainer(JComponent c) {
        Container parent = c.getParent();
        if (parent instanceof JViewport) {
            if (parent.getComponentCount() == 1) {
                Container viewportParent = parent.getParent();
                if (viewportParent instanceof OverlayScrollPaneHack.AquaOverlayViewportHolder) {
                    viewportParent = viewportParent.getParent();
                }
                if (viewportParent instanceof JScrollPane) {
                    return (JScrollPane) viewportParent;
                }
            }
        }
        return null;
    }

    public static void configure(@NotNull AquaUIPainter painter, @NotNull Component c, int width, int height) {
        AquaAppearance appearance = AppearanceManager.getRegisteredAppearance(c);
        if (appearance != null) {
            painter.configureAppearance(appearance);
        }
        painter.configure(width, height);
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
    public static void setTitleBarStyle(@NotNull Window w, int style) {
        ensureWindowPeer(w);
        execute(w, ptr -> setTitleBarStyle(w, ptr, style));
    }

    private static long setTitleBarStyle(@NotNull Window w, long wptr, int style) {

        JRootPane rp = getRootPane(w);
        assert rp != null;

        int result;

        if (!WindowStylePatch.isNeeded()) {
            boolean hasTitleBar = true;
            boolean isFullWindowContent = false;
            boolean isTransparentTitleBar = false;
            boolean isMovableByBackground = isNativeTextured(w);
            boolean isMovable = true;
            boolean isFixNeeded = false;
            boolean isHidden = false;

            switch (style) {
                case TITLE_BAR_NONE:
                    hasTitleBar = false;
                    break;

                case TITLE_BAR_TRANSPARENT:
                    isFullWindowContent = true;
                    isTransparentTitleBar = true;
                    isMovableByBackground = false;
                    isFixNeeded = true;
                    break;

                case TITLE_BAR_HIDDEN:
                    isFullWindowContent = true;
                    isTransparentTitleBar = true;
                    isMovable = OSXSystemProperties.OSVersion < 1011;
                    isMovableByBackground = false;
                    isFixNeeded = true;
                    isHidden = true;
                    break;

                case TITLE_BAR_OVERLAY:
                    isFullWindowContent = true;
                    isFixNeeded = true;
                    break;

                case TITLE_BAR_ORDINARY:
                default:
            }

            rp.putClientProperty("apple.awt.fullWindowContent", isFullWindowContent);
            rp.putClientProperty("apple.awt.transparentTitleBar", isTransparentTitleBar);
            rp.putClientProperty("apple.awt.draggableWindowBackground", isMovableByBackground);
            result = nativeSetTitleBarProperties(wptr, hasTitleBar, isMovable, isHidden, isFixNeeded);

        } else {
            result = nativeSetTitleBarStyle(wptr, style);
        }

        if (result != 0) {
            throw new UnsupportedOperationException("Unable to set window title bar style");
        } else if (style == TITLE_BAR_HIDDEN) {
            setWindowTitle(w, "");
        }
        return 0;
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
        execute(w, AquaUtils::addNativeToolbarToWindow);
    }

    private static long addNativeToolbarToWindow(long wptr) throws UnsupportedOperationException {
        int result = nativeAddToolbarToWindow(wptr);
        if (result != 0) {
            throw new UnsupportedOperationException("Unable to add native toolbar to window");
        }
        return 0;
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
    public static int unsetTitledWindowStyle(Window w) throws UnsupportedOperationException {
        Rectangle oldBounds = w.getBounds();
        Insets oldInsets = w.getInsets();
        int top = oldInsets.top;

        if (top == 0) {
            // Most likely, the window does not yet know its insets.
            top = 22;
        }

        try {
            int newHeight = oldBounds.height - top;
            int oldWidth = oldBounds.width;
            nativeSetTitledWindowStyle(w, false, new Insets(0, 0, 0, 0));
            w.setSize(oldWidth, newHeight);
            w.invalidate();
            w.validate();
            return top;
        } catch (Exception ex) {
            String errorMessage = ex.toString();
            throw new UnsupportedOperationException(errorMessage);
        }
    }

    public static void restoreTitledWindowStyle(Window w, int top, @NotNull Dimension originalSize) {
        try {
            nativeSetTitledWindowStyle(w, true, new Insets(top, 0, 0, 0));
            w.setSize(originalSize);
            w.invalidate();
            w.validate();
        } catch (Exception ex) {
            logError("Unable to restore titled window style", ex);
        }
    }

    /**
     * Return the object representing a custom styled window.
     * @param w The window.
     * @return the custom styled window object for {@code w}, or null if {@code w} does not use one of our custom window
     * styles.
     */
    public static AquaCustomStyledWindow getCustomStyledWindow(@NotNull Window w) {
        JRootPane rp = getRootPane(w);
        if (rp != null) {
            AquaRootPaneUI ui = getUI(rp, AquaRootPaneUI.class);
            if (ui != null) {
                return ui.getCustomStyledWindow();
            }
        }
        return null;
    }

    public static void setBackgroundCarefully(@NotNull Component c, @NotNull Color color) {

        JComponent contentPane = null;
        boolean contentPaneWasOpaque = false;
        JComponent layeredPane = null;
        boolean layeredPaneWasOpaque = false;

        if (c instanceof Window) {
            Window w = (Window) c;
            boolean isDecorated = isDecorated(w);

            // Special case for decorated frames and dialogs, which reject non-opaque background colors.
            if (isDecorated && color.getAlpha() < 255) {
                setWindowBackground(w, color);
                return;
            }

            // Special case for non-decorated windows, which insist on making the content pane and layered pane opaque
            // when setting an opaque background color.

            if (!isDecorated && color.getAlpha() == 255 && c instanceof RootPaneContainer) {
                RootPaneContainer rpc = (RootPaneContainer) c;
                Container container = rpc.getContentPane();
                if (container instanceof JComponent) {
                    contentPane = (JComponent) container;
                    contentPaneWasOpaque = contentPane.isOpaque();
                    layeredPane = rpc.getLayeredPane();
                    layeredPaneWasOpaque = layeredPane.isOpaque();
                }
            }
        }

        c.setBackground(color);

        if (contentPane != null && !contentPaneWasOpaque) {
            contentPane.setOpaque(false);
        }
        if (layeredPane != null && !layeredPaneWasOpaque) {
            layeredPane.setOpaque(false);
        }
    }

    /**
     * Set the textured attribute of the window peer. This method has no other side effects.
     * @param w The window.
     * @param isTextured The new value of the textured attribute.
     */
    public static void setWindowTextured(Window w, boolean isTextured) {

        try {
            nativeSetWindowTextured(w, isTextured);
        } catch (Throwable ex) {
            logError("Unable to set textured", ex);
        }
    }

    /**
     * Ensure that the specified window has a frame buffer that supports an alpha channel. An alpha channel is needed to
     * use the magic eraser.
     */
    public static void enableTranslucency(@NotNull Window w) {
        // The textured attribute is one of three ways to make a window support an alpha channel.
        // Setting the opaque attribute to false is another, but it triggers a repainting bug in Java.
        // Setting a window shape is the third, but a window shape is not wanted.
        AquaUtils.setWindowTextured(w, true);
        new ShadowMaker(w);
    }

    /**
     * I have not found a reliable way to ensure that enough opaque pixels are present to allow AppKit to compute
     * the window shadow for a vibrant popup. This class is a workaround for that problem.
     *
     * See bug JDK-7124236.
     */
    private static class ShadowMaker implements ActionListener, Runnable {
        private final @NotNull Window w;

        public ShadowMaker(@NotNull Window w) {
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
            JRootPane rp = AquaUtils.getRootPane(w);
            if (rp != null) {
                AquaUtils.syncAWTView(w);
                rp.putClientProperty("apple.awt.windowShadow.revalidateNow", Math.random());
            }
        }
    }

    /**
     * Set the background color of a window. Unlike the public methods for setting the window background, this method
     * does not reject setting a transparent or translucent background on a decorated window. This method may not work
     * if the window does not have a peer.
     * @param w The window.
     * @param c The color.
     */
    private static void setWindowBackground(Window w, Color c) {

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
                logError("Unable to set window background", th);
            }
        } finally {
            JavaSupport.unlockRenderQueue();
        }
    }

    public static void configurePopup(Window w, float radius) {
        execute(w, ptr -> nativeSetWindowCornerRadius(ptr, radius));
    }

    public static void setWindowRepresentedFilename(@NotNull Window w, @NotNull String filename) {
        try {
            execute(w, ptr -> nativeSetWindowRepresentedFilename(ptr, filename));
        } catch (UnsupportedOperationException ex) {
            // This operation fails on an embedded frame
        }
    }

    // for debugging
    public static void setAWTViewVisibility(Window w, boolean isVisible) {
        execute(w, ptr -> nativeSetAWTViewVisibility(ptr, isVisible));
    }

    public static void syncAWTView(Window w) {
        if (w.isDisplayable()) {
            // Both calls appear to be necessary to ensure that the pixels are ready when the window is made visible.
            Toolkit.getDefaultToolkit().sync();
            try {
                execute(w, AquaUtils::nativeSyncAWTView);
            } catch (UnsupportedOperationException ex) {
                // native window may no longer exist (esp when called from ShadowMaker)
            }
        }
    }

    /**
     * Specify the appearance to be used by a window. This method has no effect if the window is not displayable.
     * If the window appearance changes, a callback will be generated.
     * @param w The window.
     * @param appearanceName The name of the appearance, or null to cause the window to inherit its appearance.
     *                       An unrecognized appearance name is treated as null.
     * @return true if and only if {@code appearanceName} is recognized as a valid appearance name and the appearance
     * was installed in the window.
     */

    public static boolean setWindowAppearance(@NotNull Window w, @Nullable String appearanceName) {
        if (w.isDisplayable()) {
            return 0 == execute(w, ptr -> nativeSetWindowAppearance(ptr, appearanceName));
        } else {
            return false;
        }
    }

    /**
     * Identify the effective appearance of the specified window.
     * @return the appearance name, or null if not available.
     */

    public static @Nullable String getWindowEffectiveAppearanceName(@NotNull Window w) {
        if (w.isDisplayable()) {
            try {
                return executeForObject(w, AquaUtils::nativeGetWindowEffectiveAppearanceName);
            } catch (UnsupportedOperationException ex) {
                // native window may no longer exist
            }
        }
        return null;
    }

    /**
     * Perform an action that requires a native pointer and returns a long.
     */

    public interface NativeAction {
        long run(long wptr);
    }

    /**
     * Perform an action that requires the native NSWindow pointer for a window.
     */

    public static long execute(@NotNull Window w, NativeAction action) {
        Object[] data = new Object[1];
        long ptr = nativeGetNativeWindow(w, data);
        if (ptr == 0) {
            String name = w.getName();
            if (w instanceof Frame) {
                Frame fr = (Frame) w;
                name = fr.getTitle() + " " + name;
            }
            UnsupportedOperationException ex = new UnsupportedOperationException("Unable to get NSWindow for window " + name);
            //ex.printStackTrace();
            throw ex;
        } else {
            Lock lock = (Lock) data[0];
            if (lock != null) {
                lock.lock();
                try {
                    return action.run(ptr);
                } finally {
                    lock.unlock();
                }
            } else {
                return action.run(ptr);
            }
        }
    }

    /**
     * Perform an action that requires a native pointer and returns an object.
     */

    public interface NativeObjectAction<T> {
        T run(long wptr);
    }

    /**
     * Perform an action that requires the native NSWindow pointer for a window.
     */

    public static <T> T executeForObject(Window w, NativeObjectAction<T> action) {
        Object[] data = new Object[1];
        long ptr = nativeGetNativeWindow(w, data);
        if (ptr == 0) {
            String name = w.getName();
            if (w instanceof Frame) {
                Frame fr = (Frame) w;
                name = fr.getTitle() + " " + name;
            }
            throw new UnsupportedOperationException("Unable to get NSWindow for window " + name);
        } else {
            Lock lock = (Lock) data[0];
            if (lock != null) {
                lock.lock();
                try {
                    return action.run(ptr);
                } finally {
                    lock.unlock();
                }
            } else {
                return action.run(ptr);
            }
        }
    }

    public static void debugWindow(Window w) {
        execute(w, AquaUtils::nativeDebugWindow);
    }

    /**
     * Obtain the native window pointer and associated information for a window.
     * @param w The window.
     * @param data The ReadWriteLock used to control access to the native window pointer is stored in the first element
     *               (null if not available).
     * @return the native window pointer, or zero if not available.
     */
    private static native long nativeGetNativeWindow(Window w, Object[] data);

    private static native void nativeSetTitledWindowStyle(Window w, boolean isDecorated, Insets insets);
    private static native void nativeSetWindowTextured(Window w, boolean isTextured);
    private static native void nativeSetWindowBackground(Window w, Color color);
    private static native boolean nativeIsFullScreenWindow(long w);
    private static native int nativeSetTitleBarStyle(long w, int style);
    private static native int nativeSetTitleBarProperties(long w, boolean hasTitleBar, boolean isMovable, boolean isHidden, boolean isFixNeeded);
    private static native int nativeAddToolbarToWindow(long w);
    private static native int nativeSetWindowCornerRadius(long w, float radius);
    private static native int nativeSetWindowRepresentedFilename(long w, String name);
    private static native int nativeSetAWTViewVisibility(long w, boolean isVisible);
    private static native int nativeSyncAWTView(long w);
    private static native int nativeGetLeftSideBearing(JComponent c, FontMetrics fm, char firstChar);
    public static native boolean nativeHasOpaqueBeenExplicitlySet(JComponent c);
    public static native void nativeInstallAATextInfo(UIDefaults table);
    private static native void nativeSetWindowVisibleField(Window w, boolean isVisible);
    public static native void disablePopupCache(Popup p);
    public static native boolean getScreenMenuBarProperty();
    public static native void setScreenMenuBar(JFrame frame, MenuBarUI menuBarUI);
    public static native void clearScreenMenuBar(JFrame frame, MenuBarUI menuBarUI);
    private static native int nativeSetWindowAppearance(long w, @Nullable String appearanceName);
    private static native @Nullable String nativeGetWindowEffectiveAppearanceName(long w);
    public static native @Nullable String nativeGetApplicationAppearanceName();
    private static native void registerWindowChangedAppearanceCallback(@NotNull WindowAppearanceChangedCallback callback);

    public static native int nativeDebugWindow(long w);
    public static native void syslog(String msg);
}
