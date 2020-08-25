/*
 * Copyright (c) 2015 Alan Snyder.
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
import java.beans.*;
import java.security.PrivilegedAction;

import javax.swing.*;
import javax.swing.plaf.*;

import org.violetlib.jnr.aqua.AquaUIPainter.Size;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaUtilControlSize {
    protected final static String CLIENT_PROPERTY_KEY = "JComponent.sizeVariant";
    protected final static String SYSTEM_PROPERTY_KEY = "swing.component.sizevariant";

    interface Sizeable {
        void applySizeFor(JComponent c, Size size, boolean isDefaultSize);
    }

    protected static final RecyclableSingleton<PropertySizeListener> sizeListener = new RecyclableSingletonFromDefaultConstructor<PropertySizeListener>(PropertySizeListener.class);
    protected static PropertySizeListener getSizeListener() {
        return sizeListener.get();
    }

    protected static void addSizePropertyListener(final JComponent c) {
        c.addPropertyChangeListener(CLIENT_PROPERTY_KEY, getSizeListener());
        AquaUtilControlSize.applyComponentSize(c, c.getClientProperty(CLIENT_PROPERTY_KEY));
    }

    protected static void removeSizePropertyListener(final JComponent c) {
        c.removePropertyChangeListener(CLIENT_PROPERTY_KEY, getSizeListener());
    }

    private static Size getSizeFromString(final String name) {
        if ("regular".equalsIgnoreCase(name)) return Size.REGULAR;
        if ("small".equalsIgnoreCase(name)) return Size.SMALL;
        if ("mini".equalsIgnoreCase(name)) return Size.MINI;
        if ("large".equalsIgnoreCase(name)) return Size.LARGE;
        return null;
    }

    public static String getStringFromSize(Size sz) {
        if (sz == Size.REGULAR) return "regular";
        if (sz == Size.SMALL) return "small";
        if (sz == Size.MINI) return "mini";
        if (sz == Size.LARGE) return "large";
        return null;
    }

    private static Size getDefaultSize() {
        final String sizeProperty = java.security.AccessController.doPrivileged((PrivilegedAction<String>) () -> System.getProperty(SYSTEM_PROPERTY_KEY));
        final Size size = getSizeFromString(sizeProperty);
        if (size != null) return size;
        return Size.REGULAR;
    }

    protected final static Size defaultSize = getDefaultSize();

    public static Size getUserSizeFrom(JComponent c) {
        final Object sizeProp = c.getClientProperty(CLIENT_PROPERTY_KEY);
        if (sizeProp == null) {
            return AquaUtilControlSize.defaultSize;
        }
        final Size size = getSizeFromString(sizeProp.toString());
        return size != null ? size : Size.REGULAR;
    }

    // call JComponent.getUI() if it exists, then call Sizeable.applySizeFor() if the UI is "Sizeable"
    // next best thing to -respondsToSelector: :-P

    /**
     * If possible, ask the component UI to apply any changes based on the size variant.
     * @param c The component.
     * @param size The component size to apply
     * @param isDefaultSize True if the size was obtained as a default, false if it was specified using a client property.
     * @return true if the UI was invoked, false otherwise.
     */
    private static boolean applyUISizing(JComponent c, Size size, boolean isDefaultSize) {
        Sizeable sizeable = AquaUtils.getUI(c, Sizeable.class);
        if (sizeable != null) {
            sizeable.applySizeFor(c, size, isDefaultSize);
            return true;
        } else {
            return false;
        }
    }

    protected static class PropertySizeListener implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent evt) {
            final String key = evt.getPropertyName();
            if (!CLIENT_PROPERTY_KEY.equalsIgnoreCase(key)) return;

            final Object source = evt.getSource();
            if (!(source instanceof JComponent)) return;

            final JComponent c = (JComponent)source;
            applyComponentSize(c, evt.getNewValue());
        }
    }

    /**
     * Update the component size based on the client property.
     * @param c The component.
     * @param value The value of the size client property.
     */
    private static void applyComponentSize(JComponent c, Object value) {
        Size size = null;
        boolean isDefaultSize = false;

        if (value != null) {
            size = getSizeFromString(value.toString());
        }

        if (size == null) {
            size = AquaUtilControlSize.defaultSize;
            isDefaultSize = true;
        }

        if (!applyUISizing(c, size, isDefaultSize)) {
            // If the component UI does not understand size variants, at least we can set the proper font.
            configureFontFromSize(c, size);
        }
    }

    public static void configureFontFromSize(JComponent c, Size size) {
        if (isOKToInstallDefaultFont(c)) {
            Font priorFont = c.getFont();
            if (priorFont != null) {
                installDefaultFont(c, getFontForSize(priorFont, size));
            }
        }
    }

    /**
     * Install a default font in a component. If it looks like the application has already installed a font, we do not
     * change it.
     */
    public static void installDefaultFont(JComponent c, Font f) {
        if (isOKToInstallDefaultFont(c)) {
            c.setFont(f);
        }
    }

    public static boolean isOKToInstallDefaultFont(JComponent c) {
        Font f = c.getFont();
        return f == null || f instanceof UIResource;
    }

    public static void uninstallDefaultFont(JComponent c) {
        Font f = c.getFont();
        if (f instanceof UIResource) {
            c.setFont(null);
        }
    }

    /**
     * Return a variant of the specified font sized to match the specified component size variant.
     * @param f The specified font.
     * @param size The component size variant.
     * @return f a derived version of {@code f} at an appropriate size.
     * Note that the returned font will not be a UIResource.
     */
    public static Font getFontForSize(Font f, Size size) {
        if (!(f instanceof AquaFonts.DerivedUIResourceFont)) {
            f = new AquaFonts.DerivedUIResourceFont(f);
        }

        if (size == Size.MINI) return f.deriveFont(AquaFonts.getMiniControlTextFont().getSize2D());
        if (size == Size.SMALL) return f.deriveFont(AquaFonts.getSmallControlTextFont().getSize2D());
        return f.deriveFont(AquaFonts.getControlTextFont().getSize2D());
    }
}
