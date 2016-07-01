/*
 * Copyright (c) 2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import javax.swing.*;
import javax.swing.plaf.UIResource;

import static org.violetlib.aqua.AquaButtonExtendedTypes.WidgetInfo;
import static org.violetlib.jnr.aqua.AquaUIPainter.State;
import static org.violetlib.jnr.aqua.AquaUIPainter.ButtonState;
import static org.violetlib.aqua.AquaButtonExtendedTypes.ColorDefaults;

/**
 * An icon for a specific button whose rendering may depend upon the widget, state, and button state.
 * The rendering is based on a specified source image. For most buttons, that image is defined by the button's
 * default icon.
 */

public abstract class AquaButtonIcon implements Icon, UIResource {

    /**
     * A rendering key that contains the information that potentially affects the rendering of the icon.
     */
    public static final class Key {
        private WidgetInfo widgetInfo;
        private State state;
        private ButtonState bs;

        public Key(WidgetInfo widgetInfo, State state, ButtonState bs) {
            this.widgetInfo = widgetInfo;
            this.state = state;
            this.bs = bs;
        }

        public WidgetInfo getWidgetInfo() {
            return widgetInfo;
        }

        public State getState() {
            return state;
        }

        public ButtonState getButtonState() {
            return bs;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Key key = (Key) o;
            return Objects.equals(widgetInfo, key.widgetInfo) &&
                    state == key.state &&
                    bs == key.bs;
        }

        @Override
        public int hashCode() {
            return Objects.hash(widgetInfo, state, bs);
        }
    }

    protected final Image source;
    protected final boolean isTemplate;
    protected final Map<Key,Image> imageCache;
    protected final ColorDefaults colorDefaults;

    public AquaButtonIcon(Image source, boolean isTemplate, ColorDefaults colorDefaults) {
        this.source = source;
        this.isTemplate = isTemplate;
        this.colorDefaults = colorDefaults;
        imageCache = new HashMap<>();
    }

    public boolean isTemplate() {
        return isTemplate;
    }

    @Override
    public int getIconWidth() {
        return source.getWidth(null);
    }

    @Override
    public int getIconHeight() {
        return source.getHeight(null);
    }

    protected abstract Color getExistingColor();
    protected abstract Key getRenderingKey();

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        Key key = getRenderingKey();
        Image im = imageCache.get(key);
        if (im == null) {
            im = createImage(key);
            imageCache.put(key, im);
        }
        boolean isComplete = g.drawImage(im, x, y, c);
        if (!isComplete) {
            new ImageIcon(im);
            if (!g.drawImage(im, x, y, c)) {
                System.err.println("Button icon not drawn!");
            }
        }
    }

    protected boolean isNonexclusiveStyle(Key key) {
        return false;
    }

    protected Color getTemplateColor(Key key) {
        State state = key.getState();
        boolean isNonexclusiveStyle = isNonexclusiveStyle(key);
        if (state != State.DISABLED && state != State.DISABLED_INACTIVE && !isNonexclusiveStyle) {
            Color color = getExistingColor();
            if (color != null && !(color instanceof UIResource)) {
                return color;
            }
        }
        ButtonState bs = key.getButtonState();
        WidgetInfo info = key.getWidgetInfo();
        return info.getForeground(state, bs, colorDefaults, isNonexclusiveStyle, true);
    }

    /**
     * Create an image to use for the specified rendering key.
     * @param key
     * @return the image to use.
     */
    protected Image createImage(Key key) {
        if (isTemplate) {
            Color color = getTemplateColor(key);
            if (color != null) {
                Image im = AquaImageFactory.createImageFromTemplate(source, color);
                if (im != null) {
                    return im;
                }
            }
        }
        return createImageForKey(key);
    }

    protected Image createImageForKey(Key key) {
        State st = key.getState();
        if (st == State.PRESSED) {
            return AquaImageFactory.generatePressedDarkImage(source);
        }
        if (st == State.DISABLED || st == State.DISABLED_INACTIVE) {
            return AquaImageFactory.generateDisabledLightImage(source);
        }
        return source;
    }
}
