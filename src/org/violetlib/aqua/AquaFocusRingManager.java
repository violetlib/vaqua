/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * Implement focus rings drawn over components, not by them.
 */
public class AquaFocusRingManager {

    private static AquaFocusRingManager INSTANCE = new AquaFocusRingManager();

    public static AquaFocusRingManager getInstance() {
        return INSTANCE;
    }

    private Component currentOwner;

    private final String FOCUS_OWNER_PROPERTY = "permanentFocusOwner";

    private final PropertyChangeListener myFocusChangeListener;
    private boolean isInstalled;
    private final AquaFocusRingPainter painter;

    protected AquaFocusRingManager() {

        myFocusChangeListener = new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent ev) {
                String name = ev.getPropertyName();

                if (!FOCUS_OWNER_PROPERTY.equals(name)) {
                    return;
                }

                Object o = ev.getNewValue();
                if (o instanceof Component) {
                    Component c = (Component) o;
                    AquaFocusRingManager.this.update(c);
                }
            }
        };

        painter = new AquaFocusRingPainter();
    }

    public void install() {
        if (!isInstalled) {
            KeyboardFocusManager fm = KeyboardFocusManager.getCurrentKeyboardFocusManager();
            fm.addPropertyChangeListener(FOCUS_OWNER_PROPERTY, myFocusChangeListener);
            Component c = fm.getPermanentFocusOwner();
            update(c);
            isInstalled = true;
        }
    }

    public void uninstall() {
        if (isInstalled) {
            KeyboardFocusManager fm = KeyboardFocusManager.getCurrentKeyboardFocusManager();
            fm.removePropertyChangeListener(FOCUS_OWNER_PROPERTY, myFocusChangeListener);
            if (currentOwner != null) {
                painter.setFocusOwner(null);
                currentOwner = null;
            }
            isInstalled = false;
        }
    }

    private void update(Component newFocusOwner) {
        if (newFocusOwner == currentOwner) {
            return;
        }

        currentOwner = newFocusOwner;
        Component displayableComponent = currentOwner instanceof JComponent && isValid(currentOwner) ? currentOwner : null;
        painter.setFocusOwner((JComponent) displayableComponent);
    }

    private boolean isValid(Component c) {
        // TBD: could improve this
        // TBD: should this be dynamic, like Frame.active?

        return c.isVisible();
    }

    private void updateFocusRing(JComponent c) {
        if (c == currentOwner) {
            if (isValid(c)) {
                painter.update();
            }
        }
    }

    /**
     * This method should be called by a component UI when the focus ring outline for the component may have changed
     * without the bounds of the component changing. This method only needs to be called when the focus ring is being
     * displayed.
     */
    public static void focusRingOutlineChanged(JComponent c) {
        AquaFocusRingManager m = getInstance();
        m.updateFocusRing(c);
    }
}
