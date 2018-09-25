/*
 * Changes Copyright (c) 2015-2018 Alan Snyder.
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
import java.awt.event.*;
import java.beans.*;

import javax.swing.*;
import javax.swing.plaf.UIResource;

/**
 * This class is used by the text components, AquaEditorPaneUI, AquaTextAreaUI, AquaTextFieldUI and AquaTextPaneUI to control painting of the
 * component's border.  NOTE: It is assumed that this handler is added to components that extend JComponent.
 */
public class AquaFocusHandler implements FocusListener, PropertyChangeListener {
    // Flag to help focusGained() determine whether the origin focus loss was due to a temporary focus loss or not.
    private boolean wasTemporary = false;

    // Flag to track when a border needs a repaint due to a window becoming activate/inactive.
    private boolean repaintBorder = false;

    public static final String FRAME_ACTIVE_PROPERTY = "Frame.active";
    public static final String HAS_FOCUS_DELEGATE_KEY = "Component.hasFocusDelegate";
    public static final String QUAQUA_HAS_FOCUS_DELEGATE_KEY = "Quaqua.Component.cellRendererFor";

    public void focusGained(FocusEvent ev) {
        // If we gained focus and it wasn't due to a previous temporary focus loss
        // or the frame became active again, then repaint the border on the component.
        if (!wasTemporary || repaintBorder) {
            AquaBorder.repaintBorder((JComponent) ev.getSource());
            repaintBorder = false;
        }
        wasTemporary = false;
    }

    public void focusLost(FocusEvent ev) {
        wasTemporary = ev.isTemporary();

        // If we lost focus due to a permanent focus loss then repaint the border on the component.
        if (!wasTemporary) {
            AquaBorder.repaintBorder((JComponent) ev.getSource());
        }
    }

    public void propertyChange(PropertyChangeEvent ev) {
        if (!FRAME_ACTIVE_PROPERTY.equals(ev.getPropertyName())) return;

        if (Boolean.TRUE.equals(ev.getNewValue())) {
            // The FRAME_ACTIVE_PROPERTY change event is sent before a component gains focus.
            // We set a flag to help the focusGained() determine when they should be repainting
            // the components focus.
            repaintBorder = true;
        } else if (wasTemporary) {
            // The FRAME_ACTIVE_PROPERTY change event is sent after a component loses focus.
            // We use the wasTemporary flag to determine if we need to repaint the border.
            AquaBorder.repaintBorder((JComponent) ev.getSource());
        }
    }

    /**
     * Determine if a component should display as focused. This method handles cases where a component is used for
     * rendering and its focused appearance is controlled by a different component.
     * @param c The component.
     * @return true if the component has the keyboard focus or its delegate has the keyboard focus.
     */
    public static boolean hasFocus(Component c) {
        if (!c.isEnabled()) {
            return false;
        }

        JComponent jc = null;
        if (c instanceof JComponent) {
            jc = (JComponent) c;
        }

        if (jc != null && !isActive(jc)) {
            return false;
        }

        if (c.hasFocus()) {
            return true;
        }

        if (jc != null) {
            Object o = jc.getClientProperty(HAS_FOCUS_DELEGATE_KEY);
            if (o == null) {
                o = jc.getClientProperty(QUAQUA_HAS_FOCUS_DELEGATE_KEY);
            }
            if (o instanceof Component) {
                Component delegate = (Component) o;
                return delegate.hasFocus();
            }
        }
        return false;
    }

    public static void updateComponentTreeUIActivation(Component c, boolean active) {
        if (c instanceof javax.swing.JInternalFrame) {
            active = active && ((JInternalFrame)c).isSelected();
        }

        if (c instanceof javax.swing.JComponent) {
            ((javax.swing.JComponent)c).putClientProperty(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, active);
        }

        Component[] children = null;

        if (c instanceof javax.swing.JMenu) {
            children = ((javax.swing.JMenu)c).getMenuComponents();
        } else if (c instanceof Container) {
            children = ((Container)c).getComponents();
        }

        if (children == null) {
            return;
        }

        for (Component element : children) {
            updateComponentTreeUIActivation(element, active);
        }
    }

    public static boolean isActive(JComponent c) {
        if (c == null) return true;
        Object activeObj = c.getClientProperty(AquaFocusHandler.FRAME_ACTIVE_PROPERTY);
        if (Boolean.FALSE.equals(activeObj)) return false;
        return true;
    }

    static final PropertyChangeListener REPAINT_LISTENER = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
            Object source = evt.getSource();
            if (source instanceof JComponent) {
                ((JComponent)source).repaint();
            }
        }
    };

    protected static void install(JComponent c) {
        c.addPropertyChangeListener(FRAME_ACTIVE_PROPERTY, REPAINT_LISTENER);
    }

    protected static void uninstall(JComponent c) {
        c.removePropertyChangeListener(FRAME_ACTIVE_PROPERTY, REPAINT_LISTENER);
    }

    // Based on SwingUtilities2.compositeRequestFocus. It returns the component that compositeRequestFocus()
    // would request focus on.
    public static Component getFocusableComponent(Component component) {
        if (component instanceof Container) {
            Container container = (Container)component;
            if (container.isFocusCycleRoot()) {
                FocusTraversalPolicy policy = container.getFocusTraversalPolicy();
                Component comp = policy.getDefaultComponent(container);
                if (comp!=null) {
                    return comp;
                }
            }
            Container rootAncestor = container.getFocusCycleRootAncestor();
            if (rootAncestor!=null) {
                FocusTraversalPolicy policy = rootAncestor.getFocusTraversalPolicy();
                Component comp = policy.getComponentAfter(rootAncestor, container);

                if (comp!=null && SwingUtilities.isDescendingFrom(comp, container)) {
                    return comp;
                }
            }
        }
        if (component.isFocusable()) {
            return component;
        }
        return null;
    }
}
