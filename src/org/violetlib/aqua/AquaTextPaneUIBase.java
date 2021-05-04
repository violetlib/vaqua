/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A base class for multiple-line text component UIs.
 */
public class AquaTextPaneUIBase extends AquaTextComponentUIBase {

    public AquaTextPaneUIBase(@NotNull AquaTextComponentUIDelegate delegate) {
        super(delegate);
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        AquaKeyBindings bindings = AquaKeyBindings.instance();
        bindings.installAquaUpDownActions(editor);
    }

    // Scrollable component support
    //
    // A multi-line text component can be made scrollable by wrapping it in a scroll pane. When this is done, the text
    // component border should be attached to the scroll pane instead of the text component, although it continues to
    // be configured from the text component.
    //
    // The special case for a scrollable text component is recognized when:
    //
    // The text component does not have an application defined border.
    // The text component opaque attribute has not been assigned by the application.
    // The text component parent is a JViewport with one child.
    // The JViewport is associated with a JScrollPane ancestor.
    // The scroll pane does not have an application defined border.

    protected @Nullable JScrollPane owningScrollPane;
    protected @Nullable Container watchedParent;
    protected boolean installingBorder;

    protected @Nullable HierarchyListener hierarchyListener;
    protected @Nullable PropertyChangeListener scrollPaneBorderListener;
    protected @Nullable ContainerListener watchedParentContainerListener;

    @Override
    protected void installDefaults() {
        super.installDefaults();
        updateBorderOwner();
    }

    @Override
    protected void uninstallDefaults() {
        clearScrollPaneBorder();
        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        hierarchyListener = new MyHierarchyListener();
        editor.addHierarchyListener(hierarchyListener);
    }

    @Override
    protected void uninstallListeners() {
        editor.removeHierarchyListener(hierarchyListener);
        hierarchyListener = null;
        super.uninstallListeners();
    }

    @Override
    protected void propertyChange(@NotNull PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String prop = evt.getPropertyName();
        if ("border".equals(prop) && !installingBorder) {
            updateBorderOwner();
        }
    }

    protected void updateBorderOwner() {
        if (editor != null && !JavaSupport.hasOpaqueBeenExplicitlySet(editor)) {
            Border textComponentBorder = editor.getBorder();
            if (textComponentBorder == null || textComponentBorder instanceof UIResource) {
                Container parent = editor.getParent();
                if (parent instanceof JViewport && parent.getComponentCount() == 1) {
                    JScrollPane scrollPane = AquaUtils.getScrollPaneAncestor(editor);
                    if (scrollPane != null) {
                        Border b = scrollPane.getBorder();
                        if (b == null || b instanceof UIResource) {
                            updateScrollPaneBorder(scrollPane, parent);
                            return;
                        }
                    }
                }
            }
        }
        clearWatchedParent();
        clearOwningScrollPane();
    }

    protected void clearScrollPaneBorder() {
        clearWatchedParent();
        clearOwningScrollPane();
        installBorder();
    }

    private void installBorder() {
        if (editor != null) {
            Border textComponentBorder = editor.getBorder();
            if (textComponentBorder == null || textComponentBorder instanceof UIResource) {
                installingBorder = true;
                if (JavaSupport.hasOpaqueBeenExplicitlySet(editor)) {
                    // If the application set the opaque attribute, do not install our border
                    editor.setBorder(null);
                } else {
                    if (!(textComponentBorder instanceof AquaTextComponentBorder)) {
                        Border b = new AquaTextComponentBorder(editor);
                        editor.setBorder(b);
                    }
                }
                installingBorder = false;
            }
        }
    }

    private void clearWatchedParent() {
        if (watchedParent != null) {
            watchedParent.removeContainerListener(watchedParentContainerListener);
            watchedParent = null;
        }
    }

    private void clearOwningScrollPane() {
        if (owningScrollPane != null) {
            owningScrollPane.removePropertyChangeListener(scrollPaneBorderListener);
            LookAndFeel.installBorder(owningScrollPane, "ScrollPane.border");
            JViewport viewport = owningScrollPane.getViewport();
            viewport.setOpaque(true);
            LookAndFeel.installProperty(owningScrollPane, "opaque", Boolean.TRUE);
            owningScrollPane = null;
        }
    }

    protected void updateScrollPaneBorder(@NotNull JScrollPane scrollPane, @NotNull Container parent) {
        if (scrollPane != owningScrollPane) {
            clearOwningScrollPane();
            owningScrollPane = scrollPane;

            if (scrollPaneBorderListener == null) {
                scrollPaneBorderListener = new ScrollPaneBorderListener();
            }
            owningScrollPane.addPropertyChangeListener(scrollPaneBorderListener);
        }

        if (parent != watchedParent) {
            clearWatchedParent();
            watchedParent = parent;
            if (watchedParentContainerListener == null) {
                watchedParentContainerListener = new WatchedParentContainerListener();
            }
            watchedParent.addContainerListener(watchedParentContainerListener);
        }

        AquaTextComponentBorder tcb = new AquaTextComponentBorder(editor);
        installingBorder = true;
        scrollPane.setBorder(tcb);
        LookAndFeel.installProperty(scrollPane, "opaque", Boolean.FALSE);

        JViewport viewport = scrollPane.getViewport();
        viewport.setOpaque(false);

        editor.setBorder(null);
        installingBorder = false;
    }

    private class MyHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(@NotNull HierarchyEvent e) {
            if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0) {
                if (editor != null && editor.isDisplayable()) {
                    updateBorderOwner();
                }
            }
        }
    }

    private class WatchedParentContainerListener implements ContainerListener {
        @Override
        public void componentAdded(ContainerEvent e) {
            updateBorderOwner();
        }

        @Override
        public void componentRemoved(ContainerEvent e) {
            updateBorderOwner();
        }
    }

    private class ScrollPaneBorderListener implements PropertyChangeListener {
        @Override
        public void propertyChange(@NotNull PropertyChangeEvent evt) {
            String prop = evt.getPropertyName();
            if ("border".equals(prop) && !installingBorder) {
                updateBorderOwner();
            }
        }
    }
}
