/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.jetbrains.annotations.NotNull;

/**
 * This object implements a workaround. To use overlay scroll bars, the component containing the scroll bars and the
 * viewport must disable optimized drawing. This component is normally the JScrollPane, but JScrollPane enables
 * optimized drawing and (as of JDK 8) only a subclass can change that. The workaround is to install an intermediate
 * container (called a holder) for the scroll bars and viewport. The other part of the workaround is to simulate the
 * behavior of JTable addNotify and removeNotify, which installs or removes the table header as the scroll pane column
 * header view. The JTable code fails if there is an intermediate container.
 */
public class OverlayScrollPaneHack {

    // see JDK-8139205

    protected final JScrollPane scrollPane;

    /** non-null if an intermediate component is used to paint the overlay scroll bars */
    protected AquaOverlayViewportHolder holder;

    protected JViewport lastKnownViewport;
    protected Component lastKnownView;

    protected PropertyChangeListener propertyChangeListener;
    protected ChangeListener viewportChangeListener;

    /**
     * Install the intermediate container on a scroll pane.
     */
    public OverlayScrollPaneHack(@NotNull JScrollPane scrollPane) {
        this.scrollPane = scrollPane;
        lastKnownViewport = scrollPane.getViewport();
        lastKnownView = lastKnownViewport != null ? SwingUtilities.getUnwrappedView(lastKnownViewport) : null;

        propertyChangeListener = new MyPropertyChangeListener();
        viewportChangeListener = new MyViewportChangeListener();

        holder = new AquaOverlayViewportHolder();
        holder.setBounds(0, 0, scrollPane.getWidth(), scrollPane.getHeight());
        scrollPane.add(holder);
        scrollPane.revalidate();
        scrollPane.repaint();
        sync(true);

        scrollPane.addPropertyChangeListener(propertyChangeListener);

        if (lastKnownViewport != null) {
            lastKnownViewport.addChangeListener(viewportChangeListener);
        }
    }

    /**
     * Remove the intermediate container from the scroll pane.
     */
    public void dispose() {
        if (propertyChangeListener != null) {
            scrollPane.removePropertyChangeListener(propertyChangeListener);
            propertyChangeListener = null;
        }
        if (viewportChangeListener != null) {
            if (lastKnownViewport != null) {
                lastKnownViewport.removeChangeListener(viewportChangeListener);
            }
            viewportChangeListener = null;
        }
        if (lastKnownView instanceof JTable) {
            scrollPane.setColumnHeaderView(null);
        }
        if (holder != null) {
            JViewport viewport = scrollPane.getViewport();
            if (viewport != null && viewport.getParent() == holder) {
                scrollPane.add(viewport);
            }
            JScrollBar hsb = scrollPane.getHorizontalScrollBar();
            if (hsb != null) {
                scrollPane.add(hsb);
            }
            JScrollBar vsb = scrollPane.getVerticalScrollBar();
            if (vsb != null) {
                scrollPane.add(vsb);
            }
            scrollPane.remove(holder);
            holder = null;
            scrollPane.revalidate();
            scrollPane.repaint();
        }
    }

    protected void sync(boolean isNewHolder) {
        JViewport viewport = scrollPane.getViewport();
        if (viewport != lastKnownViewport) {
            if (lastKnownViewport != null) {
                lastKnownViewport.removeChangeListener(viewportChangeListener);
                if (holder != null) {
                    holder.remove(lastKnownViewport);
                }
            }
            if (viewport != null) {
                viewport.addChangeListener(viewportChangeListener);
            }
            lastKnownViewport = viewport;
        }

        if (holder != null && viewport != null && viewport.getParent() != holder) {
            holder.add(viewport);
            scrollPane.revalidate();  // force layout, which will reinstall the viewport into the layout manager
        }

        Component view = viewport != null ? SwingUtilities.getUnwrappedView(viewport) : null;

        if (holder != null && (isNewHolder || view != lastKnownView)) {
            // Normally JTable does this, but it will be confused by our intermediate container
            if (lastKnownView instanceof JTable) {
                scrollPane.setColumnHeaderView(null);
            }
            if (view instanceof JTable) {
                JTable t = (JTable) view;
                scrollPane.setColumnHeaderView(t.getTableHeader());
            }
        }

        lastKnownView = view;
    }

    public void syncScrollPaneSize() {
        if (holder != null) {
            holder.setBounds(0, 0, scrollPane.getWidth(), scrollPane.getHeight());
        }
    }

    public void reconfigure(JScrollBar sb, String which) {

        if (holder != null) {

            // Move the scroll bar to the holder to avoid painting artifacts. Ensure that the components are ordered so
            // that the scroll bar will be painted after (on top of) the viewport.

            scrollPane.remove(sb);
            holder.remove(sb);
            holder.add(sb, 0);
        }
    }

    protected class MyPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String name = evt.getPropertyName();
            if (name != null) {
                if (name.equals("viewport")) {
                    sync(false);
                }
            }
        }
    }

    protected class MyViewportChangeListener implements ChangeListener {
        private Component lastKnownView;

        @Override
        public void stateChanged(ChangeEvent e) {
            // The viewport view may have changed.
            JViewport v = scrollPane.getViewport();
            if (v != null) {
                Component view = SwingUtilities.getUnwrappedView(v);
                if (view != lastKnownView) {
                    sync(false);
                }
            }
        }
    }

    protected class AquaOverlayViewportHolder extends JComponent {

        public AquaOverlayViewportHolder() {
        }

        @Override
        public boolean isOptimizedDrawingEnabled() {
            return false;
        }
    }
}
