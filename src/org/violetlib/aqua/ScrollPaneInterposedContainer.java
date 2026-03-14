/*
 * Copyright (c) 2015-2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.jetbrains.annotations.*;

/**
 * This object manages a container (called a holder) interposed between a scroll pane and its viewport, scroll bars,
 * and column header view.
 * <p>
 * The holder supports two requirements:
 * <ul>
 * <li>
 * When overlay scroll bars are used, the scroll bars and the viewport may overlap. When children of a container may
 * overlap, the container must disable optimized drawing.
 * </li>
 * <li>
 * When a scroll pane is rendered with a rounded rectangle border (as in macOS 26), the viewport and scroll bars must be
 * clipped to prevent them from painting over or outside the border. Clipping also requires that the container be a
 * painting origin, to ensure that no descendants are repainted directly (avoiding the clipping).
 * </li>
 * </ul>
 * These requirements can only be implemented by overriding methods. The holder allows the LAF to support these
 * requirements without any need to subclass application classes.
 * <p>
 * Inserting an intermediate container can confuse existing code that does not expect it. Specifically, JTable addNotify
 * and removeNotify will be unable to install or remove the table header as the scroll pane column header view. This
 * code simulates that behavior.
 */
public class ScrollPaneInterposedContainer {

    // see JDK-8139205: Enable a component UI to specify the result of isOptimizedDrawingEnabled()

    protected final JScrollPane scrollPane;

    /** non-null if an intermediate component is used to paint the overlay scroll bars */
    protected AquaOverlayViewportHolder holder;

    protected JViewport lastKnownViewport;
    protected Component lastKnownView;
    protected JViewport lastKnownColumnHeader;
    protected JViewport lastKnownRowHeader;

    protected PropertyChangeListener propertyChangeListener;
    protected ChangeListener viewportChangeListener;

    // Inserting or removing the holder generates hierarchy events that should be ignored.
    public static boolean isRearrangingHolder;

    /**
     * Install the holder on a scroll pane.
     */
    public ScrollPaneInterposedContainer(@NotNull JScrollPane scrollPane) {
        this.scrollPane = scrollPane;
        lastKnownViewport = scrollPane.getViewport();
        lastKnownView = lastKnownViewport != null ? SwingUtilities.getUnwrappedView(lastKnownViewport) : null;
        lastKnownColumnHeader = scrollPane.getColumnHeader();
        lastKnownRowHeader = scrollPane.getRowHeader();

        propertyChangeListener = new MyPropertyChangeListener();
        viewportChangeListener = new MyViewportChangeListener();

        holder = new AquaOverlayViewportHolder();
        holder.setBounds(0, 0, scrollPane.getWidth(), scrollPane.getHeight());

        asRearrangement(() -> scrollPane.add(holder));

        scrollPane.revalidate();
        scrollPane.repaint();
        sync(true);

        scrollPane.addPropertyChangeListener(propertyChangeListener);

        if (lastKnownViewport != null) {
            lastKnownViewport.addChangeListener(viewportChangeListener);
        }
    }

    private static void asRearrangement(@NotNull Runnable r)
    {
        boolean old = isRearrangingHolder;
        isRearrangingHolder = true;
        try {
            r.run();
        } finally {
            isRearrangingHolder = old;
        }
    }

    /**
     * Remove the holder from the scroll pane.
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
            JViewport columnHeader = scrollPane.getColumnHeader();
            if (columnHeader != null) {
                scrollPane.add(columnHeader);
            }
            JViewport rowHeader = scrollPane.getRowHeader();
            if (rowHeader != null) {
                scrollPane.add(rowHeader);
            }
            asRearrangement(() -> scrollPane.remove(holder));
            holder = null;
            scrollPane.revalidate();
            scrollPane.repaint();
        }
    }

    protected void sync(boolean isNewHolder) {

        asRearrangement(() -> {

            boolean changed = false;

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
                changed = true;
            }

            JViewport columnHeader = scrollPane.getColumnHeader();
            if (columnHeader != lastKnownColumnHeader) {
                if (lastKnownColumnHeader != null) {
                    if (holder != null) {
                        holder.remove(lastKnownColumnHeader);
                    }
                }
                lastKnownColumnHeader = columnHeader;
                changed = true;
            }

            JViewport rowHeader = scrollPane.getRowHeader();
            if (rowHeader != lastKnownRowHeader) {
                if (lastKnownRowHeader != null) {
                    if (holder != null) {
                        holder.remove(lastKnownRowHeader);
                    }
                }
                lastKnownRowHeader = rowHeader;
                changed = true;
            }

            if (holder != null) {
                if (viewport != null && viewport.getParent() != holder) {
                    holder.add(viewport); // this removes the viewport from the scroll pane
                    changed = true;
                }
                if (columnHeader != null && columnHeader.getParent() != holder) {
                    holder.add(columnHeader); // this removes the column header viewport from the scroll pane
                    changed = true;
                }
                if (rowHeader != null && rowHeader.getParent() != holder) {
                    holder.add(rowHeader); // this removes the row header viewport from the scroll pane
                    changed = true;
                }
                if (changed) {
                    LayoutManager layout = scrollPane.getLayout();
                    if (layout instanceof ScrollPaneLayout) {
                        ScrollPaneLayout spl = (ScrollPaneLayout) layout;
                        spl.syncWithScrollPane(scrollPane);
                    }
                }
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
        });
    }

    public void syncScrollPaneSize() {
        if (holder != null) {
            holder.setBounds(0, 0, scrollPane.getWidth(), scrollPane.getHeight());
            //JViewport v = scrollPane.getViewport();
            //v.setBounds(0, 0, scrollPane.getWidth(), scrollPane.getHeight());
        }
    }

    public void setInsetter(@Nullable Insetter2D insetter)
    {
        if (holder != null) {
            holder.setInsetter(insetter);
        }
    }

    public void reconfigureScrollBar(@NotNull JScrollBar sb, @NotNull String which) {

        if (holder != null) {

            // Move the scroll bar to the holder to avoid painting artifacts. Ensure that the components are ordered so
            // that the scroll bar will be painted after (on top of) the viewport.

            asRearrangement(() -> {
                scrollPane.remove(sb);
                holder.remove(sb);
                holder.add(sb, 0);
            });
        }
    }

    protected class MyPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String name = evt.getPropertyName();
            if (name != null) {
                if (name.equals("viewport") || name.equals("columnHeader") || name.equals("rowHeader")) {
                    sync(false);
                }
            }
        }
    }

    protected class MyViewportChangeListener implements ChangeListener {
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

    protected class AquaOverlayViewportHolder extends JComponent implements Accessible {

        private @Nullable Insetter2D insetter;

        public AquaOverlayViewportHolder() {
        }

        @Override
        public AccessibleContext getAccessibleContext() {
            JViewport v = scrollPane.getViewport();
            return v != null ? v.getAccessibleContext() : null;
        }

        @Override
        public boolean isOptimizedDrawingEnabled() {
            return false;
        }

        @Override
        protected boolean isPaintingOrigin() {
            return insetter != null;
        }

        @Override
        protected void paintChildren(Graphics g) {
            if (insetter != null) {
                Shape s = insetter.getInteriorShape(getWidth(), getHeight());
                if (s != null) {
                    Graphics2D gg = (Graphics2D) g.create();
                    gg.clip(s);
                    g = gg;
                }
            }
            super.paintChildren(g);
        }

        private void setInsetter(@Nullable Insetter2D insetter)
        {
            this.insetter = insetter;
        }
    }
}
