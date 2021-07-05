/*
 * Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.*;

/**
 * The container that wraps a component to support vertical scrolling and rounded corners. Used for contextual menus,
 * pop up and pull down menus, but not for editable combo box menus. By subclassing, supports increasing the popup
 * height when scrolling.
 */
public class AquaScrollingPopupMenuWrapper extends JPanel {

    // TBD: a larger initial delay does not work when the popup is resized because we get an unwanted mouse exit/mouse
    // enter for each resize, which resets the timer each time.

    private static final int kInitialDelay = 20;    // was 200, see TBD above
    private static final int kNormalDelay = 20;

    protected final ArrowPane topArrowPane;
    protected final JComponent originalContent;
    protected final JComponent scrollableView;
    protected final ArrowPane bottomArrowPane;
    protected final JViewport viewport;

    protected int width;
    protected int height;

    protected boolean isTopArrowInstalled;
    protected boolean isBottomArrowInstalled;

    protected int topArrowHeight;
    protected int bottomArrowHeight;
    protected int unitScroll = 12;

    protected Timer fScrollTimer;
    protected ActionListener fScrollListener;

    /**
     * Create a component that displays the specified content in a vertically scrollable viewport.
     * @param content The content.
     * @param selectedRegion An optional region of the content that should be initially visible. The view may be
     *                       scrolled to make the region visible.
     * @param selectedRegionLocation An optional preferred location for the selected region within this component. The
     *                               view may be scrolled to place the region at this location.
     * @param width The preferred width of this component.
     * @param height The preferred height of this component.
     * @param border An optional border for this component.
     */
    public AquaScrollingPopupMenuWrapper(JComponent content,
                                         Rectangle selectedRegion,
                                         Point selectedRegionLocation,
                                         int width,
                                         int height,
                                         Border border) {
        super(null);

        this.originalContent = content;
        this.width = width;
        this.height = height;

        setOpaque(false);
        setBorder(border);
        setLayout(new MyLayoutManager());
        setPreferredSize(new Dimension(width, height));

        // The mouse wheel listener for a menu must be on the menu otherwise BasicPopupMenuUI will cancel the popup.
        content.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseWheelMoved(MouseWheelEvent e) {
                if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) {
                    mouseWheelScroll(e);
                }
                e.consume();
            }
        });

        if (content instanceof JPopupMenu) {
            // Can't have a popup menu directly within a viewport because setting the position of the popup menu tries
            // to move the popup on the screen
            JPanel wrap = new JPanel();
            wrap.setName("JPopUpMenuWrapper");
            wrap.setLayout(new BorderLayout());
            wrap.add(content);
            wrap.setOpaque(false);
            scrollableView = wrap;
        } else {
            scrollableView = content;
        }

        Dimension contentSize = scrollableView.getPreferredSize();
        scrollableView.setSize(contentSize);

        this.topArrowPane = createArrowPane(true);
        this.bottomArrowPane = createArrowPane(false);

        viewport = new JViewport();
        viewport.setView(scrollableView);
        viewport.setLayout(null);   // ViewportLayout is too eager to reposition the view, so we do our own layout
        add(viewport);
        add(topArrowPane);
        add(bottomArrowPane);

        topArrowHeight = topArrowPane.getPreferredSize().height;
        bottomArrowHeight = bottomArrowPane.getPreferredSize().height;

        int initialViewPosition = 0;

        if (selectedRegion != null) {

            // Determine the initial view position (scroll position) so that the selected region is visible and, if
            // possible, displayed in the preferred location. This calculation is tricky because the top and bottom
            // arrows may or may not be needed and must be accounted for.

            Insets s = getInsets();
            boolean isTopArrowAllocated = false;
            boolean isBottomArrowAllocated = false;
            int availableHeight = height - s.top - s.bottom;
            if (contentSize.height > availableHeight) {
                // the bottom arrow will be displayed
                isBottomArrowAllocated = true;
                availableHeight -= bottomArrowHeight;
            }

            int selectedRegionOrigin = selectedRegion.y;
            int selectedRegionHeight = selectedRegion.height;
            if (selectedRegionLocation != null) {
                int desiredY = selectedRegionLocation.y - s.top;
                initialViewPosition = Math.max(0, selectedRegionOrigin - desiredY);
                if (initialViewPosition > 0) {
                    // the top arrow will be displayed
                    isTopArrowAllocated = true;
                    initialViewPosition += topArrowHeight;
                    availableHeight -= topArrowHeight;
                    // the bottom arrow may or may not be displayed
                    if (isBottomArrowAllocated) {
                        isBottomArrowAllocated = false;
                        availableHeight += bottomArrowHeight;
                    }
                    if (contentSize.height - initialViewPosition > availableHeight) {
                        isBottomArrowAllocated = true;
                        availableHeight -= bottomArrowHeight;
                    }
                }
            }

            // We have a nominal position for the selected region.
            // We may need to scroll up to make it visible.
            int y = selectedRegionOrigin - initialViewPosition; // relative to content (ignores top arrow)
            if (y + selectedRegionHeight > availableHeight) {
                // If we have not already accounted for a top arrow, do it now
                if (!isTopArrowAllocated) {
                    isTopArrowAllocated = true;
                    initialViewPosition += topArrowHeight;
                    availableHeight -= topArrowHeight;
                    // the bottom arrow may or may not be displayed
                    if (isBottomArrowAllocated) {
                        isBottomArrowAllocated = false;
                        availableHeight += bottomArrowHeight;
                    }
                    if (contentSize.height - initialViewPosition > availableHeight) {
                        isBottomArrowAllocated = true;
                        availableHeight -= bottomArrowHeight;
                    }
                }
                int delta = y + selectedRegionHeight - availableHeight;
                initialViewPosition += delta;

                // after scrolling up, the bottom arrow may no longer be needed
                if (isBottomArrowAllocated) {
                    if (contentSize.height - initialViewPosition <= availableHeight + bottomArrowHeight) {
                        initialViewPosition -= bottomArrowHeight;
                    }
                }
            }
        }

        viewport.setBorder(null);
        viewport.setOpaque(false);
        viewport.setViewPosition(new Point(0, initialViewPosition));

        configure(true);

        fScrollListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (topArrowPane.isActive) {
                    topArrowPane.actionPerformed(e);
                } else if (bottomArrowPane.isActive) {
                    bottomArrowPane.actionPerformed(e);
                } else {
                    stopTimer();
                }
            }
        };

        fScrollTimer = new Timer(kNormalDelay, fScrollListener);
        fScrollTimer.setInitialDelay(kInitialDelay);
    }

    protected final boolean scrollIfPossible(MouseEvent e, int delta) {
        int limit = Math.max(0, scrollableView.getHeight() - viewport.getExtentSize().height);
        Point vp = viewport.getViewPosition();
        int pos = vp.y + delta;
        if (pos < 0) {
            pos = 0;
        } else if (pos > limit) {
            pos = limit;
        }
        int actualDelta = pos - vp.y;
        if (actualDelta != 0) {
            scroll(e, actualDelta);
            return true;
        }
        return false;
    }

    protected void scroll(MouseEvent e, int delta) {
        Point vp = viewport.getViewPosition();
        viewport.setViewPosition(new Point(vp.x, vp.y + delta));
        configure(false);
        repaint();
    }

    protected void mouseWheelScroll(MouseWheelEvent e) {
        int direction = e.getWheelRotation() < 0 ? -1 : 1;
        double units = AquaScrollPaneUI.getUnitsToScroll(e, true);
        int delta = (int) (units * unitScroll * direction);
        scrollIfPossible(e, delta);
    }

    protected void startTimer() {
        if (originalContent instanceof AquaExtendedPopup) {
            AquaExtendedPopup cbp = (AquaExtendedPopup) originalContent;
            cbp.startArrowScroll();
        }
        fScrollTimer.setInitialDelay(kInitialDelay);
        fScrollTimer.start();
    }

    protected void stopTimer() {
        fScrollTimer.stop();
        if (originalContent instanceof AquaExtendedPopup) {
            AquaExtendedPopup cbp = (AquaExtendedPopup) originalContent;
            cbp.stopArrowScroll();
        }
    }

    @Override
    public void paintComponent(Graphics g) {

        configure(false);

        // The following code supports pop up menus with rounded corners. The key requirement is that the popup must
        // not paint near the corners.

        Border b = getBorder();
        if (b instanceof BackgroundPainter) {
            BackgroundPainter bp = (BackgroundPainter) b;
            Rectangle bounds = getBounds();
            bp.paintBackground(this, g, bounds.x, bounds.y, bounds.width, bounds.height);
        }

        super.paintComponent(g);
    }

    protected void configure(boolean forceLayout) {
        int pos = viewport.getViewPosition().y;
        boolean showTopArrow = pos > 0;
        Insets s = getInsets();
        int availableHeight = height - s.top - s.bottom;
        if (showTopArrow) {
            availableHeight -= topArrowHeight;
        }
        int limit = scrollableView.getHeight() - availableHeight;
        boolean showBottomArrow = pos < limit;
        configure(showTopArrow, showBottomArrow, forceLayout);
    }

    protected void configure(boolean showTopArrow, boolean showBottomArrow, boolean forceLayout) {
        boolean changed = false;
        if (showTopArrow != isTopArrowInstalled) {
            isTopArrowInstalled = showTopArrow;
            changed = true;
        }
        if (showBottomArrow != isBottomArrowInstalled) {
            isBottomArrowInstalled = showBottomArrow;
            changed = true;
        }
        if (changed || forceLayout) {
            // The layout must be changed immediately if scrolling is underway
            invalidate();
            validate();
            repaint();
        }
   }

    protected ArrowPane createArrowPane(boolean isTop) {
        return new ArrowPane(isTop);
    }

    protected class ArrowPane extends JPanel implements ActionListener, MouseListener {
        private final boolean isTop;
        private final Icon icon;
        private boolean isActive;

        public ArrowPane(boolean isTop) {
            super(new BorderLayout());
            setOpaque(false);
            this.isTop = isTop;
            icon = getArrowIcon(isTop);
            add(new JLabel(icon), BorderLayout.CENTER);
            setBorder(isTop ? new EmptyBorder(2, 0, 6, 0) : new EmptyBorder(6, 0, 2, 0));
            addMouseListener(this);
        }

        @Override
        public void mouseClicked(MouseEvent e) {
        }

        @Override
        public void mousePressed(MouseEvent e) {
        }

        @Override
        public void mouseReleased(MouseEvent e) {
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            isActive = true;
            startTimer();
        }

        @Override
        public void mouseExited(MouseEvent e) {
            isActive = false;
            stopTimer();
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!isActive) {
                stopTimer();
            } else {
                int delta = isTop ? -unitScroll : unitScroll;
                if (!scrollIfPossible(null, delta)) {
                    isActive = false;
                    stopTimer();
                };
            }
        }
    }

    protected Icon getArrowIcon(boolean isTop) {
        return isTop ? AquaImageFactory.getMenuUpArrowIcon() : AquaImageFactory.getMenuDownArrowIcon();
    }

    // A custom layout manager is needed to ensure that the viewport size is not changed unintentionally by the Popup,
    // because that will alter the view position.

    protected class MyLayoutManager implements LayoutManager {
        @Override
        public void addLayoutComponent(String name, Component comp) {
        }

        @Override
        public void removeLayoutComponent(Component comp) {
        }

        @Override
        public Dimension preferredLayoutSize(Container c) {
            return null;
        }

        @Override
        public Dimension minimumLayoutSize(Container c) {
            return null;
        }

        @Override
        public void layoutContainer(Container c) {
            Point position = viewport.getViewPosition();
            Insets s = c.getInsets();
            int left = s.left;
            int right = s.right;
            int top = s.top;
            int bottom = s.bottom;
            int w = width - left - right;
            int h = height - top - bottom;
            int topArrowHeight = isTopArrowInstalled ? AquaScrollingPopupMenuWrapper.this.topArrowHeight : 0;
            int bottomArrowHeight = isBottomArrowInstalled ? AquaScrollingPopupMenuWrapper.this.bottomArrowHeight : 0;
            topArrowPane.setBounds(left, top, w, topArrowHeight);
            viewport.setBounds(left, top + topArrowHeight, w, h - topArrowHeight - bottomArrowHeight);
            bottomArrowPane.setBounds(left, top + h - bottomArrowHeight, w, bottomArrowHeight);
            viewport.setViewPosition(position);
        }
    }
}
