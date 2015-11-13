/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Supports the behavior of a scroll pane using overlay scroll bars.
 */
public class AquaOverlayScrollPaneController {

    protected final Timer deactivationTimer;
    protected final AnimationController fadeOutController;

    protected JScrollBar vsb;
    protected JScrollBar hsb;
    protected JScrollBar activeScrollBar;
    protected boolean isMouseOverActiveScrollBar;

    protected final MouseListener scrollBarMouseListener;

    protected final static int FADE_OUT_TIME = 250;
    protected final static int DEACTIVATE_DELAY_TIME = 750;

    public AquaOverlayScrollPaneController() {
        deactivationTimer = new Timer(DEACTIVATE_DELAY_TIME, new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                fadeOutController.start();
            }
        });
        deactivationTimer.setRepeats(false);

        fadeOutController = new AnimationController(new FadeOutAnimation(), FADE_OUT_TIME);

        scrollBarMouseListener = new ScrollBarMouseListener();
    }

    public void dispose() {
        deactivationTimer.stop();
        if (hsb != null) {
            hsb.removeMouseListener(scrollBarMouseListener);
            hsb = null;
        }
        if (vsb != null) {
            vsb.removeMouseListener(scrollBarMouseListener);
            vsb = null;
        }
    }

    /**
     * Identify the active scroll bar.
     * @return SwingConstants.VERTICAL, SwingConstants.HORIZONTAL, or -1.
     */
    public int getActiveAxis() {
        if (activeScrollBar == vsb) {
            return SwingConstants.VERTICAL;
        }
        if (activeScrollBar == hsb) {
            return SwingConstants.HORIZONTAL;
        }
        return -1;
    }

    /**
     * Called to activate the corresponding scroll bar when the user does a wheel scroll over the scroll pane. Because
     * multiple mouse wheel events may have been queued, calls to activate may be made even after the scroll bar has
     * been activated. Such redundant calls should not schedule deactivation if the mouse is over the scroll bar.
     */
    public void activateScrollBar(MouseEvent e, boolean isVertical) {
        JScrollBar sb = isVertical ? vsb : hsb;

        boolean isNew = sb != activeScrollBar;

        if (activeScrollBar != sb && activeScrollBar != null) {
            deactivateImmediately();
        }

        activeScrollBar = sb;

        if (activeScrollBar != null) {
            activeScrollBar.setVisible(true);

            Rectangle scrollBarBounds = activeScrollBar.getBounds();
            isMouseOverActiveScrollBar = scrollBarBounds.contains(e.getX(), e.getY());

            if (isNew) {
                AquaScrollBarUI ui = AquaUtils.getUI(activeScrollBar, AquaScrollBarUI.class);
                if (ui != null) {
                    ui.setRolloverDisplayState(isMouseOverActiveScrollBar);
                }
            }

            if (!isMouseOverActiveScrollBar) {
                scheduleOrPostponeDeactivation();
            }
        }
    }

    public void setVerticalScrollBar(JScrollBar sb) {
        if (sb != vsb) {
            if (vsb != null) {
                disconnect(vsb);
            }
            vsb = sb;
            disconnect(vsb);
            if (vsb != null) {
                initialize(vsb, ScrollPaneConstants.VERTICAL_SCROLLBAR);
            }
        }
    }

    public void setHorizontalScrollBar(JScrollBar sb) {
        if (sb != hsb) {
            if (hsb != null) {
                disconnect(hsb);
            }
            hsb = sb;
            if (hsb != null) {
                initialize(hsb, ScrollPaneConstants.HORIZONTAL_SCROLLBAR);
            }
        }
    }

    protected void disconnect(JScrollBar sb) {
        sb.removeMouseListener(scrollBarMouseListener);
        sb.setVisible(false);
        if (sb == activeScrollBar) {
            isMouseOverActiveScrollBar = false;
            activeScrollBar = null;
        }
    }

    protected void initialize(JScrollBar sb, String which) {
        reconfigure(sb, which);
        sb.addMouseListener(scrollBarMouseListener);
        sb.setVisible(false);
        AquaScrollBarUI ui = AquaUtils.getUI(sb, AquaScrollBarUI.class);
        if (ui != null) {
            ui.setRolloverDisplayState(false);
            ui.setAlpha(1);
        }
    }

    protected void reconfigure(JScrollBar sb, String which) {
    }

    /**
     * If there is an active (visible) scroll bar that is not already scheduled to be deactivated, schedule its
     * deactivation after the standard delay time. If deactivation has already been scheduled, reschedule it to happen
     * after the standard delay time starting now. If the scroll bar is in the process of deactivating, cancel the
     * deactivation process and restore the scroll bar to its fully visible state.
     */
    public void scheduleOrPostponeDeactivation() {
        deactivationTimer.stop();
        fadeOutController.stop();
        setAlpha(activeScrollBar, 1);
        deactivationTimer.start();
    }

    /**
     * If there is an active (visible) scroll bar in the process of deactivating or scheduled for future deactivation,
     * cancel the deactivation request and restore the scroll bar to its fully visible state.
     */
    public void cancelDeactivation() {
        deactivationTimer.stop();
        fadeOutController.stop();
        setAlpha(activeScrollBar, 1);
    }

    /**
     * If there is an active (visible) scroll bar, deactivate it immediately.
     */
    public void deactivateImmediately() {
        deactivationTimer.stop();
        fadeOutController.stop();
        if (activeScrollBar != null) {
            activeScrollBar.setVisible(false);
            AquaScrollBarUI ui = AquaUtils.getUI(activeScrollBar, AquaScrollBarUI.class);
            if (ui != null) {
                ui.setRolloverDisplayState(false);
                ui.setAlpha(1);
            }
            activeScrollBar = null;
        }
    }

    protected class FadeOutAnimation implements AnimationController.Animation {
        @Override
        public void setAnimationState(float a) {
            if (activeScrollBar != null) {
                AquaScrollBarUI ui = AquaUtils.getUI(activeScrollBar, AquaScrollBarUI.class);
                if (ui != null) {
                    if (ui.isDragging()) {
                        scheduleOrPostponeDeactivation();
                        return;
                    }
                    setAlpha(activeScrollBar, 1-a);
                }
            }
        }
    }

    protected void setAlpha(JScrollBar sb, float alpha) {
        if (sb != null) {
            if (alpha == 0) {
                deactivateImmediately();
            } else {
                AquaScrollBarUI ui = AquaUtils.getUI(sb, AquaScrollBarUI.class);
                if (ui != null) {
                    ui.setAlpha(alpha);
                }
            }
        }
    }

    protected class ScrollBarMouseListener extends MouseAdapter {
        @Override
        public void mouseEntered(MouseEvent e) {
            JScrollBar sb = getScrollBar(e);
            if (sb == activeScrollBar) {
                cancelDeactivation();
                isMouseOverActiveScrollBar = true;
            }
        }

        @Override
        public void mouseExited(MouseEvent e) {
            JScrollBar sb = getScrollBar(e);
            if (sb == activeScrollBar) {
                scheduleOrPostponeDeactivation();
                isMouseOverActiveScrollBar = false;
            }
        }

        protected JScrollBar getScrollBar(MouseEvent e) {
            Component c = e.getComponent();
            if (c instanceof JScrollBar) {
                return (JScrollBar) c;
            }
            return null;
        }
    }
}
