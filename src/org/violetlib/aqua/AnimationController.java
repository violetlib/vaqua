/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Timer;

/**
 * A real time animation controller.
 */
public class AnimationController {

    public static final int MINIMUM_DURATION = 100;             // minimum effective animation time in milliseconds
    public static final int MINIMUM_REPAINT_INTERVAL = 20;      // in milliseconds

    public interface Animation {

        /**
         * Update the state of the animation.
         * @param a A value between zero and one where zero represents the initial state and one represents the final
         *          state.
         */
        void setAnimationState(float a);
    }

    protected final Animation animation;
    protected final int desiredRepaintInterval;
    protected final int defaultDuration;

    private long startTime = 0;
    private long endTime = 0;
    private float duration;
    private Timer timer;

    /**
     * Create a controller for an animation.
     * @param animation The animation.
     * @param defaultDuration The default duration of the animation, in milliseconds.
     */
    public AnimationController(Animation animation, int defaultDuration) {
        this(animation, MINIMUM_REPAINT_INTERVAL, defaultDuration);
    }

    /**
     * Create a controller for an animation.
     * @param animation The animation.
     * @param repaintInterval The desired repaint interval, in milliseconds.
     * @param defaultDuration The default duration of the animation, in milliseconds.
     */
    public AnimationController(Animation animation, int repaintInterval, int defaultDuration) {
        this.animation = animation;
        this.desiredRepaintInterval = Math.max(MINIMUM_REPAINT_INTERVAL, repaintInterval);
        this.defaultDuration = Math.max(0, defaultDuration);
    }

    /**
     * Start or restart the animation using the default duration.
     */
    public void start() {
        start(defaultDuration);
    }

    /**
     * Start or restart the animation.
     * @param duration The duration of the animation. If the specified duration is too small, the animation
     *   state is immediately set to the final state.
     */
    public synchronized void start(int duration) {
        if (animation != null) {
            if (duration < MINIMUM_DURATION) {
                animation.setAnimationState(1);
            } else {
                animation.setAnimationState(0);
                startTime = System.currentTimeMillis();
                endTime = startTime + duration;
                this.duration = duration;
                if (timer == null) {
                    timer = new Timer(desiredRepaintInterval, new MyTimerAction());
                } else {
                    timer.stop();
                }
                timer.start();
            }
        }
    }

    /**
     * Stop the current animation, if any.
     */
    public synchronized void stop() {
        if (timer != null) {
            timer.stop();
        }
        startTime = 0;
        endTime = 0;
        duration = 0;
    }

    /**
     * Indicate whether there is a currently running animation.
     * @return true if and only if there is a currently running animation.
     */

    public synchronized boolean isRunning() {
        return startTime > 0;
    }

    /**
     * This method is called from the timer to update the animation state.
     */
    private synchronized void timerUpdate() {

        if (timer != null) {
            if (animation != null && startTime > 0) {
                long time = System.currentTimeMillis();
                if (time >= endTime) {
                    animation.setAnimationState(1);
                } else if (time >= startTime) {
                    float animationState = (time - startTime) / duration;
                    animation.setAnimationState(animationState);
                    return;
                }
            }

            timer.stop();
        }
    }

    private class MyTimerAction implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            timerUpdate();
        }
    }
}
