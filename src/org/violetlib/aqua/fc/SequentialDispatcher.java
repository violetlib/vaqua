/*
 * @(#)SequentialDispatcher.java  1.0  2002-05-18
 *
 * Copyright (c) 2001-2013 Werner Randelshofer, Switzerland.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of
 * Werner Randelshofer. ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Werner Randelshofer.
 */

package org.violetlib.aqua.fc;

/**
 * Processes Runnable objects sequentially on a processor thread.
 * The order in which the runnable objects are processed is
 * the same in which they were added to the dispatcher.
 * <p>
 * Design pattern used: Acceptor
 * Role in design pattern: EventCollector and EventProcessor
 *
 * @author  Werner Randelshofef
 * @version 1.0 2002-05-18 Created
 */
public class SequentialDispatcher extends EventLoop {
    /**
     * Creates new SequentialDispatcher which processes Runnable objects
     * at java.lang.Thread.NORM_PRIORITY.
     */
    public SequentialDispatcher() {
    }
    /**
     * Creates a new SequentialDispatcher which processes Runnable Objects
     * at the desired thread priority.
     *
     * @param priority The Thread priority of the event processor.
     */
    public SequentialDispatcher(int priority) {
        super(priority);
    }

    /**
     * This method processes an event on the event processor thread.
     *
     * @param event An event from the queue.
     */
    protected void processEvent(Object event) {
        Runnable r = (Runnable) event;
        r.run();
    }

    /**
     * Queues the Runnable object for later execution on the
     * processor thread.
     */
    public void dispatch(Runnable r) {
        collectEvent(r);
    }
}
