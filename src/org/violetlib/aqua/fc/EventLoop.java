/*
 * @(#)EventLoop.java  1.4  2009-06-01
 *
 * Copyright (c) 2001-2013 Werner Randelshofer, Switzerland.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.util.LinkedList;

/**
 * An EventLoop can process events on a separate worker thread.
 * It consists of two parts: the event collector and the event
 * processor.
 * <p>
 * The event collector collects all incoming events and puts them
 * into a queue.<br>
 * The event processor removes the events from the queue and
 * processes them.
 * <p>
 * The key feature of the EventLoop is, that clients don't have
 * to wait until an event has been processed. Clients are free
 * to proceed as soon as the collector has put the event into
 * the queue.
 * <p>
 * Other important features are that events are processed in
 * the same sequence as they have been collected and that only one
 * thread is used to process the events.
 * <p>
 * <b>Usage</b>
 * <p>
 * This is an abstract class. It does all the queue handling, but
 * does no processing. To use it, you have to create a subclass
 * which overrides the methods #collectEvent and #processEvent.
 * <p>
 * <b>Example</b>
 * <p>
 * An EventLoop, which outputs Strings on a background thread
 * could look like this:
 * <pre><tt>
 * public class AsyncDisplay
 * extends AbstractEventLoop {
 *     public void display(String string) {
 *         collectEvent(string);
 *     }
 *     protected void processEvent(Object event) {
 *          System.out.println((String) event);
 *    }
 * }
 * </tt></pre>
 * <p>
 * To use the class proceed like this:
 * <p><pre><tt>
 * AsyncDisplay a = new AsyncDisplay();
 *  a.display("Hello World");
 * </tt></pre>
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public abstract class EventLoop {
    private Thread eventProcessor;
    private int threadPriority;
    private final LinkedList eventQueue = new LinkedList();
    /**
     * Indicates whether multiple events will be coalesced
     * by the event processor or not.
     */
    private boolean isCoalesce;
    /**
     * Indicates whether events will be processed by the
     * event processor or not.
     */
    private volatile boolean isAlive = true;
    /**
     * Indicates whether events shall be enqueued add the beginning
     * of the queue.
     */
    private volatile boolean isLIFO = false;
    /**
     * Creates a new EventLoop which processes events at Thread.NORM_PRORITY.
     */
    public EventLoop() {
        this(Thread.NORM_PRIORITY);
    }
    /**
     * Creates a new EventLoop which processes events at the
     * desired thread priority.
     *
     * @param priority The Thread priority of the event processor.
     */
    public EventLoop(int priority) {
        this.threadPriority = priority;
    }
    /**
     * Collects an event and puts it into the event queue
     * for later processing.
     *
     * @param event The event to be put into the queue.
     */
    protected void collectEvent(Object event) {
        synchronized(eventQueue) {
            if (! isCoalesce || ! eventQueue.contains(event)) {
                if (isLIFO) {
                    eventQueue.addFirst(event);
                } else {
                    eventQueue.addLast(event);
                }
                if (isAlive) startProcessor();
            }
        }
    }

    /**
     * Sets whether the EventLoop coalesces multiple pending events.
     * A busy application may not be able to keep up with event
     * generation, causing multiple events to be queued.
     * Coalescing is based on equality tests of event objects.
     * More formally, coalesces an event o if and only if the queue
     * contains at least one element e such that
     * <code>(o==null ? e==null : o.equals(e))</code>.
     * <p>
     * EventLoops do not coalesce events by default.
     *
     * @param b Specify true to turn on coalescing.
     */
    public void setCoalesce(boolean b) {
        isCoalesce = b;
    }
    /**
     * Returns true if the EventLoop
     * coalesces multiple pending events.
     *
     * @see #setCoalesce(boolean)
     */
    public boolean isCoalesce() {
        return isCoalesce;
    }
    /**
     * Sets whether the EventLoop shall enqueue events at the beginning of the
     * queue instead of at the end of the queue.
     *
     * @param b Specify true to enqueue events at the beginning of the queue.
     */
    public void setLIFO(boolean b) {
        isLIFO = b;
    }
    /**
     * Returns true if the EventLoop
     * coalesces multiple pending events.
     *
     * @see #setCoalesce(boolean)
     */
    public boolean isLIFO() {
        return isLIFO;
    }

    /**
     * Starts the event processor.
     * <br>The event processor is started by default.
     */
    public void start() {
        synchronized(eventQueue) {
            isAlive = true;
            startProcessor();
        }
    }

    /**
     * Stops the event processor.
     */
    public void stop() {
        synchronized (eventQueue) {
            isAlive = false;
        }
    }

    /**
     * Clears the event queue.
     */
    public void clear() {
        synchronized(eventQueue) {
            eventQueue.clear();
        }
    }

    public void dispose() {
        stop();
        clear();
    }

    /**
     * This is the method which really starts the processor.
     */
    private void startProcessor() {
        synchronized(eventQueue) {
            if (eventProcessor == null) {
                eventProcessor = new Thread(this+" Event Processor") {
                    public void run() {
                        processEvents();
                    }
                };
                try {
                    // The event processor must not be a daemon.
                    // If it was a daemon, the virtual machine would
                    // stop before the event had been processed.
                    eventProcessor.setDaemon(false);
                } catch (SecurityException e) {}
                try {
                    eventProcessor.setPriority(threadPriority);
                } catch (SecurityException e) {}
                eventProcessor.start();
            }
        }
    }

    /**
     * This method processes an event on the event processor thread.
     *
     * @param event An event from the queue.
     */
    protected abstract void processEvent(Object event);

    /**
     * This method removes events from the event queue
     * and proceses them until the queue is empty or
     * until #stop is called.
     * <p>
     * This method must be called from the event processor
     * thread only.
     */
    protected void processEvents() {
        Object event;
        while (true) {
            synchronized(eventQueue) {
                if (eventQueue.isEmpty() || ! isAlive) {
                    eventProcessor = null;
                    return;
                }
                event = eventQueue.removeFirst();
            }
            try {
                processEvent(event);
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
    }
}
