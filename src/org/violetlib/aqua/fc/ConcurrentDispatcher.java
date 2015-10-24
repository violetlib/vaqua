/*
 * @(#)ConcurrentDispatcher.java  2.1  2009-06-01
 *
 * Copyright (c) 2002-2010 Werner Randelshofer, Switzerland
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.util.LinkedList;

/**
 * Processes Runnable objects concurrently on a pool of processor threads.
 * The order in which the runnable objects are processed is not
 * necesseraly the same in which they were added to the dispatcher.
 * There is one thread pool per instance.
 * <p>
 * Design pattern used: Acceptor
 * Role in design pattern: EventCollector and EventProcessor
 * <p>
 * <b>Example</b>
 * <br>The following program prints "Hello World" on the
 * processor thread:
 * <pre>
 * // Create the Dispatcher.
 * ConcurrentDispatcher dispatcher = new ConcurrentDispatcher();
 *
 * // Create the Runnable object.
 * Runnable runner = new Runnable() {
 *     public void run()  {
 *         System.out.println("Hello World");
 *     }
 * };
 *
 * // Execute the Runnable objekt using the dispatcher.
 * dispatcher.dispatch(runner);
 * </pre>
 *
 * @author  Werner Randelshofer, Switzerland
 * @version 2.1 2009-06-01 Added dispose method.
 * <br>2.0 2002-04-07 dispatchLIFO added.
 * <br>1.0 2002-05-18 Created.
 */
public class ConcurrentDispatcher {

    /**
     * The priority of the processor thread.
     */
    private int priority;
    /**
     * The queue stores the events until they
     * can be processed by a processor thread.
     */
    private final LinkedList queue = new LinkedList();
    /**
     * Number of concurrent threads.
     */
    private int threadCount;
    /**
     * Maximum number of concurrent threads.
     */
    private int maxThreadCount;
    /**
     * Set the policy to enqueue the runnable
     * for later execution if there are no available
     * threads in the pool.
     */
    public static final int ENQUEUE_WHEN_BLOCKED = 0;
    /**
     * Set the policy for blocked execution to be that
     * the current thread executes the command if there
     * are no available threads in the pool.
     */
    public static final int RUN_WHEN_BLOCKED = 1;
    /**
     * The policy used when the maximal number of
     * threads is reached.
     */
    private int blockingPolicy = ENQUEUE_WHEN_BLOCKED;

    /**
     * Creates a new ConcurrentDispatcher and
     * sets the priority of the processor thread to
     * java.lang.Thread.NORM_PRIORITY and with
     * up to five concurrent threads in the thread
     * pool.
     */
    public ConcurrentDispatcher() {
        this(Thread.NORM_PRIORITY, 5);
    }

    /**
     * Creates a new ConcurrentDispatcher.
     *
     * @param priority       The priority of the processor
     *                       thread.
     * @param maxThreadCount The maximal number of concurrent
     *                       threads in the thread pool.
     */
    public ConcurrentDispatcher(int priority, int maxThreadCount) {
        this.priority = priority;
        this.maxThreadCount = maxThreadCount;
    }

    /**
     * Sets the maximum number of concurrent threads.
     *
     * @param maxThreadCount Maximal number of concurrent threads.
     *                       A value of zero or below zero stops the dispatcher
     *                       when the queue is empty.
     */
    public void setMaxThreadCount(int maxThreadCount) {
        this.maxThreadCount = maxThreadCount;
    }

    /**
     * Returns the maximal number of concurrent threads.
     */
    public int getMaxThreadCount() {
        return maxThreadCount;
    }

    /**
     * Enqueues the Runnable object, and executes
     * it on a processor thread.
     */
    public void dispatch(Runnable runner) {
        dispatch(runner, false);
    }

    /**
     * Enqueues the Runnable object, and executes
     * it on a processor thread.
     */
    public void dispatch(Runnable runner, boolean isLIFO) {
        isLIFO = false;
        synchronized (queue) {
            if (threadCount < maxThreadCount) {
                if (isLIFO) {
                    queue.addFirst(runner);
                } else {
                    queue.addLast(runner);
                }

                Thread processor = new Thread(this + " Processor") {

                    public void run() {
                        processEvents();
                    }
                };
                threadCount++;


                // The processor thread must not be a daemon,
                // or else the Java VM might stop before
                // all runnables have been processed.
                try {
                    processor.setDaemon(false);
                } catch (SecurityException e) {
                    e.printStackTrace();
                }
                try {
                    processor.setPriority(priority);
                } catch (SecurityException e) {
                    e.printStackTrace();
                }

                processor.start();
                return;

            } else if (blockingPolicy == ENQUEUE_WHEN_BLOCKED) {
                if (isLIFO) {
                    queue.addFirst(runner);
                } else {
                    queue.addLast(runner);
                }

                return;
            }
        }

        //implicit: if (threadCount >= maxThreadCount && blockingPolicy == RUN_WHEN_BLOCKED)
        runner.run();
    }

    public void stop() {

    }

    /**
     * This method dequeues all Runnable objects from the
     * queue and executes them. The method returns
     * when the queue is empty.
     */
    protected void processEvents() {
        Object runner;
        loop:
        while (true) {
            synchronized (queue) {
                if (queue.isEmpty()) {
                    threadCount--;
                    break loop;
                }
                runner = queue.removeFirst();
            }
            try {
                ((Runnable) runner).run();
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Disposes the dispatcher and all associated processes.
     */
    public void dispose() {
        synchronized (queue) {
            queue.clear();
        }
    }
}
