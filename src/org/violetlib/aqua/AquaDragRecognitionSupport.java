/*
 * Copyright (c) 2005, 2008, Oracle and/or its affiliates. All rights reserved.
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

import javax.swing.*;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragSource;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;

/**
 * Drag recognition support. Copied from DragRecognitionSupport because we need it to write drag gesture recognition
 * code but it is package private.
 */
public class AquaDragRecognitionSupport {
    private int motionThreshold;
    private MouseEvent dndArmedEvent;
    private JComponent component;

    private static AquaDragRecognitionSupport instance;

    /**
     * This interface allows us to pass in a handler to mouseDragged,
     * so that we can be notified immediately before a drag begins.
     */
    public interface BeforeDrag {
        void dragStarting(MouseEvent me);
    }

    /**
     * Returns the AquaDragRecognitionSupport for the caller's context.
     */
    private static AquaDragRecognitionSupport getDragRecognitionSupport() {
        if (instance == null) {
            instance = new AquaDragRecognitionSupport();
        }
        return instance;
    }

    /**
     * Returns whether or not the event is potentially part of a drag sequence.
     */
    public static boolean mousePressed(MouseEvent me) {
        return getDragRecognitionSupport().mousePressedImpl(me);
    }

    /**
     * If a dnd recognition has been going on, return the MouseEvent
     * that started the recognition. Otherwise, return null.
     */
    public static MouseEvent mouseReleased(MouseEvent me) {
        return getDragRecognitionSupport().mouseReleasedImpl(me);
    }

    /**
     * Returns whether or not a drag gesture recognition is ongoing.
     */
    public static boolean mouseDragged(MouseEvent me, BeforeDrag bd) {
        return getDragRecognitionSupport().mouseDraggedImpl(me, bd);
    }

    private void clearState() {
        dndArmedEvent = null;
        component = null;
    }

    public static int mapDragOperationFromModifiers(MouseEvent me, TransferHandler th) {

        if (th == null || !SwingUtilities.isLeftMouseButton(me)) {
            return TransferHandler.NONE;
        }

        return convertModifiersToDropAction(me.getModifiersEx(), th.getSourceActions((JComponent) me.getSource()));
    }

    /**
     * Returns whether or not the event is potentially part of a drag sequence.
     */
    private boolean mousePressedImpl(MouseEvent me) {
        component = (JComponent)me.getSource();

        if (mapDragOperationFromModifiers(me, component.getTransferHandler()) != TransferHandler.NONE) {
            motionThreshold = DragSource.getDragThreshold();
            dndArmedEvent = me;
            return true;
        }

        clearState();
        return false;
    }

    /**
     * If a dnd recognition has been going on, return the MouseEvent
     * that started the recognition. Otherwise, return null.
     */
    private MouseEvent mouseReleasedImpl(MouseEvent me) {
        /* no recognition has been going on */
        if (dndArmedEvent == null) {
            return null;
        }

        MouseEvent retEvent = null;

        if (me.getSource() == component) {
            retEvent = dndArmedEvent;
        } // else component has changed unexpectedly, so return null

        clearState();
        return retEvent;
    }

    /**
     * Returns whether or not a drag gesture recognition is ongoing.
     */
    private boolean mouseDraggedImpl(MouseEvent me, BeforeDrag bd) {
        /* no recognition is in progress */
        if (dndArmedEvent == null) {
            return false;
        }

        /* component has changed unexpectedly, so bail */
        if (me.getSource() != component) {
            clearState();
            return false;
        }

        int dx = Math.abs(me.getX() - dndArmedEvent.getX());
        int dy = Math.abs(me.getY() - dndArmedEvent.getY());
        if (Math.sqrt(dx*dx+dy*dy) > motionThreshold) {
            TransferHandler th = component.getTransferHandler();
            int action = mapDragOperationFromModifiers(me, th);
            if (action != TransferHandler.NONE) {
                /* notify the BeforeDrag instance */
                if (bd != null) {
                    bd.dragStarting(dndArmedEvent);
                }
                th.exportAsDrag(component, dndArmedEvent, action);
                clearState();
            }
        }

        return true;
    }

    // Copied from SunDragSourceContextPeer
    private static int convertModifiersToDropAction(int modifiersEx, int sourceActions) {
        int dropAction;
        if ( 0 != (modifiersEx & (InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK)))  {
            dropAction = DnDConstants.ACTION_COPY & sourceActions;
            if (dropAction == DnDConstants.ACTION_NONE) {
                dropAction = DnDConstants.ACTION_MOVE & sourceActions;
            }
        } else if ( 0 != (modifiersEx & (InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK)) &&
                0 != (modifiersEx & (InputEvent.META_DOWN_MASK)))  {
            dropAction = DnDConstants.ACTION_LINK & sourceActions;
            if (dropAction == DnDConstants.ACTION_NONE) {
                dropAction = DnDConstants.ACTION_MOVE & sourceActions;
            }
        } else {
            dropAction = DnDConstants.ACTION_MOVE & sourceActions;
            if (dropAction == DnDConstants.ACTION_NONE) {
            dropAction = DnDConstants.ACTION_COPY & sourceActions;
            }
        }
        return dropAction;
    }
}
