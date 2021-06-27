/*
 * Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeListener;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.geom.ExpandableOutline;
import org.violetlib.geom.GeneralRoundRectangle;

/**
 * Manages a overlay focus ring for the current focus owner.
 */
public class AquaFocusRingPainter {
    protected static final int ANIMATION_DURATION = 100;

    protected static final float DEFAULT_CORNER = 4;

    private JComponent currentFocusRingOwner;
    private FocusRingOutlineProvider currentFocusRingProvider;
    private float animationState;

    private OverlayPainterComponent painterComponent;
    private AnimationController animationController;
    private FocusRingOutlineProvider defaultFocusRingProvider;
    private AquaCellEditorPolicy cellEditorPolicy;

    private final PropertyChangeListener myActiveChangeListener;

    public AquaFocusRingPainter() {
        painterComponent = new MyFocusRingPainterComponent();
        animationController = new AnimationController(new MyAnimation(), ANIMATION_DURATION);
        defaultFocusRingProvider = new AquaDefaultFocusRingProvider();
        cellEditorPolicy = AquaCellEditorPolicy.getInstance();
        myActiveChangeListener = ev -> {
            String name = ev.getPropertyName();
            if (AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(name)) {
                painterComponent.repaint();
            }
        };
    }

    public void setFocusOwner(@Nullable JComponent c) {
        clear();
        if (c != null) {
            currentFocusRingProvider = AquaUtils.getUI(c, FocusRingOutlineProvider.class);
            if (currentFocusRingProvider != null) {
                Shape s = currentFocusRingProvider.getFocusRingOutline(c);
                if (s != null) {
                    currentFocusRingOwner = c;
                    // Check for the special case of a component that displays a focus ring made scrollable by being
                    // contained in a scroll pane. In this case, the scroll pane displays the focus ring.
                    JScrollPane sp = AquaUtils.getScrollPaneAncestor(c);
                    if (sp != null) {
                        currentFocusRingOwner = sp;
                        currentFocusRingProvider = defaultFocusRingProvider;
                    }
                } else {
                    currentFocusRingProvider = null;
                }
            }
        }

        if (currentFocusRingProvider != null) {
            assert currentFocusRingOwner != null;
            currentFocusRingOwner.addPropertyChangeListener(myActiveChangeListener);
            painterComponent.attach(currentFocusRingOwner);
            animationState = 0;
            animationController.start();
        }
    }

    public void update() {
        if (currentFocusRingOwner != null) {
            painterComponent.repaint();
        }
    }

    protected void clear() {
        if (currentFocusRingOwner != null) {
            currentFocusRingOwner.removePropertyChangeListener(myActiveChangeListener);
            currentFocusRingOwner = null;
            currentFocusRingProvider = null;
            animationState = 0;
            animationController.stop();
            painterComponent.attach(null);
        }
    }

    protected class MyFocusRingPainterComponent extends OverlayPainterComponent {
        public MyFocusRingPainterComponent() {
            super(new Insets(100, 100, 100, 100));
        }

        @Override
        protected void internalPaint(@NotNull Graphics2D g) {
            if (currentFocusRingOwner != null && AquaFocusHandler.isActive(currentFocusRingOwner) && currentFocusRingProvider != null) {
                Shape s = currentFocusRingProvider.getFocusRingOutline(currentFocusRingOwner);
                if (s != null) {
                    Color focusColor = AquaColors.getSystemColor(currentFocusRingOwner,"keyboardFocusIndicator");
                    FocusRingPainter p = getFocusRingPainter(s);
                    p.paint(g, focusColor);
                }
            }
        }
    }

    // We need a policy of how to map from a widget outline to a pair of shapes. If the widget outline is a rectangle,
    // the outer shape should be a round rect and the inner shape should be the widget outline. Otherwise, as above.
    // Another aspect of the policy is to create a smaller focus ring if the component is being used as a cell editor.
    // Not sure how best to do that, but one idea is to base it on the existence of an AquaBorder.

    protected FocusRingPainter getFocusRingPainter(@NotNull Shape s) {
        assert currentFocusRingOwner != null;
        boolean useSmallRing = isCellEditor(currentFocusRingOwner);
        float outerOffset = useSmallRing ? 2 : 3f;
        float innerOffset = useSmallRing ? 0 : -0.5f;

        ExpandableOutline outline = ExpandableOutline.fromShape(s);
        float currentOuterOffset = outerOffset + (1 - animationState) * 10;

        Shape innerShape;
        Shape outerShape;

        // Outlines with sharp corners get a special treatment (in most cases). The outline is used as the inner shape
        // unchanged. The outer shape is a rounded rect created from the bounds of the outline.

        boolean createRoundRectOuter = false;

        if (s instanceof Rectangle2D) {
            createRoundRectOuter = !useSmallRing;
        } else if (s instanceof GeneralRoundRectangle) {
            createRoundRectOuter = true;
        }

        if (createRoundRectOuter) {
            innerShape = s;
            Rectangle2D r = s.getBounds2D();
            Shape ss = new RoundRectangle2D.Double(r.getX(), r.getY(), r.getWidth(), r.getHeight(), DEFAULT_CORNER, DEFAULT_CORNER);
            outline = ExpandableOutline.fromShape(ss);
            outerShape = outline.getShape(currentOuterOffset);
        } else {
            outerShape = outline.getShape(currentOuterOffset);
            innerShape = outline.getShape(innerOffset);
        }
        return new OutlineFocusRingPainter(innerShape, outerShape);
    }

    private class MyAnimation implements AnimationController.Animation {
        @Override
        public void setAnimationState(float a) {
            animationState = a;
            painterComponent.repaint();
        }
    }

    /**
     * Decide whether a given component should use a standard focus ring or a cell focus ring. The basic idea is that a
     * cell editor should use a smaller focus ring that does not overlap other cells.
     */
    protected boolean isCellEditor(@NotNull JComponent c) {
        return cellEditorPolicy.getCellStatus(c) != null;
    }
}
