/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.*;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A model of a segmented control. A segmented control model is created as needed when there are potential interactions
 * among the buttons of a segmented control. The buttons must be horizontally adjacent toggle buttons within a common
 * parent. At least two buttons are required, because with fewer buttons there is no need for a model. Each button must
 * have an appropriate style and position property.
 */

public class SegmentedControlModel {

    protected static final Map<Container,SegmentedControlModel> parentCache = new HashMap<>();

    private final @NotNull Container parent;
    private final @NotNull JToggleButton @NotNull [] buttons;
    private final @Nullable ButtonGroup group;
    private boolean isValid = true;

    private final @NotNull ContainerListener myContainerListener;
    private final @NotNull ComponentListener myComponentListener;
    private final @NotNull PropertyChangeListener myPropertyChangeListener;

    private SegmentedControlModel(@NotNull Container parent,
                                  @NotNull JToggleButton @NotNull [] buttons,
                                  @Nullable ButtonGroup group) {
        this.parent = parent;
        this.buttons = buttons;
        this.group = group;

        parentCache.put(parent, this);

        // If a button is added to or removed from the parent, relocated, or its UI or related client properties change,
        // mark this model as invalid to force a recomputation. Unfortunately, there is no way to be notified of
        // changes in the button groups.

        myComponentListener = new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                invalidate();
            }

            @Override
            public void componentMoved(ComponentEvent e) {
                invalidate();
            }
        };

        myContainerListener = new ContainerAdapter() {
            @Override
            public void componentAdded(@NotNull ContainerEvent e) {
                update(e);
            }

            @Override
            public void componentRemoved(@NotNull ContainerEvent e) {
                update(e);
            }

            void update(@NotNull ContainerEvent e) {
                Component child = e.getChild();
                if (identifySegmentedButton(child) != null) {
                    invalidate();
                }
            }
        };

        myPropertyChangeListener = e -> {
            String name = e.getPropertyName();
            if (name != null) {
                if (name.equals("UI")
                        || name.equals(AquaButtonUI.BUTTON_TYPE)
                        || name.equals(AquaButtonUI.SEGMENTED_BUTTON_POSITION)) {
                    invalidate();
                }
            }
        };

        parent.addContainerListener(myContainerListener);
        for (JToggleButton b : buttons) {
            b.addComponentListener(myComponentListener);
            b.addPropertyChangeListener(myPropertyChangeListener);
        }
    }

    public @NotNull Container getParent() {
        return parent;
    }

    public int count() {
        return buttons.length;
    }

    public @NotNull JToggleButton get(int i) {
        return buttons[i];
    }

    public @Nullable ButtonGroup getGroup() {
        return group;
    }

    public @Nullable JToggleButton getLeftAdjacentButton(@NotNull JToggleButton button) {
        int count = count();
        for (int i = 0; i < count; i++) {
            JToggleButton b = get(i);
            if (b == button) {
                return i > 0 ? get(i-1) : null;
            }
        }
        return null;
    }

    public @Nullable JToggleButton getRightAdjacentButton(@NotNull JToggleButton button) {
        int count = count();
        for (int i = 0; i < count; i++) {
            JToggleButton b = get(i);
            if (b == button) {
                return i < count-1 ? get(i+1) : null;
            }
        }
        return null;
    }

    /**
     * Indicate whether this model is still valid. If the model is not valid, a new model must be created (if possible).
     * Invalidation is largely driven by events, but some checking may be performed by this method.
     */

    public boolean isValid() {
        if (!isValid) {
            return false;
        }

        if (buttonGroupsHaveChanged()) {
            invalidate();
            return false;
        }

        return true;
    }

    private void invalidate() {
        if (isValid) {
            isValid = false;
            parent.removeContainerListener(myContainerListener);
            for (JToggleButton b : buttons) {
                b.removeComponentListener(myComponentListener);
                b.removePropertyChangeListener(myPropertyChangeListener);
            }
            parentCache.remove(parent);
        }
    }

    private boolean buttonGroupsHaveChanged() {
        for (JToggleButton b : buttons) {
            ButtonGroup g = getButtonGroup(b);
            if (g != group) {
                return true;
            }
        }
        return false;
    }

    public void detach() {
        invalidate();
    }

    /**
     * Return the segmented control model for a button, if any. If a cached segmented control model is found, it
     * is checked to ensure that it is still valid. If not valid, it is discarded, and a new model is created, if
     * possible. Buttons are reconfigured if the old model is discarded and/or a new model is created.
     *
     * @param button The button.
     *
     * @return the valid segmented control model for the button, or null if none.
     */
    public static @Nullable SegmentedControlModel getSegmentedControlModel(@NotNull AbstractButton button) {
        Set<JToggleButton> buttonsNeedingReconfiguration = new HashSet<>();
        SegmentedControlModel m = getCachedSegmentedControlModel(button);
        if (m != null) {
            if (m.isValid()) {
                return m;
            }
            int count = m.count();
            for (int i = 0; i < count; i++) {
                buttonsNeedingReconfiguration.add(m.get(i));
            }
        }
        // If the existing model is not valid, it will have detached itself and cleared the cache.
        SegmentedControlModel model = null;
        Container parent = button.getParent();
        if (parent != null) {
            model = createModel(parent);
            if (model != null) {
                int count = model.count();
                for (int i = 0; i < count; i++) {
                    buttonsNeedingReconfiguration.add(model.get(i));
                }
            }
        }

        for (JToggleButton b : buttonsNeedingReconfiguration) {
            AquaButtonUI ui = AquaUtils.getUI(b, AquaButtonUI.class);
            if (ui != null) {
                ui.configure(b);
            }
        }

        return model;
    }

    private static @Nullable SegmentedControlModel getCachedSegmentedControlModel(@NotNull AbstractButton b) {
        Container parent = b.getParent();
        if (parent != null) {
            return parentCache.get(parent);
        }
        return null;
    }

    /**
     * Create a segmented control model for the buttons in the specified container.
     * @param parent The container.
     * @return the model, or null if the conditions for creating a valid model are not met.
     */
    private static @Nullable SegmentedControlModel createModel(@NotNull Container parent) {
        List<JToggleButton> buttons = new ArrayList<>();
        int count = parent.getComponentCount();
        for (int i = 0; i < count; i++) {
            Component c = parent.getComponent(i);
            JToggleButton b = identifySegmentedButton(c);
            if (b != null) {
                buttons.add(b);
            }
        }
        if (buttons.size() > 1) {
            buttons.sort(new LocationComparator());
            if (buttonsAreAdjacent(buttons) && hasUniformStyle(buttons) && hasValidPositions(buttons)) {
                JToggleButton[] buttonArray = buttons.toArray(new JToggleButton[0]);
                ButtonGroup group = identifyButtonGroup(buttons);
                return new SegmentedControlModel(parent, buttonArray, group);
            }
        }
        return null;
    }

    private static @Nullable JToggleButton identifySegmentedButton(@NotNull Component c) {
        if (c instanceof JToggleButton) {
            JToggleButton b = (JToggleButton) c;
            if (b.getUI().getClass() == AquaButtonToggleUI.class
                    && AquaButtonExtendedTypes.getValidSegmentPosition(b) != null) {
                return b;
            }
        }
        return null;
    }

    private static class LocationComparator implements Comparator<JToggleButton> {
        @Override
        public int compare(JToggleButton o1, JToggleButton o2) {
            int x1 = o1.getX();
            int x2 = o2.getX();
            return x1 - x2;
        }
    }

    private static boolean buttonsAreAdjacent(@NotNull List<JToggleButton> buttons) {
        int edge = -1000;
        int count = buttons.size();
        for (int i = 0; i < count; i++) {
            JToggleButton b = buttons.get(i);
            int x = b.getX();
            if (edge != -1000 && x != edge) {
                return false;
            }
            edge = x + b.getWidth();
        }
        return true;
    }

    private static boolean hasUniformStyle(@NotNull List<JToggleButton> buttons) {
        String style = null;
        for (JToggleButton b : buttons) {
            String buttonStyle = getButtonStyle(b);
            if (style != null && !style.equals(buttonStyle)) {
                return false;
            }
            style = buttonStyle;
        }
        return true;
    }

    private static @NotNull String getButtonStyle(@NotNull JToggleButton b) {
        Object buttonTypeProperty = b.getClientProperty(AquaButtonUI.BUTTON_TYPE);
        if (buttonTypeProperty instanceof String) {
            return (String) buttonTypeProperty;
        }
        return "segmented";
    }

    private static boolean hasValidPositions(@NotNull List<JToggleButton> buttons) {
        int count = buttons.size();
        assert count > 1;
        String firstPosition = AquaButtonExtendedTypes.getValidSegmentPosition(buttons.get(0));
        if (!"first".equals(firstPosition)) {
            return false;
        }
        String lastPosition = AquaButtonExtendedTypes.getValidSegmentPosition(buttons.get(count-1));
        if (!"last".equals(lastPosition)) {
            return false;
        }
        for (int i = 1; i < count-1; i++) {
            String position = AquaButtonExtendedTypes.getValidSegmentPosition(buttons.get(i));
            if (!"middle".equals(position)) {
                return false;
            }
        }
        return true;
    }

    private static @Nullable ButtonGroup identifyButtonGroup(@NotNull List<JToggleButton> buttons) {
        ButtonGroup group = null;
        for (JToggleButton b : buttons) {
            ButtonGroup buttonGroup = getButtonGroup(b);
            if (buttonGroup == null || (group != null && group != buttonGroup)) {
                return null;
            }
            group = buttonGroup;
        }
        return group;
    }

    private static @Nullable ButtonGroup getButtonGroup(@NotNull JToggleButton b) {
        ButtonModel m = b.getModel();
        if (m instanceof DefaultButtonModel) {
            DefaultButtonModel d = (DefaultButtonModel) m;
            return d.getGroup();
        }
        return null;
    }
}
