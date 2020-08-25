/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashSet;
import java.util.Set;
import javax.swing.*;
import javax.swing.plaf.ColorUIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.violetlib.aqua.AquaFocusHandler.FRAME_ACTIVE_PROPERTY;

/**
 * This class supports the association of appearances with components.
 */

public class AppearanceManager {

    public static final String AQUA_APPEARANCE_NAME_KEY = "Aqua.appearanceName";    // the name of an explicitly chosen appearance
    public static final String AQUA_APPEARANCE_KEY = "Aqua.appearance";             // the effective appearance

    private static final AppearanceManagerHierarchyListener hierarchyListener = new AppearanceManagerHierarchyListener();
    private static final ActiveStateListener activeStateListener = new ActiveStateListener();

    private static @Nullable AquaAppearance currentAppearance;

    /**
     * Return the current appearance. This method is for the very unusual cases where a component is painted that is
     * not in the component hierarchy.
     * @return the current appearance, or a default appearance if no current appearance has been registered.
     */

    public static @NotNull AquaAppearance getCurrentAppearance() {
        if (currentAppearance != null) {
            return currentAppearance;
        } else {
            return AquaAppearances.getDefaultAppearance();
        }
    }

    private static Set<Class> notifiedClasses = new HashSet<>();

    public static @Nullable AquaAppearance registerCurrentAppearance(@NotNull JComponent c) {
        AquaAppearance appearance = getRegisteredAppearance(c);
        if (appearance != null && appearance != currentAppearance) {
            currentAppearance = appearance;
            if (false) {
                // debug
                System.err.println("Current appearance changed to: " + currentAppearance);
            }
        } else if (appearance == null) {
            Class clazz = c.getClass();
            if (!notifiedClasses.contains(clazz)) {
                notifiedClasses.add(clazz);
                System.err.println("No appearance for: " + clazz.getName() + " should ensure appearance?");
            }
        }
        return currentAppearance;
    }

    public static void restoreCurrentAppearance(@Nullable AquaAppearance appearance) {
        if (appearance != currentAppearance) {
            currentAppearance = appearance;
            System.err.println("Restored appearance: " + currentAppearance);
        }
    }

    /**
     * Install event listeners to help manage the appearance property of the specified component.
     * @param c The component.
     * @throws IllegalArgumentException if {@code c} does not support {@link AquaComponentUI}.
     */

    public static void installListener(@NotNull JComponent c) {
        AquaComponentUI ui = AquaUtils.getUI(c, AquaComponentUI.class);
        if (ui != null) {
            c.addHierarchyListener(hierarchyListener);
            c.addPropertyChangeListener(FRAME_ACTIVE_PROPERTY, activeStateListener);
        } else {
            throw new IllegalArgumentException("Component must support AquaComponentUI");
        }
    }

    /**
     * Uninstall the event listeners installed by {@link #installListener}.
     * @param c The component.
     */

    public static void uninstallListener(@NotNull Component c) {
        c.removeHierarchyListener(hierarchyListener);
        c.removePropertyChangeListener(FRAME_ACTIVE_PROPERTY, activeStateListener);
    }

    public static void installAppearance(@NotNull JComponent c, @Nullable AquaAppearance appearance) {
        if (appearance != null) {
            AquaAppearance existingAppearance = getRegisteredAppearance(c);
            if (appearance != existingAppearance) {
                try {
                    c.putClientProperty(AQUA_APPEARANCE_KEY, appearance);
                } catch (Throwable th) {
                    System.err.println("Error setting appearance property. Check for failure in a property change listener");
                    th.printStackTrace();
                }
                appearanceHasChanged(c, appearance);
            }
        }
    }

    public static void uninstallAppearance(@NotNull JComponent c) {
        try {
            c.putClientProperty(AQUA_APPEARANCE_KEY, null);
        } catch (Throwable th) {
            System.err.println("Error uninstalling appearance property. Check for failure in a property change listener");
            th.printStackTrace();
        }
    }

    public static void updateAppearancesInSubtree(@NotNull Component c) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            AppearanceManager.ensureAppearance(jc);
        }

        if (c instanceof Container) {
            Container parent = (Container) c;
            int count = parent.getComponentCount();
            for (int i = 0; i < count; i++) {
                Component child = parent.getComponent(i);
                updateAppearancesInSubtree(child);
            }
        }
    }

    /**
     * Ensure that an appearance-sensitive component has the proper appearance properties for painting.
     * A component UI should call this method to compute an initial or updated configuration.
     * If the component UI does not dynamically maintain the appearance based configuration of its component, it should
     * call this method for each painting operation before any appearance-sensitive values are obtained.
     * @param c The component.
     * @return the appearance for the component.
     */

    public static @NotNull AquaAppearance ensureAppearance(@NotNull Component c) {

        // Appearances currently affect only component painting.
        // In the general case, the appearance for a displayable component depends upon its position in the component
        // hierarchy and the appearance of its native window.

        // Validating the appearance at paint time ensures that the correct appearance is used even if the component
        // hierarchy has been modified. (One common example of modification is the use of cell renderers.) An alternative
        // would be to use event listeners to track changes in the hierarchy.

        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            AquaAppearance currentAppearance = getRegisteredAppearance(jc);
            AquaAppearance appearance = getAppearanceForJComponent(jc);
            if (appearance != currentAppearance) {
                installAppearance(jc, appearance);
            }
            return appearance;
        }

        return getInheritedAppearance(c);
    }

    private static @NotNull AquaAppearance getAppearanceForComponent(@NotNull Component c) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            return getAppearanceForJComponent(jc);
        }
        // Otherwise, use the parent appearance.
        return getInheritedAppearance(c);
    }

    private static @NotNull AquaAppearance getAppearanceForJComponent(@NotNull JComponent jc) {
        boolean useVibrant = AquaVibrantSupport.isVibrant(jc) && !(jc instanceof JRootPane);

        // If the component specifies an appearance, obey that specification.
        String specifiedAppearanceName = getRegisteredAppearanceName(jc);
        if (specifiedAppearanceName != null) {
            AquaAppearance appearance = AquaAppearances.get(specifiedAppearanceName);
            if (useVibrant) {
                appearance = AquaAppearances.getVibrantAppearance(appearance);
            }
            return appearance;
        }

        // Otherwise, use the parent appearance.
        AquaAppearance appearance = getInheritedAppearance(jc);
        if (useVibrant) {
            appearance = AquaAppearances.getVibrantAppearance(appearance);
        }
        return appearance;
    }

    private static @NotNull AquaAppearance getAppearanceForComponent(@NotNull Component c,
                                                                     @NotNull AquaAppearance inheritedAppearance) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            boolean useVibrant = AquaVibrantSupport.isVibrant(jc) && !(jc instanceof JRootPane);

            // If the component specifies an appearance, obey that specification.
            String specifiedAppearanceName = getRegisteredAppearanceName(jc);
            if (specifiedAppearanceName != null) {
                AquaAppearance appearance = AquaAppearances.get(specifiedAppearanceName);
                if (useVibrant) {
                    appearance = AquaAppearances.getVibrantAppearance(appearance);
                }
                return appearance;
            }
        }

        // Otherwise, use the inherited appearance.
        return inheritedAppearance;
    }

    private static @NotNull AquaAppearance getInheritedAppearance(@NotNull Component c) {
        Container parent = c.getParent();
        if (parent == null) {
            String name = AquaUtils.nativeGetApplicationAppearanceName();
            if (name != null) {
                return AquaAppearances.get(name);
            }
            return AquaAppearances.getDefaultAppearance();
        }
        return getAppearanceForComponent(parent);
    }

    public static @NotNull AquaAppearance getAppearance(@NotNull Component c) {
        AquaAppearance appearance = getRegisteredAppearance(c);
        return appearance != null ? appearance : AquaAppearances.getDefaultAppearance();
    }

    public static @Nullable AquaAppearance getRegisteredAppearance(@NotNull Component c) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            Object o = jc.getClientProperty(AQUA_APPEARANCE_KEY);
            if (o instanceof AquaAppearance) {
                return (AquaAppearance) o;
            }
        }
        return null;
    }

    private static @Nullable String getRegisteredAppearanceName(@NotNull Component c) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            Object o = jc.getClientProperty(AQUA_APPEARANCE_NAME_KEY);
            if (o instanceof String) {
                return (String) o;
            }
        }
        return null;
    }

    private static void appearanceHasChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        if (c instanceof JMenuBar) {
            JMenuBar mb = (JMenuBar) c;
            // Special hack because we are required to use the platform AquaMenuBarUI to be able to use the screen menu bar
            Color background = mb.getBackground();
            if (background instanceof ColorUIResource) {
                mb.setBackground(appearance.getColor("controlBackground"));
            }
            Color foreground = mb.getForeground();
            if (foreground instanceof ColorUIResource) {
                mb.setForeground(appearance.getColor("control"));
            }
        }

        AquaComponentUI ui = AquaUtils.getUI(c, AquaComponentUI.class);
        if (ui != null) {
            ui.appearanceChanged(c, appearance);
        }
    }

    private static class ActiveStateListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent e) {
            Object source = e.getSource();
            if (source instanceof JComponent) {
                JComponent jc = (JComponent) source;
                AquaComponentUI ui = AquaUtils.getUI(jc, AquaComponentUI.class);
                 if (ui != null) {
                     Object newValue = e.getNewValue();
                     if (newValue instanceof Boolean) {
                         Boolean b = (Boolean) newValue;
                         ui.activeStateChanged(jc, b);
                     }
                 }
            }
        }
    }

    private static class AppearanceManagerHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(@NotNull HierarchyEvent e) {
            long flags = e.getChangeFlags();
            if ((flags & (HierarchyEvent.PARENT_CHANGED)) != 0) {
                Component c = e.getChanged();
                if (c instanceof JComponent) {
                    JComponent jc = (JComponent) c;
                    parentChanged(jc);
                }
            }
        }
    }

    private static void parentChanged(@NotNull JComponent c) {
        Container parent = c.getParent();
        if (parent == null) {
            // If the component was removed from a hierarchy, remove all appearance information from the component.
            // Even if the component specified an appearance name, the appearance may become stale while the component
            // is not in a component hierarchy, so it is best to remove it.
            uninstallAppearancesInTree(c);
        } else {
            // Otherwise, attempt to determine the appearance of the component.
            // First check to see if it specifies an appearance name.
            String appearanceName = getRegisteredAppearanceName(c);
            if (appearanceName != null) {
                // If the component specifies an appearance, install that appearance, or the appropriate variant.
                AquaAppearance appearance = AquaAppearances.get(appearanceName);
                appearance = getVariant(c, appearance);
                installAppearance(c, appearance);
                installAppearancesInNewlyAttachedTree(c);
                return;
            }
            AquaAppearance appearance = getInheritedAppearanceForAttachment(c);
            if (appearance == null) {
                // If the most immediate JComponent parent does not have an appearance, we remove all appearance
                // information from the component. The appearance will be determined later, most likely when an
                // appearance is assigned to the window or when its appearance is requested. The no appearance case is
                // most likely to occur while a component hierarchy is being constructed.
                uninstallAppearancesInTree(c);
            } else {
                // Inherit the parent appearance, or the appropriate variant.
                appearance = getVariant(c, appearance);
                installAppearance(c, appearance);
                installAppearancesInNewlyAttachedTree(c);
            }
        }
    }

    private static @Nullable AquaAppearance getInheritedAppearanceForAttachment(@NotNull JComponent c) {
        // Look for an appearance assigned to the nearest JComponent ancestor.
        // Skip non-JComponents, such as CellRendererPane.

        Container p = c;
        while ((p = p.getParent()) != null) {
            if (p instanceof JComponent) {
                JComponent jc = (JComponent) p;
                return getRegisteredAppearance(jc);
            }
        }
        return null;
    }

    private static void installAppearancesInNewlyAttachedTree(@NotNull Component c) {
        // We have just installed an appearance at the root of a newly attached tree.
        // If the tree is part of a complete hierarchy, the install appearances in the child trees.
        // Otherwise, it is probably better to wait.

        AquaAppearance topAppearance = getRegisteredAppearance(c);  // hopefully redundant check
        if (topAppearance != null) {
            Window w = SwingUtilities.getWindowAncestor(c);
            if (w != null) {
                Container top = (Container) c;  // must succeed
                int count = top.getComponentCount();
                for (int i = 0; i < count; i++) {
                    Component child = top.getComponent(i);
                    installAppearancesInTree(child, topAppearance);
                }
            }
        }
    }

    private static void installAppearancesInTree(@NotNull Component c,
                                                 @NotNull AquaAppearance inheritedAppearance) {
        if (c instanceof Container) {
            Container top = (Container) c;  // must succeed
            AquaAppearance topAppearance = getAppearanceForComponent(c, inheritedAppearance);
            if (c instanceof JComponent) {
                JComponent jc = (JComponent) c;
                installAppearance(jc, topAppearance);
            }
            int count = top.getComponentCount();
            for (int i = 0; i < count; i++) {
                Component child = top.getComponent(i);
                installAppearancesInTree(child, topAppearance);
            }
         }
    }

    private static void uninstallAppearancesInTree(@NotNull Component c) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            uninstallAppearance(jc);
        }

        if (c instanceof Container) {
            Container parent = (Container) c;
            int count = parent.getComponentCount();
            for (int i = 0; i < count; i++) {
                Component child = parent.getComponent(i);
                uninstallAppearancesInTree(child);
            }
        }
    }

    private static @NotNull AquaAppearance getVariant(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        boolean useVibrant = AquaVibrantSupport.isVibrant(c) && !(c instanceof JRootPane);
        if (useVibrant) {
            appearance = AquaAppearances.getVibrantAppearance(appearance);
        }
        return appearance;
    }
}
