/*
 * Copyright (c) 2018-2023 Alan Snyder.
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
import java.util.ArrayList;
import java.util.WeakHashMap;
import javax.swing.*;
import javax.swing.plaf.ColorUIResource;

import org.jetbrains.annotations.*;

import static org.violetlib.aqua.AquaFocusHandler.FRAME_ACTIVE_PROPERTY;

/**
 * This class supports the association of appearances with components.
 */

public class AppearanceManager {

    public static boolean isDebug = false;
    public static boolean isDebugShortCircuit = false;

    public static final String AQUA_APPEARANCE_NAME_KEY = "Aqua.appearanceName";    // the name of an explicitly chosen appearance
    public static final String AQUA_APPEARANCE_KEY = "Aqua.appearance";             // the effective appearance

    private static final AppearanceManagerHierarchyListener hierarchyListener = new AppearanceManagerHierarchyListener();
    private static final ActiveStateListener activeStateListener = new ActiveStateListener();
    private static final AppearanceNamePropertyListener appearanceNamePropertyListener = new AppearanceNamePropertyListener();
    private static final @NotNull AppearanceManager.HierarchyUpdateProcessor updateProcessor = new HierarchyUpdateProcessor();

    private static final WeakComponentSet componentsUsingSpecifiedAppearances = new WeakComponentSet();

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

    /**
     * Ensure that a component has the correct registered appearance for a painting operation in progress and
     * make that appearance available using the {@link #getCurrentAppearance getCurrentAppearance} method.
     * @param c The component.
     * @return the appearance for the component.
     */

    public static @NotNull AquaAppearance registerCurrentAppearance(@NotNull JComponent c) {
        AquaAppearance appearance = ensureAppearance(c);
        if (appearance != currentAppearance) {
            currentAppearance = appearance;
            if (isDebug) {
                debug(c, "Current appearance changed to: " + currentAppearance);
            }
        }
        return currentAppearance;
    }

    /**
     * Install event listeners to help manage the appearance properties of the specified component.
     * @param c The component.
     * @throws IllegalArgumentException if {@code c} does not support {@link AquaComponentUI}.
     */

    public static void installListeners(@NotNull JComponent c) {
        AquaComponentUI ui = AquaUtils.getUI(c, AquaComponentUI.class);
        if (ui != null || c instanceof JLayeredPane) {
            c.addHierarchyListener(hierarchyListener);
            c.addPropertyChangeListener(FRAME_ACTIVE_PROPERTY, activeStateListener);
            c.addPropertyChangeListener(AQUA_APPEARANCE_NAME_KEY, appearanceNamePropertyListener);
        } else {
            throw new IllegalArgumentException("Component must support AquaComponentUI");
        }
    }

    /**
     * Uninstall the event listeners installed by {@link #installListeners}.
     * @param c The component.
     */

    public static void uninstallListeners(@NotNull Component c) {
        c.removeHierarchyListener(hierarchyListener);
        c.removePropertyChangeListener(FRAME_ACTIVE_PROPERTY, activeStateListener);
        c.addPropertyChangeListener(AQUA_APPEARANCE_NAME_KEY, appearanceNamePropertyListener);
    }

    public static void setRootPaneRegisteredAppearance(@NotNull JRootPane rp, @NotNull AquaAppearance appearance) {
        updateAppearancesInSubtree(rp, appearance, false);
    }

    private static void setRegisteredAppearance(@NotNull Component c, @NotNull AquaAppearance appearance) {
        updateAppearancesInSubtree(c, appearance, false);
    }

    /**
     * Set the component registered appearance to the specified appearance.
     * @param jc The component.
     * @param appearance The appearance.
     * @return true if the registered appearance was updated (or it should have been), false if the existing registered
     * appearance matches {@code appearance}.
     */

    private static boolean setRegisteredAppearance(@NotNull JComponent jc, @NotNull AquaAppearance appearance) {
        AquaAppearance existingAppearance = getRegisteredAppearance(jc);
        if (appearance != existingAppearance) {
            try {
                jc.putClientProperty(AQUA_APPEARANCE_KEY, appearance);
                if (isDebug) {
                    debug(jc, "Registering appearance " + appearance + " for " + AquaUtils.show(jc));
                }
                appearanceHasChanged(jc, appearance);
            } catch (Throwable th) {
                Utils.logError("Unable to set appearance property on " + AquaUtils.show(jc)
                        + ". Check for failure in a property change listener", th);
                th.printStackTrace();
            }
            return true;
        }
        return false;
    }

    public static void removeRegisteredAppearance(@NotNull JComponent jc) {
        try {
            Object o = jc.getClientProperty(AQUA_APPEARANCE_KEY);
            if (o != null) {
                jc.putClientProperty(AQUA_APPEARANCE_KEY, null);
                if (isDebug) {
                    String name;
                    if (o instanceof AquaAppearance) {
                        name = ((AquaAppearance) o).getName();
                    } else {
                        name = "?";
                    }
                    debug(jc, "Removing appearance " + name + " for " + AquaUtils.show(jc));
                }
            }
        } catch (Throwable th) {
            Utils.logError("Unable to uninstall appearance property on " + AquaUtils.show(jc)
                    + ". Check for failure in a property change listener", th);
            th.printStackTrace();
        }
    }

    /**
     * Return the current application appearance.
     */

    public static @NotNull AquaAppearance getApplicationAppearance() {
        String name = AquaUtils.nativeGetApplicationAppearanceName();
        if (name != null) {
            return AquaAppearances.get(name);
        }
        return AquaAppearances.getDefaultAppearance();
    }

    /**
     * Set the registered appearance for the specified component and update its subcomponents accordingly.
     * This method short-circuits on any subcomponent that already has the appropriate registered appearance.
     */

    public static void updateAppearancesInTree(@NotNull Component c, @NotNull AquaAppearance appearance) {
        setRegisteredAppearance(c, appearance);

        if (c instanceof Container) {
            Container cc = (Container) c;
            int count = cc.getComponentCount();
            for (int i = 0; i < count; i++) {
                Component child = cc.getComponent(i);
                updateAppearancesInSubtree(child, appearance, false);
            }
        }
    }

    /**
     * Set the registered appearance for the specified component and update its subcomponents accordingly.
     * @param c The component.
     * @param appearance The appearance to register for the component.
     * @param force If true, any previously registered appearances are ignored. If false,
     * this method short-circuits on any component that already has the appropriate registered appearance.
     */

    private static void updateAppearancesInSubtree(@NotNull Component c,
                                                   @NotNull AquaAppearance appearance,
                                                   boolean force)
    {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            if (!force) {
                if (matchesRegisteredAppearance(jc, appearance)) {
                    if (isDebug && isDebugShortCircuit) {
                        debug(c, "Short circuit: component " + AquaUtils.show(c) + " has the correct appearance");
                    }
                    return;
                }
                if (hasValidRegisteredSpecifiedAppearance(jc)) {
                    if (isDebug && isDebugShortCircuit) {
                        debug(c, "Short circuit: component " + AquaUtils.show(c) + " has a valid specified appearance");
                    }
                    return;
                }
            }
            setRegisteredAppearance(jc, appearance);
        }

        if (c instanceof Container) {
            Container cc = (Container) c;
            int count = cc.getComponentCount();
            for (int i = 0; i < count; i++) {
                Component child = cc.getComponent(i);
                updateAppearancesInSubtree(child, appearance, force);
            }
        }
    }

    private static boolean matchesRegisteredAppearance(@NotNull JComponent jc, @NotNull AquaAppearance appearance) {
        AquaAppearance registeredAppearance = getRegisteredAppearance(jc);
        if (registeredAppearance == null) {
            return false;
        }
        if (registeredAppearance == appearance) {
            return true;
        }
        if (useVibrantAppearance(jc)) {
            AquaAppearance vibrantVersion = AquaAppearances.getVibrantAppearance(appearance);
            if (vibrantVersion == registeredAppearance) {
                return true;
            }
        }
        return false;
    }

    /**
     * Ensure that a component has the proper appearance properties for painting. The selected appearance is based on
     * the existence of a specified appearance, an appearance obtained from an ancestor, or the application appearance.
     * If appropriate, the vibrant variant of the specified, inherited, or application appearance will be used. The
     * currently registered appearance, if any, is ignored.
     *
     * A component UI should call this method before performing a painting operation to ensure that the correct
     * appearance is used even if the component hierarchy has been modified since the UI last configured itself.
     * Updating the configuration may be essential if the component is being used as a cell renderer.
     * <p>
     * The component is updated, if necessary, to register the appearance for future use. Ancestor components may also
     * be updated.
     * @param c The component.
     * @return the appearance to use for the component.
     */

    public static @NotNull AquaAppearance ensureAppearance(@NotNull Component c) {

        // Unlike getAppearance(), this method computes the appearance without using the registered appearance and
        // registers the computed appearance even if it is a default appearance.

        AquaAppearance a = getKnownAppearance(c, true);
        if (a == null) {
            a = getApplicationAppearance();
            setRegisteredAppearance(c, a);
        }
        return a;
    }

    /**
     * Return the appearance that should be used by a component for a painting operation in progress. If the component
     * has a registered appearance, that appearance is returned. Otherwise an appearance is determined based on the
     * existence of a specified appearance, an appearance obtained from an ancestor, or the application appearance. If
     * appropriate, the vibrant variant of the specified, inherited, or application appearance will be used. The
     * component is updated to register the appearance for future use, if it is not a default appearance. Ancestors may
     * also be updated.
     * @param c The component.
     * @return the appearance to use.
     */

    public static @NotNull AquaAppearance getAppearance(@NotNull Component c) {
        AquaAppearance a = getKnownAppearance(c, false);
        if (a != null) {
            return a;
        }
        a = getApplicationAppearance();
        if (isDebug) {
            debug(c, "Using application appearance " + a.getName() + " for " + AquaUtils.show(c));
        }
        return a;
    }

    /**
     * Return the non-default appearance that should be used by a component for a painting operation in progress. If an
     * appearance is found, the component and its descendants are updated (if possible and as needed) to register the
     * appearance for future use. Ancestors and their descendants may also be updated.
     * @param c The component.
     * @param force If true, any existing registered appearance will be ignored.
     * @return the appearance to use, or null if no known appearance is available.
     */

    private static @Nullable AquaAppearance getKnownAppearance(@NotNull Component c, boolean force) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            if (!force) {
                // If an appearance has been registered for this component, use that.
                AquaAppearance a = getRegisteredAppearance(jc);
                if (a != null) {
                    return a;
                }
            }
            // If the component has been configured to use a particular appearance regardless of context, use that.
            // The appearance is registered for future use.
            AquaAppearance a = getSpecifiedAppearanceVariant(jc);
            if (a != null) {
                updateAppearancesInSubtree(jc, a, false);
                return a;
            }
        }
        // If the nearest JComponent ancestor has a known appearance, use that.
        // The appearance is registered for future use, if possible.
        JComponent ancestor = getJComponentAncestor(c);
        if (ancestor != null) {
            AquaAppearance a = getKnownAppearance(ancestor, false);
            if (a != null) {
                updateAppearancesInSubtree(c, a, false);
                return a;
            }
        } else if (c instanceof JRootPane) {
            // The root pane has not been configured to use a specific appearance, so use the application appearance.
            AquaAppearance a = getApplicationAppearance();
            updateAppearancesInSubtree(c, a, false);
            return a;
        }
        // There is no known appearance for the component.
        return null;
    }

    /**
     * Identify the appearance for a component to inherit from an ancestor.
     * @param c The component.
     * @return the appearance to inherit, or null if no inherited appearance is available.
     */

    private static @Nullable AquaAppearance getInheritedAppearance(@NotNull Component c)
    {
        Container parent = c.getParent();
        while (parent != null) {
            AquaAppearance a = getRegisteredAppearance(parent);
            if (a != null) {
                return a;
            }
            a = getSpecifiedAppearanceVariant(parent);
            if (a != null) {
                return a;
            }
            parent = parent.getParent();
        }
        return null;
    }

    /**
     * Return the nearest ancestor that is a Swing component.
     */

    private static @Nullable JComponent getJComponentAncestor(@NotNull Component c) {
        Component current = c;
        while (true) {
            Container parent = current.getParent();
            if (parent == null) {
                return null;
            }
            if (parent instanceof JComponent) {
                return (JComponent) parent;
            }
            current = parent;
        }
    }

    /**
     * Return the appearance that has been registered for use by a component.
     * @param c The component.
     * @return the registered appearance, or null.
     */

    public static @Nullable AquaAppearance getRegisteredAppearance(@NotNull Component c) {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            return getRegisteredAppearance(jc);
        }
        return null;
    }

    /**
     * Return the appearance that has been registered for use by a component.
     * @param jc The component.
     * @return the registered appearance, or null.
     */

    public static @Nullable AquaAppearance getRegisteredAppearance(@NotNull JComponent jc) {
        Object o = jc.getClientProperty(AQUA_APPEARANCE_KEY);
        if (o instanceof AquaAppearance) {
            return (AquaAppearance) o;
        }
        return null;
    }

    /**
     * Return the appearance that has been specified for use by a component, if valid. The vibrant version of the
     * appearance will be returned, if appropriate. The component is not update.
     * @param c The component.
     * @return the specified appearance, or null if none.
     */

    private static @Nullable AquaAppearance getSpecifiedAppearanceVariant(@NotNull Component c) {
        return c instanceof JComponent ? getSpecifiedAppearanceVariant((JComponent) c) : null;
    }

    /**
     * Return the appearance that has been specified for use by a component, if valid. The vibrant version of the
     * appearance will be returned, if appropriate. The component is not updated.
     * @param jc The component.
     * @return the specified appearance, or null if none.
     */

    private static @Nullable AquaAppearance getSpecifiedAppearanceVariant(@NotNull JComponent jc) {
        String name = getSpecifiedAppearanceName(jc);
        if (name != null) {
            AquaAppearance appearance = AquaAppearances.get(name);
            return getVariant(jc, appearance);
        }
        return null;
    }

    /**
     * Return the appearance name that has been specified for use by a component.
     * @param c The component.
     * @return the specified appearance name, or null if none.
     */

    public static @Nullable String getSpecifiedAppearanceName(@NotNull Component c) {
        return c instanceof JComponent ? getSpecifiedAppearanceName((JComponent) c) : null;
    }

    /**
     * Return the appearance name that has been specified for use by a component.
     * @param jc The component.
     * @return the specified appearance name, or null if none.
     */

    private static @Nullable String getSpecifiedAppearanceName(@NotNull JComponent jc) {
        Object o = jc.getClientProperty(AQUA_APPEARANCE_NAME_KEY);
        if (o instanceof String) {
            return (String) o;
        }
        return null;
    }

    /**
     * Set the name of the appearance to be used by a component.
     * @param jc The component.
     * @param name the appearance name, or null to remove any specified appearance name.
     */

    public static void setSpecifiedAppearanceName(@NotNull JComponent jc, @Nullable String name) {
        jc.putClientProperty(AQUA_APPEARANCE_NAME_KEY, name);
    }

    /**
     * Check for a valid specified appearance for a component. A specified appearance is valid if the specified
     * appearance name identifies a supported appearance and the registered appearance matches that appearance,
     * with vibrant variants taken into account.
     * @return true if the component specifies an appearance and it matches the registered appearance.
     */

    private static boolean hasValidRegisteredSpecifiedAppearance(@NotNull JComponent jc) {
        AquaAppearance appearance = getSpecifiedAppearanceVariant(jc);
        if (appearance != null) {
            AquaAppearance registeredAppearance = getRegisteredAppearance(jc);
            return registeredAppearance == appearance;
        }
        return false;
    }

    /**
     * Check for an unexpected mismatch between the specified appearance and the registered appearance.
     * @param jc A component with a valid specified appearance.
     * @param specifiedAppearance The specified appearance.
     * @return true if the registered appearance matches the specified appearance or there is no registered
     * appearance, false otherwise.
     */

    private static boolean validateRegistrationForSpecifiedAppearance(@NotNull JComponent jc,
                                                                      @NotNull AquaAppearance specifiedAppearance) {
        AquaAppearance registeredAppearance = getRegisteredAppearance(jc);
        if (registeredAppearance == null) {
            return true;
        }
        if (registeredAppearance != specifiedAppearance) {
            AquaUtils.syslog("Registered appearance " + registeredAppearance.getName()
                    + " does not match specified appearance " + specifiedAppearance.getName()
                    + " for " + AquaUtils.show(jc));
            return false;
        }
        return true;
    }

    private static void appearanceHasChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        if (c instanceof JMenuBar) {
            JMenuBar mb = (JMenuBar) c;
            // Special hack because we are required to use the platform AquaMenuBarUI to be able to use the screen menu
            // bar
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

    private static class AppearanceNamePropertyListener
            implements PropertyChangeListener {
        @Override
        public void propertyChange(@NotNull PropertyChangeEvent e) {
            Object source = e.getSource();
            if (source instanceof JComponent) {
                JComponent jc = (JComponent) source;
                Object newValue = e.getNewValue();
                if (newValue == null || newValue instanceof String) {
                    String appearanceName = (String) newValue;
                    specifiedAppearanceNameChanged(jc, appearanceName);
                }
            }
        }
    }

    /**
     * This method is called when the specified appearance name client property of a managed component changes.
     */

    private static void specifiedAppearanceNameChanged(@NotNull JComponent c, @Nullable String appearanceName) {
        if (appearanceName != null) {
            AquaAppearance appearance = getNamedAppearanceVariant(c, appearanceName);
            if (appearance != null) {
                if (isDebug) {
                    debug(c, "Appearance " + appearanceName + " specified for " + AquaUtils.show(c));
                }
                componentsUsingSpecifiedAppearances.add(c);
                //c.addContainerListener(specifiedAppearanceContainerListener);
                updateAppearancesInTree(c, appearance);
            } else {
                if (isDebug) {
                    AquaUtils.syslog("Specified appearance " + appearanceName + " for " + AquaUtils.show(c)
                            + " is not available");
                }
            }
        } else {
            if (isDebug) {
                debug(c, "Specified appearance for " + AquaUtils.show(c) + " removed");
            }
            componentsUsingSpecifiedAppearances.remove(c);
            c.removePropertyChangeListener(appearanceNamePropertyListener);
            //c.removeContainerListener(specifiedAppearanceContainerListener);
            AquaAppearance specifiedAppearance = getSpecifiedAppearanceVariant(c);
            if (specifiedAppearance != null) {
                validateRegistrationForSpecifiedAppearance(c, specifiedAppearance);
                updateAppearancesInSubtree(c, specifiedAppearance, false);
            }
        }
    }

    private static class AppearanceManagerHierarchyListener implements HierarchyListener {

        // When a component tree is added to a hierarchy, any cached appearance objects for its components may be
        // stale. Appearance objects become stale not only when the user switches between light and dark appearances,
        // but also if the user changes the accent color, the highlight color, or the high visibility option. If the
        // user made such a change while the component tree was detached, its appearances may need to be updated.
        // In general, appearances can be determined only for a rooted hierarchy. Therefore, no attempt is made to
        // update the appearances until the components are in a rooted hierarchy.

        @Override
        public void hierarchyChanged(@NotNull HierarchyEvent e) {
            long flags = e.getChangeFlags();
            if ((flags & HierarchyEvent.PARENT_CHANGED) != 0) {
                Component top = e.getChanged();
                updateProcessor.acceptTop(top);
            }
        }
    }

    /**
     * This class manages the process of updating a subtree when it is added to a rooted hierarchy. Notification of
     * hierarchy changes are delivered to appearance-sensitive components individually; whichever one checks in first
     * initiates the update.
     */

    private static class HierarchyUpdateProcessor {
        private @Nullable Component currentTop;

        /**
         * Process the top component in a hierarchy changed event as needed.
         *
         * @param top The top component.
         */

        public void acceptTop(@NotNull Component top) {
            if (top.getParent() == null) {
                // The subtree is being removed.
                currentTop = null;
            } else {
                if (top == currentTop) {
                    return;
                }
                currentTop = top;
                // If the subtree is not part of a rooted hierarchy, do not process it now.
                JRootPane rootPane = SwingUtilities.getRootPane(top);
                if (rootPane == null) {
                    return;
                }
                // If the top component has a specified appearance, update the subtree starting with that appearance.
                AquaAppearance specifiedTopAppearance = getSpecifiedAppearanceVariant(top);
                if (specifiedTopAppearance != null) {
                    updateAppearancesInSubtree(top, specifiedTopAppearance, true);
                } else {
                    // Otherwise, try to obtain an inherited appearance. If found, update the subtree starting with that
                    // appearance.
                    AquaAppearance inheritedAppearance = getInheritedAppearance(top);
                    if (inheritedAppearance != null) {
                        updateAppearancesInSubtree(top, inheritedAppearance, true);
                    } else {
                        // Otherwise, do nothing. The full hierarchy will be updated when the root pane configures its
                        // appearance.
                    }
                }
            }
        }
    }

    /**
     * Return the appearance with the specified name that is appropriate for the specified component.
     * The returned appearance may be a vibrant appearance.
     * @param jc The component.
     * @param name The base (non-vibrant) appearance name.
     */

    private static @Nullable AquaAppearance getNamedAppearanceVariant(@NotNull JComponent jc, @NotNull String name) {
        AquaAppearance appearance = AquaAppearances.getOptional(name);
        return appearance != null ? getVariant(jc, appearance) : null;
    }

    private static @NotNull AquaAppearance getVariant(@NotNull JComponent jc, @NotNull AquaAppearance appearance) {
        if (useVibrantAppearance(jc)) {
            appearance = AquaAppearances.getVibrantAppearance(appearance);
        }
        return appearance;
    }

    private static boolean useVibrantAppearance(@NotNull JComponent jc) {
        return AquaVibrantSupport.isVibrant(jc) && !(jc instanceof JRootPane);
    }

    private static class WeakComponentSet {
        private final WeakHashMap<JComponent,JComponent> map = new WeakHashMap<>();

        public void add(@NotNull JComponent c) {
            map.put(c, c);
        }

        public void remove(@NotNull JComponent c) {
            map.remove(c);
        }

        public boolean contains(@NotNull Component c) {
            return c instanceof JComponent && map.containsKey(c);
        }

        public @NotNull java.util.List<JComponent> components() {
            return new ArrayList<>(map.keySet());
        }
    }

    private static void debug(@NotNull Component c, @NotNull String s)
    {
        Window w = SwingUtilities.getWindowAncestor(c);
        String name = w != null ? w.getName() : "";
        if (name != null && !name.isEmpty()) {
            Utils.logDebug("[" + name + "] " + s);
        } else {
            Utils.logDebug(s);
        }
    }
}
