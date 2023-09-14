/*
 * Copyright (c) 2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.WeakHashMap;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;

/**
 * Manage components whose UIs want to be notified when certain system properties change.
 * This design avoids holding a strong reference to the component.
 */

public class SystemPropertyChangeManager {

    public interface SystemPropertyChangeListener {
        void systemPropertyChanged(JComponent c, Object type);
    }

    private static final WeakHashMap<JComponent,JComponent> components = new WeakHashMap<>();

    public static void register(JComponent c) {
        components.put(c, null);
    }

    public static void unregister(JComponent c) {
        components.remove(c);
    }

    public static void notifyChange(Object type) {
        Collection<JComponent> cs = components.keySet();
        if (!cs.isEmpty()) {
            List<JComponent> componentList = new ArrayList<>(cs);
            for (JComponent jc : componentList) {
                ComponentUI ui = JavaSupport.getUI(jc);
                if (ui instanceof SystemPropertyChangeListener) {
                    SystemPropertyChangeListener l = (SystemPropertyChangeListener) ui;
                    l.systemPropertyChanged(jc, type);
                } else {
                    components.remove(jc);
                }
            }
        }
    }
}
