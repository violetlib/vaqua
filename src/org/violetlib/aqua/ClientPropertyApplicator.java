/*
 * Changes copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2012, Oracle and/or its affiliates. All rights reserved.
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import javax.swing.*;

public class ClientPropertyApplicator<T extends JComponent, N> implements PropertyChangeListener {
    private final Map<String, Property<N>> properties = new HashMap<String, Property<N>>();

    @SuppressWarnings("unchecked")
    public ClientPropertyApplicator(Property<N>... propertyList) {
        for (Property<N> p : propertyList) {
            properties.put(p.name, p);
        }
    }

    void applyProperty(N target, String propName, Object value) {
        Property<N> property = properties.get(propName);
        if (property != null) {
            property.applyProperty(target, value);
        }
    }

    public void attachAndApplyClientProperties(T target) {
        target.addPropertyChangeListener(this);
        N obj = convertJComponentToTarget(target);
        if (obj == null) {
            return;
        }

        Set<String> propNames = properties.keySet();
        for (String propName : propNames) {
            Object value = target.getClientProperty(propName);
            if (value == null) {
                continue;
            }
            applyProperty(obj, propName, value);
        }
    }

    public void removeFrom(T target) {
        target.removePropertyChangeListener(this);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void propertyChange(PropertyChangeEvent evt) {
        N obj = convertJComponentToTarget((T)evt.getSource());
        if (obj == null) return;
        applyProperty(obj, evt.getPropertyName(), evt.getNewValue());
    }

    @SuppressWarnings("unchecked")
    public N convertJComponentToTarget(T component) {
        return (N)component; // naive implementation
    }

    public abstract static class Property<X> {
        private final String name;

        public Property(String name) {
            this.name = name;
        }

        public abstract void applyProperty(X target, Object value);
    }
}
