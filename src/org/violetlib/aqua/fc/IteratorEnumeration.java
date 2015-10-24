/*
 * @(#)IteratorEnumeration.java
 *
 * Copyright (c) 2004-2013 Werner Randelshofer, Switzerland.
 * All rights reserved.
 *
 * The copyright of this software is owned by Werner Randelshofer.
 * You may not use, copy or modify this software, except in
 * accordance with the license agreement you entered into with
 * Werner Randelshofer. For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.util.Enumeration;
import java.util.Iterator;

/**
 * This Enumeration is a wrapper over an Iterator.
 *
 * @author  Werner Randelshofer
 * @version $Id$
 */
public class IteratorEnumeration implements Enumeration {
    private Iterator iterator;

    public IteratorEnumeration(Iterator iterator) {
        this.iterator = iterator;
    }

    public boolean hasMoreElements() {
        return iterator.hasNext();
    }

    public Object nextElement() {
        return iterator.next();
    }
}
