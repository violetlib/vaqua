/*
 * @(#)ArrayUtil.java
 *
 * Copyright (c) 2003-2010 Werner Randelshofer, Switzerland
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.util.ArrayList;

/**
 * ArrayUtil.
 *
 * @author  Werner Randelshofer
 * @version $Id$
 */
public class ArrayUtil {

    /** Prevent instance creation. */
    private ArrayUtil() {
    }

    public static ArrayList asList(int[] a) {
        ArrayList list = new ArrayList(a.length);
        for (int i=0; i < a.length; i++) {
            list.add(a[i]);
        }
        return list;
    }

    private final static char[] hexChars = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };

    public static int[] truncate(int[] a, int off, int len) {
        int[] b = new int[len];
        System.arraycopy(a, off, b, 0, len);
        return b;
    }
}
