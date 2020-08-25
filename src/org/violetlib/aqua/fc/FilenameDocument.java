/*
 * @(#)FilenameDocument.java
 *
 * Copyright (c) 2010 Werner Randelshofer, Switzerland.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;
import java.util.HashSet;

/**
 * A document model which silently converts forbidden filename characters into dashes.
 * <p>
 * On Mac OS X, only the colon character is forbidden: {@code : }.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public class FilenameDocument extends PlainDocument {

    private HashSet<Character> forbidden;

    public FilenameDocument() {
        forbidden = new HashSet<Character>();
        forbidden.add(':');
    }

    @Override
    public void insertString(int offs, String str, AttributeSet a)
            throws BadLocationException {
        char[] chars = str.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (forbidden.contains(chars[i])) {
                chars[i] = '-';
            }
        }

        super.insertString(offs, new String(chars), a);
    }
}
