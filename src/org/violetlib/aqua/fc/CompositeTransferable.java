/*
 * @(#)CompositeTransferable.java
 *
 * Copyright (c) 2001-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2015 Alan Snyder.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * CompositeTransferable.
 *
 * @author  Werner Randelshofer
 * @version $Id$
 */
public class CompositeTransferable implements java.awt.datatransfer.Transferable {
    private Map<DataFlavor,Transferable> transferables = new HashMap<>();
    private LinkedList<DataFlavor> flavors = new LinkedList<>();

    /** Creates a new instance of CompositeTransferable */
    public CompositeTransferable() {
    }

    public void add(Transferable t) {
        DataFlavor[] fs = t.getTransferDataFlavors();
        for (DataFlavor f : fs) {
            if (!transferables.containsKey(f)) {
                flavors.add(f);
            }
            transferables.put(f, t);
        }
    }

    /**
     * Returns an object which represents the data to be transferred.  The class
     * of the object returned is defined by the representation class of the flavor.
     *
     * @param flavor the requested flavor for the data
     * @see DataFlavor#getRepresentationClass
     * @exception IOException                if the data is no longer available
     *             in the requested flavor.
     * @exception UnsupportedFlavorException if the requested data flavor is
     *             not supported.
     */
    public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
        Transferable t = transferables.get(flavor);
        if (t == null) {
            throw new UnsupportedFlavorException(flavor);
        }
        return t.getTransferData(flavor);
    }

    /**
     * Returns an array of DataFlavor objects indicating the flavors the data
     * can be provided in.  The array should be ordered according to preference
     * for providing the data (from most richly descriptive to least descriptive).
     * @return an array of data flavors in which this data can be transferred
     */
    public DataFlavor[] getTransferDataFlavors() {
        return flavors.toArray(new DataFlavor[transferables.size()]);
    }

    /**
     * Returns whether or not the specified data flavor is supported for
     * this object.
     * @param flavor the requested flavor for the data
     * @return boolean indicating whether or not the data flavor is supported
     */
    public boolean isDataFlavorSupported(DataFlavor flavor) {
        return transferables.containsKey(flavor);
    }
}
