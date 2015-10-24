/*
 * @(#)FileTransferHandler.java
 *
 * Copyright (c) 2004-2013 Werner Randelshofer, Switzerland.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.*;
import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * FileTransferHandler.
 *
 * @author  Werner Randelshofer
 * @version $Id$
 */
public class FileTransferHandler implements DropTargetListener {

    private JFileChooser fileChooser;

    /** Creates a new instance. */
    public FileTransferHandler(JFileChooser fileChooser) {
        this.fileChooser = fileChooser;
    }

    public void setFileChooser(JFileChooser fileChooser) {
        this.fileChooser = fileChooser;
    }

    /**
     * Called while a drag operation is ongoing, when the mouse pointer enters
     * the operable part of the drop site for the <code>DropTarget</code>
     * registered with this listener.
     *
     * @param evt the <code>DropTargetDragEvent</code>
     */
    public void dragEnter(DropTargetDragEvent evt) {
        handleDrag(evt);
    }

    private void handleDrag(DropTargetDragEvent evt) {
        // Reject if flavor not supported
        if (!evt.getCurrentDataFlavorsAsList().contains(DataFlavor.javaFileListFlavor)) {
            evt.rejectDrag();
        }
        // Change drag operation or reject if no suitable operation available
        if (evt.getDropAction() != DnDConstants.ACTION_COPY) {
            if ((evt.getSourceActions() & DnDConstants.ACTION_COPY) != 0) {
                evt.acceptDrag(DnDConstants.ACTION_COPY);
            } else {
                evt.rejectDrag();
            }
        }
    }

    /**
     * Called when a drag operation is ongoing, while the mouse pointer is still
     * over the operable part of the drop site for the <code>DropTarget</code>
     * registered with this listener.
     *
     * @param evt the <code>DropTargetDragEvent</code>
     */
    public void dragOver(DropTargetDragEvent evt) {
        handleDrag(evt);
    }

    /**
     * Called if the user has modified
     * the current drop gesture.
     *
     * @param evt the <code>DropTargetDragEvent</code>
     */
    public void dropActionChanged(DropTargetDragEvent evt) {
        handleDrag(evt);
    }

    /**
     * Called while a drag operation is ongoing, when the mouse pointer has
     * exited the operable part of the drop site for the
     * <code>DropTarget</code> registered with this listener.
     *
     * @param evt the <code>DropTargetEvent</code>
     */
    public void dragExit(DropTargetEvent evt) {
    }

    /**
     * Called when the drag operation has terminated with a drop on
     * the operable part of the drop site for the <code>DropTarget</code>
     * registered with this listener.
     * <p>
     * This method is responsible for undertaking
     * the transfer of the data associated with the
     * gesture. The <code>DropTargetDropEvent</code>
     * provides a means to obtain a <code>Transferable</code>
     * object that represents the data object(s) to
     * be transfered.<P>
     * From this method, the <code>DropTargetListener</code>
     * shall accept or reject the drop via the
     * acceptDrop(int dropAction) or rejectDrop() methods of the
     * <code>DropTargetDropEvent</code> parameter.
     * <P>
     * Subsequent to acceptDrop(), but not before,
     * <code>DropTargetDropEvent</code>'s getTransferable()
     * method may be invoked, and data transfer may be
     * performed via the returned <code>Transferable</code>'s
     * getTransferData() method.
     * <P>
     * At the completion of a drop, an implementation
     * of this method is required to signal the success/failure
     * of the drop by passing an appropriate
     * <code>boolean</code> to the <code>DropTargetDropEvent</code>'s
     * dropComplete(boolean success) method.
     * <P>
     * Note: The data transfer should be completed before the call  to the
     * <code>DropTargetDropEvent</code>'s dropComplete(boolean success) method.
     * After that, a call to the getTransferData() method of the
     * <code>Transferable</code> returned by
     * <code>DropTargetDropEvent.getTransferable()</code> is guaranteed to
     * succeed only if the data transfer is local; that is, only if
     * <code>DropTargetDropEvent.isLocalTransfer()</code> returns
     * <code>true</code>. Otherwise, the behavior of the call is
     * implementation-dependent.
     * <P>
     * @param evt the <code>DropTargetDropEvent</code>
     */
    public void drop(DropTargetDropEvent evt) {
        evt.acceptDrop(DnDConstants.ACTION_COPY);
        boolean success = false;
        try {
            if (fileChooser != null) {
                List files = (List) evt.getTransferable().getTransferData(DataFlavor.javaFileListFlavor);
                File[] fileArray = (File[]) files.toArray(new File[files.size()]);
                if (fileArray.length > 0) {
                    if (fileChooser.getUI() instanceof SubtreeFileChooserUI) {
                        // The file chooser has a settable root directory.
                        // -> Determine which directory to set as the root.
                        File dir = fileArray[0];

                        if (dir.isDirectory() && fileArray.length==1 //
                                && fileChooser.getFileSelectionMode() == JFileChooser.FILES_ONLY) {
                            // The user dropped a directory on a file chooser which
                            // only selects files
                            // -> Make the directory the current directory of the file chooser.

                            ((SubtreeFileChooserUI) fileChooser.getUI()).selectDirectory(dir);
                        } else {
                            // The user dropped a file or directory on a file chooser which
                            // selects files or directories (or both)
                            // -> Make the parent directory the current directory of the file chooser.

                            dir = dir.getParentFile();
                            if (dir != null) {
                                ((SubtreeFileChooserUI) fileChooser.getUI()).selectDirectory(dir);
                            }
                            fileChooser.setSelectedFiles(fileArray);
                        }
                    } else {
                        // The file chooser has not a settable root directory.
                        // -> Just set the selected files.
                        fileChooser.setSelectedFiles(fileArray);
                    }
                }
            }
            success = true;
        } catch (UnsupportedFlavorException e) {
        } catch (IOException e) {
        }
        evt.dropComplete(success);
    }
}
