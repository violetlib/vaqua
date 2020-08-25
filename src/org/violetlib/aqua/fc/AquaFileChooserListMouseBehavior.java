/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import org.violetlib.aqua.AquaListMouseBehavior;
import org.violetlib.aqua.GenericList;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.io.File;

/**
 * Customize the mouse behavior of JLists in the file chooser browser. Some behaviors of NSOpenPanel are quite bizarre,
 * presumably implementation artifacts rather than design choices. This class does not try to imitate bizarre behavior.
 */
public class AquaFileChooserListMouseBehavior extends AquaListMouseBehavior {

    /**
     * The file selection handler is invoked when the user clicks on an unselectable item in a Save dialog.
     */
    public interface FileSelectionHandler {
        void fileSelected(File f);
    }

    protected final JFileChooser fc;
    protected FileSelectionHandler fileSelectionHandler;

    protected final int OP_INSTALL_NAME = 1000; // install the file name in the text field

    protected boolean isAcceptableTarget;
    protected boolean isTraversableDirectory;

    public AquaFileChooserListMouseBehavior(JFileChooser fc, GenericList list) {
        super(list);
        this.fc = fc;
    }

    public void setFileSelectionHandler(FileSelectionHandler h) {
        fileSelectionHandler = h;
    }

    @Override
    protected void interpretLocation(MouseEvent e) {
        targetIndex = getIndex(e);
        dragWouldTransfer = list.isDragEnabled();
        isAcceptableTarget = isAcceptableTarget(targetIndex);
        isTraversableDirectory = isTraversable(targetIndex);
    }

    @Override
    protected int getSelectionOperation(MouseEvent e) {

        int op = super.getSelectionOperation(e);

        /*
          The behavior of NSSavePanel is somewhat weird and not worth emulating. The simple model: an unmodified click
          on a traversable directory selects the directory in the browser, on anything else it transfers the item name
          to the text field without changing the browser selection. Modified clicks do nothing.
        */

        if (fc.getDialogType() == JFileChooser.SAVE_DIALOG) {
            if (op == OP_SELECT) {
                return isTraversableDirectory ? OP_SELECT : OP_INSTALL_NAME;
            }
            return 0;
        }

        /*
          Command-clicking a traversable directory that is not an acceptable result has no effect unless the selection
          is empty or the item is selected.
        */

        if (op == OP_TOGGLE && !isAcceptableTarget && isTraversableDirectory) {
            return list.isSelectionEmpty() || list.isRowSelected(targetIndex) ? op : 0;
        }

        /*
          Command-clicking a file in a files only chooser will deselect a selected directory.
        */

        if (op == OP_TOGGLE && isAcceptableTarget && !isTraversableDirectory && shouldDeselectDirectory()) {
            return OP_SELECT;
        }

        return op;
    }

    /**
     * Support the implementation of the behavior that trying to add a file (acceptable item) to a multiple selection
     * when the selection is a directory (traversable item) and the file chooser accepts only files should select the
     * file in place of (rather than in addition to) the directory. The code handles cases that do not normally arise
     * where the existing selection contains multiple items of which at least one is a directory.
     */
    private boolean shouldDeselectDirectory() {
        if (!fc.isMultiSelectionEnabled()) {
            return false;
        }

        if (fc.isDirectorySelectionEnabled()) {
            return false;
        }

        if (list.isRowSelected(targetIndex)) {
            return false;
        }

        if (list.isSelectionEmpty()) {
            return false;
        }

        int minIndex = list.getMinSelectionIndex();
        int maxIndex = list.getMaxSelectionIndex();
        for (int index = minIndex; index <= maxIndex; index++) {
            if (list.isRowSelected(index)) {
                if (isTraversable(index)) {
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    protected void performSelectionOperation(int op) {
        if (op == OP_INSTALL_NAME) {
            File f = getFile(targetIndex);
            if (f != null && fileSelectionHandler != null) {
                fileSelectionHandler.fileSelected(f);
            }
        } else {
            super.performSelectionOperation(op);
        }
    }

    @Override
    public void mousePressed(MouseEvent e) {

        super.mousePressed(e);

        if (mouseReleaseOp == OP_SELECT) {

            /*
              Nonstandard list behavior: Do not select an invalid item.
            */

            if (!isAcceptableTarget && !isTraversableDirectory) {
                mouseReleaseOp = 0;
            }
        }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        super.mouseReleased(e);

        /*
          If after all operations have been performed there are no selected items in the list, transfer the focus to the
          parent list.
        */

        if (list.isSelectionEmpty()) {
            JBrowser b = findBrowser(list.getComponent());
            if (b != null) {
                b.requestFocus();
            }
        }
    }

    @Override
    protected boolean isMousePressedOperation(int op) {
        if (op == OP_SELECT) {
            return false;
        } else {
            return super.isMousePressedOperation(op);
        }
    }

    private JBrowser findBrowser(Component c) {
        while (c != null) {
            if (c instanceof JBrowser) {
                return (JBrowser) c;
            }
            c = c.getParent();
        }
        return null;
    }

    /**
     * Indicate whether the designated list item is an acceptable file chooser selection.
     */
    private boolean isAcceptableTarget(int index) {
        if (index >= 0) {
            Object e = list.getRow(index);
            if (e instanceof FileInfo) {
                FileInfo fi = (FileInfo) e;
                return fi.isAcceptable();
            }
        }
        return false;
    }

    /**
     * Indicate whether the designated list item is a traversable directory.
     */
    private boolean isTraversable(int index) {
        if (index >= 0) {
            Object e = list.getRow(index);
            if (e instanceof FileInfo) {
                FileInfo fi = (FileInfo) e;
                return fi.isTraversable();
            }
        }
        return false;
    }

    @Override
    protected void trimSelectedInterval(int index, int direction) {
        if (direction > 0) {
            int count = list.getRowCount();
            while (++index < count) {
                if (list.isRowSelected(index)) {
                    list.removeSelectionInterval(index, index);
                } else if (isAcceptableTarget(index)) {
                    break;
                }
            }
        } else if (direction < 0) {
            while (--index >= 0) {
                if (list.isRowSelected(index)) {
                    list.removeSelectionInterval(index, index);
                } else if (isAcceptableTarget(index)) {
                    break;
                }
            }
        }
    }

    @Override
    protected void setSelectionInterval(int index1, int index2) {

        if (index1 == index2) {
            if (isAcceptableTarget(index1) || isTraversable(index1)) {
                list.setSelectionInterval(index1, index1);
            }
            return;
        }

        boolean old = list.isValueAdjusting();
        list.setValueIsAdjusting(true);
        list.clearSelection();
        int delta = index2 > index1 ? -1 : 1;
        int index = index2;
        for (;;) {
            if (isAcceptableTarget(index)) {
                list.addSelectionInterval(index, index);
            }
            if (index == index1) {
                break;
            }
            index += delta;
        }
        list.setValueIsAdjusting(old);
    }

    @Override
    protected void addSelectionInterval(int index1, int index2) {
        int delta = index1 > index2 ? -1 : 1;
        int index = index1;
        for (;;) {
            if (isAcceptableTarget(index)) {
                list.addSelectionInterval(index, index);
            } else {
                list.removeSelectionInterval(index, index);
            }
            if (index == index2) {
                return;
            }
            index += delta;
        }
    }

    @Override
    protected void toggleSelectionInterval(int index1, int index2) {
        int delta = index1 > index2 ? -1 : 1;
        int index = index1;
        for (;;) {
            if (isAcceptableTarget(index) && !list.isRowSelected(index)) {
                list.addSelectionInterval(index, index);
            } else {
                list.removeSelectionInterval(index, index);
            }
            if (index == index2) {
                return;
            }
            index += delta;
        }
    }

    /**
     * Return the designated list element as the corresponding file.
     */
    private File getFile(int index) {
        if (index >= 0) {
            Object e = list.getRow(index);
            if (e instanceof FileInfo) {
                FileInfo fi = (FileInfo) e;
                return fi.getFile();
            }
        }

        return null;
    }
}
