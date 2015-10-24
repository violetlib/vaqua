/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.tree.TreePath;
import java.util.List;

/**
 * The file chooser list view.
 */
public abstract class ListView extends JPanel implements FileChooserView {

    protected SubtreeTreeModel model;

    private ChangeListener changeListener;
    private ChangeEvent changeEvent = new ChangeEvent(this);
    private SelectListener selectListener;

    public static ListView create(JFileChooser fc) {
        return new ListViewImpl(fc);
    }

    @Override
    public void setModel(SubtreeTreeModel m) {
        if (m != model) {
            model = m;
            updateForNewModel();
        }
    }

    @Override
    public void addSelectionChangeListener(ChangeListener l) {
        changeListener = l;
    }

    @Override
    public void addSelectListener(SelectListener l) {
        selectListener = l;
    }

    protected void selectionChanged() {
        if (changeListener != null) {
            changeListener.stateChanged(changeEvent);
        }
    }

    protected abstract void updateForNewModel();

    protected final void select(TreePath path) {
        if (selectListener != null) {
            selectListener.select(path);
        }
    }

    @Override
    public void ensureSelectionIsVisible() {
        List<TreePath> paths = getSelection();
        if (!paths.isEmpty()) {
            ensurePathIsVisible(paths.get(0));
        }
    }
}
