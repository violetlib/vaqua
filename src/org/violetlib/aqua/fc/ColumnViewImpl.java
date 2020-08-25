/*
 * Copyright (c) 2014-2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.dnd.DropTarget;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.violetlib.aqua.AquaUtils;
import org.violetlib.aqua.OSXSystemProperties;

/**
 * An implementation of column view.
 */
public class ColumnViewImpl extends ColumnView {

    public static final String PREVIEW_COMPONENT_CLIENT_PROPERTY_KEY = "JFileChooser.previewComponent";

    protected final JFileChooser fc;
    protected final AquaFileChooserBrowser browser;
    protected final JScrollPane browserScrollPane;
    private final MouseListener mouseListener;
    private final TreeSelectionListener treeSelectionListener;
    private boolean isActive;

    public ColumnViewImpl(JFileChooser fc) {
        this.fc = fc;

        mouseListener = createDoubleClickListener();
        treeSelectionListener = new MyTreeSelectionListener();

        setFocusable(false);

        int version = OSXSystemProperties.OSVersion;

        int columnMinimumWidth;

        if (version <= 1013) {
            columnMinimumWidth = 164;
        } else {
            columnMinimumWidth = 206;
        }

        browser = new ColumnViewBrowser(fc);
        browser.setColumnMinimumWidth(columnMinimumWidth);
        browser.setShowCellTipOrigin((Point) UIManager.get("FileChooser.cellTipOrigin"));
        browser.setShowCellTips(true);
        browser.setPreviewColumnFilled(true);

        int minimumHeight;

        if (version <= 1011) {
            minimumHeight = 332;
        } else if (version == 1012) {
            minimumHeight = 203;
        } else {
            minimumHeight = 100;
        }

        setMinimumSize(new Dimension(720, minimumHeight));

        browserScrollPane = new JScrollPane();
        browserScrollPane.setViewport(new BrowserViewport());
        browserScrollPane.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        browserScrollPane.setViewportView(browser);
        browserScrollPane.putClientProperty("Quaqua.Component.visualMargin", new Insets(3, 2, 3, 2));
        //browserScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        browserScrollPane.setFocusable(false);
        browserScrollPane.getVerticalScrollBar().setFocusable(false);
        browserScrollPane.getHorizontalScrollBar().setFocusable(false);

        setLayout(new BorderLayout());
        add(browserScrollPane);
    }

    protected class ColumnViewBrowser extends AquaFileChooserBrowser {
        public ColumnViewBrowser(JFileChooser fc) {
            super(fc);
        }

        @Override
        protected void fileSelectedInSavePanel(File f) {
            if (isActive) {
                SubtreeTreeModel model = (SubtreeTreeModel) getModel();
                FileSystemTreeModel fullModel = (FileSystemTreeModel) model.getTargetModel();
                TreePath path = fullModel.toPath(f, null);
                select(path);
            }
        }

        @Override
        public boolean getScrollableTracksViewportWidth() {

            // If we are filling the preview column, then widen the view as needed to fill the viewport.

            if (browser.isPreviewColumnFilled()) {
                JViewport vp = (JViewport) getParent();
                int viewPreferredWidth = vp.getPreferredSize().width;
                Dimension extentSize = vp.getExtentSize();
                if (extentSize.width > viewPreferredWidth) {
                    return true;
                }
            }
            return false;
        }

        @Override
        protected void previewColumnVisibilityChanged(boolean isVisible) {
            AquaFileChooserUI ui = AquaUtils.getUI(fc, AquaFileChooserUI.class);
            if (ui != null) {
                ui.configureDialogSize();
            }
        }
    }

    @Override
    public void setActive(boolean b) {
        this.isActive = b;
        if (b) {
            browser.addMouseListener(mouseListener);
            browser.addTreeSelectionListener(treeSelectionListener);
        } else {
            browser.removeMouseListener(mouseListener);
            browser.removeTreeSelectionListener(treeSelectionListener);
        }
    }

    @Override
    public void setModel(SubtreeTreeModel m) {
        browser.setModel(m);
        FileSystemTreeModel fullModel = (FileSystemTreeModel) m.getTargetModel();
        browser.setPrototypeCellValue(fullModel.getPrototypeValue());
    }

    @Override
    public void setFileRenderer(GenericCellRenderer r) {
        browser.setColumnCellRenderer((ListCellRenderer) r);
    }

    @Override
    public void setMultipleSelection(boolean b) {
        if (b) {
            browser.setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        } else {
            browser.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        }
    }

    @Override
    public java.util.List<TreePath> getSelection() {
        TreePath[] ps = browser.getSelectionPaths();
        return ps != null ? new ArrayList<TreePath>(Arrays.asList(ps)) : new ArrayList<TreePath>();
    }

    @Override
    public void setSelection(TreePath path) {
        browser.setSelectionPath(path);
    }

    @Override
    public void setSelection(java.util.List<TreePath> paths) {
        TreePath[] ps = paths.toArray(new TreePath[paths.size()]);
        browser.setSelectionPaths(ps);
    }

    @Override
    public void ensurePathIsVisible(TreePath path) {
        browser.ensurePathIsVisible(path);
    }

    private void installPreviewComponent() {
        boolean isSave = isFileNameFieldVisible();
        if (isSave) {
            browser.setPreviewRenderer(null);
        } else {
            Component specifiedPreviewComponent = (Component) fc.getClientProperty(PREVIEW_COMPONENT_CLIENT_PROPERTY_KEY);
            if (specifiedPreviewComponent != null) {
                browser.setPreviewRenderer(new BrowserPreviewRenderer() {
                    public Component getPreviewRendererComponent(JBrowser browser, TreePath[] paths) {
                        return specifiedPreviewComponent;
                    }
                });
            } else {
                BrowserPreviewRenderer oldRenderer = browser.getPreviewRenderer();
                if (!(oldRenderer instanceof FilePreview)) {
                    BrowserPreviewRenderer previewRenderer = new FilePreview(fc);
                    browser.setPreviewRenderer(previewRenderer);
                }
            }
        }
    }

    @Override
    public boolean requestFocusInWindow() {
        return browser.requestFocusInWindow();
    }

    @Override
    public synchronized void addKeyListener(KeyListener l) {
        browser.addKeyListener(l);
    }

    @Override
    public void reconfigure() {
        setMultipleSelection(fc.isMultiSelectionEnabled());
        browser.repaint();
        installPreviewComponent();
        browser.updatePreviewColumn();
    }

    @Override
    public synchronized void setDropTarget(DropTarget dt) {
        super.setDropTarget(dt);
        browser.setDropTarget(dt);
    }

    protected MouseListener createDoubleClickListener() {
        return new DoubleClickListener();
    }

    protected class BrowserViewport extends JViewport {
        @Override
        public Dimension getMinimumSize() {
            Component view = getView();
            return view.getMinimumSize();
        }
    }

    protected class MyTreeSelectionListener implements TreeSelectionListener {
        @Override
        public void valueChanged(TreeSelectionEvent e) {
            selectionChanged();
        }
    }

    protected class DoubleClickListener extends MouseAdapter {
        @Override
        public void mouseClicked(MouseEvent e) {
            // Note: We must not react on mouse clicks with clickCount=1.
            //       Because this interferes with the mouse handling code in
            //       the JBrowser which does list selection.
            if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount() == 2 && fc.getDialogType() != JFileChooser.SAVE_DIALOG) {

                TreePath path = browser.getPathForLocation(e.getX(), e.getY());
                if (path != null) {
                    // Only react on double click if all selected files are
                    // acceptable
                    for (TreePath tp : browser.getSelectionPaths()) {
                        FileSystemTreeModel.Node n = (FileSystemTreeModel.Node) tp.getLastPathComponent();
                        if (!fc.accept(n.getFile())) {
                            return;
                        }
                    }
                    ColumnViewImpl.this.select(path);
                }
            }
        }
    }

    /**
     * Returns true, if the file name field is visible.
     */
    protected boolean isFileNameFieldVisible() {
        return (fc.getDialogType() == JFileChooser.SAVE_DIALOG) || (fc.getDialogType() == JFileChooser.CUSTOM_DIALOG);
    }
}
