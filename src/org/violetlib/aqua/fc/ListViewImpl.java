/*
 * Copyright (c) 2014-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.event.KeyListener;
import java.io.File;
import java.text.DateFormat;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.*;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.*;
import org.violetlib.treetable.*;

/**
 * An implementation of a file chooser list view.
 */
public class ListViewImpl extends ListView {

    protected final TreeTable tree;
    protected final JScrollPane listViewScrollPane;

    protected final MyTableColumnModel tableColumnModel;
    private Font labelFont;
    private int COLUMN_MARGIN = 10;
    private GenericCellRenderer fileRenderer;
    protected final JFileChooser fc;
    private final TreeSelectionListener treeSelectionListener;
    protected Border tableHeaderBorder;

    public ListViewImpl(JFileChooser fc) {

        this.fc = fc;

        labelFont = UIManager.getFont("FileChooser.listView.font");  // probably not needed
        treeSelectionListener = new MyTreeSelectionListener();

        setFocusable(false);

        /*
          A fake tree model is needed to work around a problem. JTree maintains a cache of tree model nodes for layout
          purposes. The cache is not flushed when a new tree model is supplied. The result is that the model adapter in
          TreeTable tries to ask the new model whether a cached node is a leaf. This produces a ClassCastException in
          the new model.
        */

        TreeModel fakeTreeModel = new FileSystemTreeModel(fc);
        tableColumnModel = createColumnModel();
        tree = new MyTreeTable(fakeTreeModel, new MyTreeColumnModel(), tableColumnModel);
        tree.setUI(new MyTreeTableUI(fc, tree));
        tree.putClientProperty("JTree.style", "striped");   // this probably has no effect
        if (OSXSystemProperties.OSVersion >= 1016) {
            tree.putClientProperty("JTree.viewStyle", "inset");
        }
        tree.setRootVisible(false);
        tree.setAlternateRowColor(tree.getBackground());
        tree.setBackground(UIManager.getColor("List.alternateBackground.0"));
        tree.setNodeSortingEnabled(true);
        tree.setRowMargin(0);
        tree.setRowHeight(18);
        tree.setOpaque(true);

        listViewScrollPane = new JScrollPane();
        listViewScrollPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        listViewScrollPane.setViewportView(tree);
        listViewScrollPane.setColumnHeaderView(tree.getTableHeader());

        setLayout(new BorderLayout());
        add(listViewScrollPane);

        tableHeaderBorder = new EmptyBorder(3, 3, 3, 0);

        JTableHeader header = tree.getTableHeader();
        TableCellRenderer defaultHeaderRenderer = header.getDefaultRenderer();
        TableCellRenderer leftHeaderRenderer = createHeaderCellRenderer(defaultHeaderRenderer, SwingConstants.LEFT);
        TableCellRenderer rightHeaderRenderer = createHeaderCellRenderer(defaultHeaderRenderer, SwingConstants.RIGHT);
        tableColumnModel.nameColumn.setHeaderRenderer(leftHeaderRenderer);
        tableColumnModel.dateModifiedColumn.setHeaderRenderer(leftHeaderRenderer);
        tableColumnModel.sizeColumn.setHeaderRenderer(rightHeaderRenderer);
        tableColumnModel.kindColumn.setHeaderRenderer(leftHeaderRenderer);
    }

    @Override
    public void setActive(boolean b) {
        TreeSelectionModel sm = tree.getSelectionModel();

        if (b) {
            sm.addTreeSelectionListener(treeSelectionListener);
        } else {
            sm.removeTreeSelectionListener(treeSelectionListener);
        }
    }

    @Override
    public boolean requestFocusInWindow() {
        return tree.requestFocusInWindow();
    }

    @Override
    public synchronized void addKeyListener(KeyListener l) {
        tree.addKeyListener(l);
    }

    protected class MyTreeTable extends TreeTable {
        public MyTreeTable(TreeModel tm, TreeColumnModel tcm, TableColumnModel cm) {
            super(tm, tcm, cm);
        }

        @Override
        public Icon getIcon(Object node, boolean expanded, boolean leaf) {
            if (node instanceof FileInfo) {
                FileInfo info = (FileInfo) node;
                return info.getIcon();
            }

            return super.getIcon(node, expanded, leaf);
        }
    }

    protected class MyTreeRowSorter extends DefaultTreeTableSorter<TreeModel,TreeColumnModel,Object> {
        public MyTreeRowSorter(TreeTable tree) {
            super(tree.getTreeModel(), tree.getTreeColumnModel());
            setComparator(0, new FileSystemTreeModel.ByNameComparator());
            setComparator(1, new FileSystemTreeModel.ByDateComparator());
            setComparator(2, new FileSystemTreeModel.BySizeComparator());
            setComparator(3, new FileSystemTreeModel.ByKindComparator());
        }
    }

    @Override
    protected void updateForNewModel() {
        tree.setTreeModel(model);
        tree.setRowSorter(new MyTreeRowSorter(tree));
        tree.revalidate();
        tree.repaint();
    }

    @Override
    public void reconfigure() {
        setMultipleSelection(fc.isMultiSelectionEnabled());
        tree.repaint();
    }

    @Override
    public void setFileRenderer(GenericCellRenderer r) {
        fileRenderer = r;
        tree.repaint();
    }

    @Override
    public void setMultipleSelection(boolean b) {
        tree.getSelectionModel().setSelectionMode(b ? TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION : TreeSelectionModel.SINGLE_TREE_SELECTION);
    }

    @Override
    public void setSelection(TreePath path) {
        tree.setSelectionPath(path);
    }

    @Override
    public void setSelection(java.util.List<TreePath> paths) {
        TreePath[] ps = paths.toArray(new TreePath[paths.size()]);
        tree.setSelectionPaths(ps);
    }

    @Override
    public java.util.List<TreePath> getSelection() {
        TreePath[] ps = tree.getSelectionPaths();
        return ps != null ? new ArrayList<TreePath>(Arrays.asList(ps)) : new ArrayList<TreePath>();
    }

    @Override
    public void ensurePathIsVisible(TreePath path) {
        tree.scrollPathToVisible(path);
    }

    protected class MyTreeSelectionListener implements TreeSelectionListener {
        @Override
        public void valueChanged(TreeSelectionEvent e) {
            ListViewImpl.this.selectionChanged();
        }
    }

    protected class MyTreeTableUI extends AquaFileChooserTreeTableUI {

        public MyTreeTableUI(JFileChooser fc, TreeTable tt) {
            super(fc, tt);
        }

        @Override
        protected void select(TreePath path) {
            ListViewImpl.this.select(path);
        }
    }

    protected MyTableColumnModel createColumnModel() {
        return new MyTableColumnModel();
    }

    protected class MyTableColumnModel extends DefaultTableColumnModel {
        public TableColumn nameColumn;
        public TableColumn dateModifiedColumn;
        public TableColumn sizeColumn;
        public TableColumn kindColumn;

        public MyTableColumnModel() {
            nameColumn = new MyTableColumn(0, 200, true, "Name");
            dateModifiedColumn = new MyTableColumn(1, 90, false, "Date Modified");
            sizeColumn = new MyTableColumn(2, 80, false, "Size");
            kindColumn = new MyTableColumn(3, 150, false, "Kind");

            nameColumn.setCellRenderer(new MyNameCellRenderer());
            dateModifiedColumn.setCellRenderer(new MyDateCellRenderer());
            sizeColumn.setCellRenderer(new MySizeCellRenderer());
            kindColumn.setCellRenderer(new MyKindCellRenderer());

            addColumn(nameColumn);
            addColumn(dateModifiedColumn);
            addColumn(sizeColumn);
            addColumn(kindColumn);

            setColumnMargin(COLUMN_MARGIN);
        }
    }

    protected class MyTableColumn extends TableColumn {

        /*
          Seems like a bug in JTable that the column margin is not included in the column layout calculation.
        */

        public MyTableColumn(int modelIndex, int width, boolean canWiden, String name) {
            super(modelIndex, width + COLUMN_MARGIN);
            if (!canWiden) {
                setMaxWidth(getWidth());
            }
            setHeaderValue(name);
        }
    }

    protected class MyCellRenderer extends DefaultTreeTableCellRenderer {
        protected @NotNull String textColorName;

        public MyCellRenderer(int alignment, @NotNull String textColorName) {
            this.textColorName = textColorName;
            setHorizontalAlignment(alignment);
            setFont(labelFont);
        }

        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable, Object value, boolean selected, boolean hasFocus, int row, int column) {
            hasFocus = false;   // avoid special display of focused cell
            value = getCellValue(value);
            Component c = super.getTreeTableCellRendererComponent(treeTable, value, selected, hasFocus, row, column);
            return fix(c, selected);
        }

        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable, Object value, boolean selected, boolean hasFocus, int row, int column, boolean expanded, boolean leaf) {
            value = getCellValue(value);
            Component c = super.getTreeTableCellRendererComponent(treeTable, value, selected, hasFocus, row, column, expanded, leaf);
            return fix(c, selected);
        }

        protected Object getCellValue(Object o) {
            return o;
        }

        protected Component fix(Component c, boolean selected) {
            AquaAppearance appearance = AppearanceManager.ensureAppearance(c);
            Color fg = appearance.getColor(selected ? "alternateSelectedControlText" : textColorName);
            if (fg == null) {
                fg = Color.BLACK;
            }
            c.setForeground(AquaAppearance.getOrdinaryColor(fg));
            return c;
        }
    }

    private class MyNameCellRenderer extends MyCellRenderer {
        private MyNameCellRenderer() {
            super(SwingConstants.LEFT, "label");
        }

        @Override
        public Component getTreeTableCellRendererComponent(TreeTable treeTable, Object value, boolean selected, boolean hasFocus, int row, int column, boolean expanded, boolean leaf) {
            AquaAppearance appearance = AppearanceManager.ensureAppearance(treeTable);
            ContainerContextualColors colors = AquaColors.STRIPED_CONTAINER_COLORS;
            Component c = fileRenderer.getCellRendererComponent(treeTable, appearance, colors, value, selected, hasFocus);
            if (c == null) {
                c = super.getTreeTableCellRendererComponent(treeTable, value, selected, hasFocus, row, column, expanded, leaf);
            }

            return c;
        }
    }

    private class MyDateCellRenderer extends MyCellRenderer {
        private MyDateCellRenderer() {
            super(SwingConstants.LEFT, "secondaryLabel");
        }

        protected Object getCellValue(Object o) {
            FileSystemTreeModel.Node pn = (FileSystemTreeModel.Node) o;
            File f = pn.lazyGetResolvedFile();
            return getModifiedString(f, tableColumnModel.dateModifiedColumn.getWidth());
        }
    }

    private class MySizeCellRenderer extends MyCellRenderer {
        private MySizeCellRenderer() {
            super(SwingConstants.RIGHT, "secondaryLabel");
        }

        protected Object getCellValue(Object o) {
            FileSystemTreeModel.Node pn = (FileSystemTreeModel.Node) o;
            return getLengthString(pn.getFileLength());
        }
    }

    private class MyKindCellRenderer extends MyCellRenderer {
        private MyKindCellRenderer() {
            super(SwingConstants.LEFT, "secondaryLabel");
        }

        protected Object getCellValue(Object o) {
            FileSystemTreeModel.Node pn = (FileSystemTreeModel.Node) o;
            return pn.getFileKind();
        }
    }

    protected TableCellRenderer createHeaderCellRenderer(TableCellRenderer defaultRenderer, int align) {
        return new MyHeaderRenderer(align);
    }

    protected class MyHeaderRenderer extends AquaTableHeaderCellRenderer {

        public MyHeaderRenderer(int alignment) {
            setHorizontalAlignment(alignment);
        }

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            JComponent c = (JComponent) super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            Border border = c.getBorder();
            Border b = border != null ? new CompoundBorder(border, tableHeaderBorder) : tableHeaderBorder;
            c.setBorder(b);

            if (AquaTableHeaderCellRenderer.getColumnSortOrder(table, column) != null) {
                // Display sorted column title in bold
                Font f = c.getFont();
                c.setFont(f.deriveFont(Font.BOLD));
            }

            return c;
        }
    }

    protected class MyTreeColumnModel extends AbstractTreeColumnModel {
        private java.util.List<String> columnNames = new ArrayList<String>();

        public MyTreeColumnModel() {
            columnNames.add("Name");
            columnNames.add("Date Modified");
            columnNames.add("Size");
            columnNames.add("Kind");
        }

        @Override
        public int getColumnCount() {
            return columnNames.size();
        }

        @Override
        public String getColumnName(int column) {
            return columnNames.get(column);
        }

        @Override
        public Object getValueAt(Object node, int column) {
            // To support sorting, all cell values are the node.
            return (FileSystemTreeModel.Node) node;
        }
    }

    // Copied from FilePreview

    protected String getLengthString(long fileLength) {
        if (fileLength < 0) {
            return "--";
        } else {
            float scaledLength;
            String label;
            if (fileLength >= 1000000000l) {
                label = "FileChooser.sizeGBytesOnly";
                scaledLength = (float) fileLength / 1000000000l;
            } else if (fileLength >= 1000000l) {
                label = "FileChooser.sizeMBytesOnly";
                scaledLength = (float) fileLength / 1000000l;
            } else if (fileLength >= 1024) {
                label = "FileChooser.sizeKBytesOnly";
                scaledLength = (float) fileLength / 1000;
            } else {
                label = "FileChooser.sizeBytesOnly";
                scaledLength = (float) fileLength;
            }

            String format = UIManager.getString(label);
            if (format != null) {
                return MessageFormat.format(format, scaledLength, fileLength);
            } else {
                return "" + fileLength;
            }
        }
    }

    protected String getModifiedString(File f, int width) {
        if (f != null) {
            Date d = new Date(f.lastModified());
            if (width < 150) {
                DateFormat df = DateFormat.getDateInstance(DateFormat.SHORT);
                return df.format(d);
            } else {
                DateFormat df = DateFormat.getDateInstance();
                return df.format(d);
            }
        } else {
            return "";
        }
    }
}
