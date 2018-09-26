/*
 * @(#)FilePreview.java
 *
 * Copyright (c) 2009-2010 Werner Randelshofer, Switzerland.
 * Copyright (c) 2014-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.text.DateFormat;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.filechooser.FileSystemView;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.table.*;
import javax.swing.tree.TreePath;

import org.violetlib.aqua.*;

/**
 * The FilePreview is used to render the preview column in the file chooser browser view.
 *
 * @author  Werner Randelshofer
 */
public class FilePreview extends JComponent implements BrowserPreviewRenderer {

    private JFileChooser fileChooser;
    private JPanel emptyPreview;
    private FileInfo info;
    private JLabel nameView;
    private JLabel typeSizeView;
    private JTable attributeView;
    private Font labelFont;
    private Font valueFont;
    private Font typeSizeFont;
    private String labelDelimiter;
    private ScaledImageView previewImageView;
    private JProgressBar imageLoadingIndicator;
    private boolean imageIsLoading;
    private Timer imageLoadingTimer;
    private TableColumn nameColumn;
    private TableColumn valueColumn;
    private SimpleTableCellRenderer nameRenderer;
    private SimpleTableCellRenderer valueRenderer;

    public FilePreview(JFileChooser fileChooser) {
        this.fileChooser = fileChooser;

        previewImageView = new ScaledImageView();
        previewImageView.setMinimumSize(new Dimension(128, 128));
        previewImageView.setPreferredSize(new Dimension(128, 128));

        imageLoadingIndicator = createImageLoadingIndicator();

        imageLoadingTimer = new Timer(500, new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (imageIsLoading) {
                    imageLoadingIndicator.setVisible(true);
                }
            }
        });
        imageLoadingTimer.setRepeats(false);

        setBorder(BorderFactory.createEmptyBorder(3, 4, 4, 4));
        setLayout(new BorderLayout());

        labelFont = UIManager.getFont("FileChooser.previewLabelFont");
        valueFont = UIManager.getFont("FileChooser.previewValueFont");
        typeSizeFont = UIManager.getFont("FileChooser.previewTypeSizeFont");

        emptyPreview = new JPanel();
        emptyPreview.setOpaque(false);

        labelDelimiter = UIManager.getString("FileChooser.previewLabelDelimiter");
        if (labelDelimiter == null) {
            labelDelimiter = "";
        }

        int columnSeparation = 5;

        {
            TableColumnModel cm = new DefaultTableColumnModel();

            {
                nameColumn = new TableColumn();
                nameRenderer = new SimpleTableCellRenderer(labelFont);
                nameRenderer.setHorizontalAlignment(SwingConstants.RIGHT);
                Insets borderMargin = new Insets(0, 0, 0, 0);
                nameRenderer.putClientProperty("Quaqua.Component.visualMargin", borderMargin);
                nameColumn.setCellRenderer(nameRenderer);
                nameColumn.setModelIndex(0);
                cm.addColumn(nameColumn);
            }

            {
                valueColumn = new TableColumn();
                valueRenderer = new SimpleTableCellRenderer(valueFont);
                valueRenderer.setHorizontalAlignment(SwingConstants.LEFT);
                valueColumn.setCellRenderer(valueRenderer);
                valueColumn.setModelIndex(1);
                cm.addColumn(valueColumn);
            }

            attributeView = new JTable(null, cm);
            attributeView.setIntercellSpacing(new Dimension(columnSeparation, 0));
            attributeView.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
            attributeView.setFocusable(false);
            attributeView.setOpaque(false);
        }

        setOpaque(false);

        OverlayContainer imageHolder = new OverlayContainer();
        imageHolder.add(imageLoadingIndicator);
        imageHolder.add(previewImageView);
        imageHolder.setMinimumSize(new Dimension(128, 128));
        imageHolder.setPreferredSize(new Dimension(128, 128));
        add(imageHolder);

        Box vb = new Box(BoxLayout.Y_AXIS);
        add(vb, BorderLayout.SOUTH);

        if (OSXSystemProperties.OSVersion < 1010) {
            GrayLine b = new GrayLine();
            b.setBorder(new EmptyBorder(5, 25, 5, 25));
            vb.add(b);
        } else {
            nameView = new JLabel();
            nameView.setFont(UIManager.getFont("FileChooser.previewNameFont"));

            if (OSXSystemProperties.OSVersion >= 1014) {
                typeSizeView = new JLabel();
                typeSizeView.setFont(typeSizeFont);
            } else {
                valueRenderer.setRowZeroFont(typeSizeFont);
            }

            {
                Box p = new Box(BoxLayout.X_AXIS);
                p.add(Box.createHorizontalGlue());
                p.add(nameView);
                p.add(Box.createHorizontalGlue());
                vb.add(p);

                if (typeSizeView != null) {
                    p = new Box(BoxLayout.X_AXIS);
                    p.add(Box.createHorizontalGlue());
                    p.add(typeSizeView);
                    p.add(Box.createHorizontalGlue());
                    vb.add(p);
                }

                p.setBorder(new EmptyBorder(0, 0, 20, 0));
            }
        }

        {
            Box p = new Box(BoxLayout.X_AXIS);
            p.add(Box.createHorizontalGlue());
            p.add(attributeView);
            p.add(Box.createHorizontalGlue());
            p.setBorder(new EmptyBorder(0, 0, 40, 0));
            vb.add(p);
        }

        MouseListener mouseHandler = new MouseAdapter() {

            public void mouseClicked(MouseEvent evt) {
                if (evt.getClickCount() == 2) {
                    FilePreview.this.fileChooser.approveSelection();
                }
            }
        };
        addMouseListener(mouseHandler);
        Component[] c = getComponents();
        for (int i = 0; i < c.length; i++) {
            c[i].addMouseListener(mouseHandler);
        }
    }

    @Override
    protected void paintComponent(Graphics g) {

        AppearanceManager.ensureAppearance(this);
        Color background = AquaColors.getBackground(this, "controlBackground");
        Color labelForeground = AquaColors.getSystemColor(this, "secondaryLabel");
        Color valueForeground = AquaColors.getSystemColor(this, "label");

        nameRenderer.setColor(AquaColors.getOrdinaryColor(labelForeground));
        valueRenderer.setColor(AquaColors.getOrdinaryColor(valueForeground));
        if (typeSizeView != null) {
            typeSizeView.setForeground(AquaColors.getOrdinaryColor(labelForeground));
        } else if (OSXSystemProperties.OSVersion >= 1010) {
            valueRenderer.setRowZeroColor(AquaColors.getOrdinaryColor(labelForeground));
        }

        // Avoid the magic eraser when displayed as a sheet
        if (isOpaque()) {
            g.setColor(background);
            g.fillRect(0, 0, getWidth(), getHeight());
        }
    }

    protected JProgressBar createImageLoadingIndicator() {
        JProgressBar b = new JProgressBar();
        b.setIndeterminate(true);
        b.putClientProperty("JProgressBar.style", "circular");
        return b;
    }

    private String toOSXPath(File file) {
        StringBuffer buf = new StringBuffer();
        FileSystemView fsv = AquaFileSystemView.getAquaFileSystemView();
        if (file != null && file.isDirectory()) {
            buf.append(':');
        }
        while (file != null) {
            buf.insert(0, fileChooser.getName(file));
            file = fsv.getParentDirectory(file);
            if (file != null) {
                buf.insert(0, ':');
            }
        }
        return buf.toString();
    }

    public Component getPreviewRendererComponent(JBrowser browser, TreePath[] paths) {

        if (paths.length > 1) {
            return emptyPreview;
        }

        Locale locale = Locale.getDefault();
        NumberFormat nf = NumberFormat.getInstance(locale);
        nf.setMaximumFractionDigits(1);
        info = (FileInfo) paths[0].getLastPathComponent();

        if (!info.isAcceptable()) {
            return emptyPreview;
        }

        File file = info.getFile();
        String name = info.getUserName();
        String kind = OSXFile.getKindString(file);
        String size = getLengthString(info.getFileLength());
        Date lastUsedDate = OSXFile.getLastUsedDate(file);
        String modified = getModifiedString(file);

        if (nameView != null) {
            nameView.setText(name);
        }

        AttributeTableModel m = new AttributeTableModel();

        if (OSXSystemProperties.OSVersion < 1010) {
            m.add("name", name);
            m.add("kind", kind);
            if (size != null) {
                m.add("size", size);
            }
            m.add("modified", modified);

            if (lastUsedDate != null) {
                m.add("lastUsed", getLastUsedString(lastUsedDate));
            }
        } else {
            String s = kind;
            if (size != null) {
                s = s + " - " + size;
            }
            if (typeSizeView != null) {
                typeSizeView.setText(s);
            } else {
                m.add("", s); // special font and text color for first row
            }
            m.add("modified", modified);
            if (lastUsedDate != null) {
                m.add("lastUsed", getLastUsedString(lastUsedDate));
            }
        }

        if (true) {
            // The original of a symlink is not displayed by NSOpenPanel, but it seems useful.
            if (info.isAlias()) {
                File resolvedFile = info.lazyGetResolvedFile();
                if (resolvedFile != null) {
                    m.add("original", toOSXPath(resolvedFile));
                }
            }
        }

        attributeView.setModel(m);
        m.updatePreferredWidths();
        attributeView.revalidate();
        attributeView.repaint();
        updatePreviewImage();
        return this;
    }

    // Duplicated in ListViewImpl

    protected String getLengthString(long fileLength) {
        if (fileLength < 0) {
            return null;
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

    protected String getModifiedString(File f) {
        if (f != null) {
            return DateFormat.getInstance().format(new Date(f.lastModified()));
        } else {
            return null; // UIManager.getString("FileChooser.modifiedUnknown");
        }
    }

    protected String getLastUsedString(Date d) {
        return DateFormat.getInstance().format(d);
    }

    private class AttributeTableModel extends AbstractTableModel {
        private java.util.List<String> names = new ArrayList<String>();
        private java.util.List<String> values = new ArrayList<String>();
        private int nameWidth;
        private int valueWidth;

        public void add(String name, String value) {
            if (value != null && !value.isEmpty()) {
                String s = UIManager.getString("FileChooser." + name); // NOI18N
                String actualName = (s != null ? s : name) + labelDelimiter;
                names.add(actualName);
                values.add(value);
                nameWidth = Math.max(nameWidth, getTextWidth(actualName, labelFont, attributeView));
                valueWidth = Math.max(valueWidth, getTextWidth(value, valueFont, attributeView));
            }
        }

        public void updatePreferredWidths() {
            int fudge = 15;
            attributeView.getColumnModel().getColumn(0).setPreferredWidth(nameWidth + fudge);
            attributeView.getColumnModel().getColumn(1).setPreferredWidth(valueWidth + fudge);
        }

        @Override
        public int getRowCount() {
            return names.size();
        }

        @Override
        public int getColumnCount() {
            return 2;
        }

        @Override
        public String getColumnName(int columnIndex) {
            if (columnIndex == 0) {
                return "Attribute";
            } else if (columnIndex == 1) {
                return "Value";
            } else {
                return null;
            }
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return String.class;
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            if (columnIndex == 0) {
                return names.get(rowIndex);
            } else if (columnIndex == 1) {
                return values.get(rowIndex);
            } else {
                return null;
            }
        }
    }

    private void updatePreviewImage() {
        imageLoadingIndicator.setVisible(false);
        previewImageView.setVisible(false);
        previewImageView.setImage(null);
        imageIsLoading = false;

        if (info != null) {
            // Retrieving the file icon requires some potentially lengthy I/O
            // operations. Therefore we do this in a worker thread.
            File file = info.lazyGetResolvedFile();
            if (file != null) {
                imageIsLoading = true;
                boolean useQuickLook = UIManager.getBoolean("FileChooser.quickLookEnabled");
                PreviewWorker w = new PreviewWorker(file, useQuickLook);
                w.execute();
                imageLoadingTimer.start();
            }
        }
    }

    protected class QuickLookPreviewWorker extends SwingWorker<Image,Image> {

        private final File file;

        public QuickLookPreviewWorker(File file) {
            this.file = file;
        }

        @Override
        protected Image doInBackground() throws Exception {
            try {
                return OSXFile.getIconImage(file, 1600, true);
            } catch (UnsupportedOperationException ex) {
                return null;
            }
        }
    }

    protected class PreviewWorker extends SwingWorker<Image,Image> {

        private final File file;
        private final boolean useQuickLook;

        public PreviewWorker(File file, boolean useQuickLook) {
            this.file = file;
            this.useQuickLook = useQuickLook;
        }

        @Override
        protected Image doInBackground() throws Exception {
            if (useQuickLook) {
                QuickLookPreviewWorker w = new QuickLookPreviewWorker(file);
                w.execute();
                Image im = w.get(10, TimeUnit.SECONDS);
                w.cancel(true);
                if (im != null) {
                    publish(im);
                    return im;
                }
            }

            Image im = OSXFile.getIconImage(file, 512, false);
            publish(im);
            return im;
        }

        @Override
        protected void process(List<Image> chunks) {
            FilePreview.this.installPreviewImage(chunks.get(0));
        }
    }

    protected void installPreviewImage(Image im) {
        imageLoadingTimer.stop();
        imageIsLoading = false;
        imageLoadingIndicator.setVisible(false);
        previewImageView.setImage(im);
        if (im != null) {
            previewImageView.setVisible(true);
        }
    }

    public static int getTextWidth(String s, Font f, JComponent c) {
        Dimension size = getTextSize(s, f, c);
        return size != null ? size.width : 0;
    }

    public static Dimension getTextSize(String s, Font f, JComponent c) {
        if (f == null) {
            f = c.getFont();
            if (f == null) {
                return null;
            }
        }

        FontMetrics fm = null;

        try {
            // Workaround a Swing bug (probably old)
            fm = c.getFontMetrics(f);
        } catch (NullPointerException ex) {
        }

        if (fm == null) {
            return null;
        }

        int w = fm.stringWidth(s);
        int h = fm.getHeight();
        return new Dimension(w, h);
    }

    private static class SimpleTableCellRenderer extends JLabel implements TableCellRenderer {
        private Font f;
        private Color fg;
        private Font rowZeroFont;
        private Color rowZeroColor;

        public SimpleTableCellRenderer(Font f) {
            this.f = f;
            this.fg = Color.BLACK;  // temporary, configured later
            setOpaque(false);
        }

        public void setRowZeroFont(Font f) {
            this.rowZeroFont = f;
        }

        public void setRowZeroColor(Color c) {
            this.rowZeroColor = c;
        }

        public void setColor(Color c) {
            this.fg = c;
        }

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            setText((value == null) ? "" : value.toString());
            setFont(row == 0 && rowZeroFont != null ? rowZeroFont : f);
            setForeground(row == 0 && rowZeroColor != null ? rowZeroColor : fg);
            return this;
        }

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         *
         * @since 1.5
         */
        public void invalidate() {}

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         */
        public void validate() {}

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         */
        public void revalidate() {}

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         */
        public void repaint(long tm, int x, int y, int width, int height) {}

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         */
        public void repaint(Rectangle r) {}

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         */
        public void repaint() {}

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         */
        protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
            // Strings get interned...
            if (propertyName=="text"
                    || propertyName == "labelFor"
                    || propertyName == "displayedMnemonic"
                    || ((propertyName == "font" || propertyName == "foreground")
                        && oldValue != newValue
                        && getClientProperty(BasicHTML.propertyKey) != null)) {

                super.firePropertyChange(propertyName, oldValue, newValue);
            }
        }

        /**
         * Overridden for performance reasons.
         * See the <a href="#override">Implementation Note</a>
         * for more information.
         */
        public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {}
    }

    private static class GrayLine extends JComponent {

        @Override
        public Dimension getMinimumSize() {
            Insets s = getInsets();
            return new Dimension(0, s.top + s.bottom + 1);
        }

        @Override
        public Dimension getPreferredSize() {
            return getMinimumSize();
        }

        @Override
        public Dimension getMaximumSize() {
            Insets s = getInsets();
            return new Dimension(100000, s.top + s.bottom + 1);
        }

        @Override
        protected void paintComponent(Graphics g) {
            if (isOpaque()) {
                g.setColor(getBackground());
                g.fillRect(0, 0, getWidth(), getHeight());
            }

            Insets s = getInsets();
            g.setColor(new Color(217, 217, 217));
            g.fillRect(s.left, s.top, getWidth() - s.left - s.right, 1);
        }
    }

    private static class ScaledImageView extends JComponent {
        private Image im;

        public void setImage(Image im) {
            this.im = im;
            repaint();
        }

        @Override
        protected void paintComponent(Graphics g) {
            if (isOpaque()) {
                g.setColor(getBackground());
                g.fillRect(0, 0, getWidth(), getHeight());
            }

            if (im != null) {
                Insets s = getInsets();
                int left = s.left;
                int top = s.top;
                int cwidth = getWidth() - s.left - s.right;
                int cheight = getHeight() - s.top - s.bottom;
                float imwidth = im.getWidth(null);
                float imheight = im.getHeight(null);
                if (imwidth > 0 && imheight > 0) {
                    int size = Math.min(cwidth, cheight);
                    float scale = Math.min(cwidth/imwidth, cheight/imheight);
                    int extraLeft = (int) Math.max(0, (cwidth - imwidth*scale) / 2);
                    int extraTop = (int) Math.max(0, (cheight - imheight*scale) / 2);
                    g.drawImage(im, left + extraLeft, top + extraTop, size, size, null);
                }
            }
        }
    }
}
