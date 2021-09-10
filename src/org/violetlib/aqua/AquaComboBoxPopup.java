/*
 * Changes Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.plaf.basic.BasicComboPopup;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@SuppressWarnings("serial") // Superclass is not serializable across versions
class AquaComboBoxPopup extends BasicComboPopup implements AquaExtendedPopup, ListDataListener {

    protected static final String uiClassID = "ComboBoxPopupMenuUI";

    public enum PopupDisplayType {
        EDITABLE_NO_SCROLL,     // an editable combo box pop up menu with no scroll pane
        EDITABLE_SCROLL,        // an editable combo box pop up menu with a scroll pane
        CONTEXTUAL              // a non editable combo box pop up menu (similar to a contextual menu)
    }

    protected PopupDisplayType currentDisplayType;

    protected static MouseWheelListener mouseWheelEventConsumer = new MouseWheelListener() {
        @Override
        public void mouseWheelMoved(MouseWheelEvent e) {
            e.consume();
        }
    };

    public AquaComboBoxPopup(JComboBox<Object> cBox) {
        super(cBox);
    }

    public AquaComboBoxType getComboBoxType() {
        return comboBox != null ? AquaComboBoxUI.getComboBoxType(comboBox) : null;
    }

    @Override
    public String getUIClassID() {
        return uiClassID;
    }

    @Override
    protected ListDataListener createListDataListener() {
        return this;
    }

    @Override
    public void intervalAdded(ListDataEvent e) {
        possiblyUpdatePopup();
    }

    @Override
    public void intervalRemoved(ListDataEvent e) {
        possiblyUpdatePopup();
    }

    @Override
    public void contentsChanged(ListDataEvent e) {
    }

    @Override
    protected void configurePopup() {
        // This is "static" configuration that happens when the UI object is created.

        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorderPainted(true);
        setOpaque(false);
        setDoubleBuffered(true);
        setFocusable(false);

        updateContents(false);

        // TODO: CPlatformWindow?
        putClientProperty("apple.awt._windowFadeOut", 150);
    }

    @Override
    protected JScrollPane createScroller() {
        JScrollPane sp = super.createScroller();
        sp.setOpaque(false);
        sp.getViewport().setOpaque(false);
        return sp;
    }

    /**
     * If the number of menu items change, we may need to change the size of the popup menu, and we many need to
     * install or remove a scroll pane.
     */
    private void possiblyUpdatePopup() {
        Rectangle newBounds = adjustPopupAndGetBounds(false);
        list.setSize(newBounds.width, newBounds.height);
        pack();
    }

    // There are several different presentations:
    //
    // An editable combo box uses a list. The list has a maximum number of displayed rows. If there are more elements,
    // then a scroll bar is used.
    //
    // A pop up or pull down button simulates a standard pop up menu. A pop up menu will expand to fill the screen if
    // necessary. If there are items not visible, an upward or downward arrow will be displayed. The menu scrolls by
    // moving the mouse over (or beyond) the arrow. Scroll bars are never used.

    public void updateContents(boolean remove) {
        // The combo box popup scroll pane is used only if the combo box is editable and has too many items.

        if (list != null) {

            if (currentDisplayType == PopupDisplayType.EDITABLE_SCROLL) {
                remove(list);
                if (remove) {
                    remove(scroller);
                }
                add(scroller);
                scroller.setViewportView(list);
                //list.setOpaque(true);
                //setOpaque(true);
            } else {
                remove(scroller);
                if (remove) {
                    remove(list);
                }
                add(list);
                //list.setOpaque(false);
                //setOpaque(false);
            }

            // The following is a workaround for bad behavior in BasicPopupMenuUI. If we do not consume a mouse wheel
            // event over the list, then BasicPopupMenuUI will dismiss the popup in response to the event. Even though
            // the mouse wheel event has no useful behavior on a non-scrollable list (on a native combo box it will
            // bounce scroll the list), the event should not dismiss the popup.
            list.removeMouseWheelListener(mouseWheelEventConsumer);
            if (currentDisplayType == PopupDisplayType.EDITABLE_NO_SCROLL) {
                list.addMouseWheelListener(mouseWheelEventConsumer);
            }

            if (remove) {
                AquaComboBoxPopupMenuUI ui = AquaUtils.getUI(this, AquaComboBoxPopupMenuUI.class);
                if (ui != null) {
                    ui.configure(comboBox, list);
                }
            }
        }

        setBorder(null);
    }

    protected Dimension getBestPopupSizeForRowCount(int maxRowCount) {
        int currentElementCount = comboBox.getModel().getSize();
        int rowCount = currentDisplayType == PopupDisplayType.EDITABLE_SCROLL
                ? Math.min(maxRowCount, currentElementCount) : currentElementCount;

        Dimension popupSize = new Dimension();
        ListCellRenderer<Object> renderer = list.getCellRenderer();

        for (int i = 0; i < rowCount; i++) {
            Object value = list.getModel().getElementAt(i);
            Component c = renderer.getListCellRendererComponent(list, value, i, false, false);
            Dimension prefSize = c.getPreferredSize();
            popupSize.height += prefSize.height;
            popupSize.width = Math.max(prefSize.width, popupSize.width);
        }

        //popupSize.width += 10;

        return popupSize;
    }

    protected @NotNull PopupDisplayType computePopupDisplayType() {
        if (comboBox.isEditable()) {
            return comboBox.getItemCount() > comboBox.getMaximumRowCount()
              ? PopupDisplayType.EDITABLE_SCROLL : PopupDisplayType.EDITABLE_NO_SCROLL;
        } else {
            return PopupDisplayType.CONTEXTUAL;
        }
    }

    @Override
    public void show() {
        int startItemCount = comboBox.getItemCount();

        Rectangle popupBounds = adjustPopupAndGetBounds(true);
        if (popupBounds == null) {
            return; // null means don't show
        }

        comboBox.firePopupMenuWillBecomeVisible();
        show(comboBox, popupBounds.x, popupBounds.y);

        // hack for <rdar://problem/4905531> JComboBox does not fire popupWillBecomeVisible if item count is 0
        int afterShowItemCount = comboBox.getItemCount();
        if (afterShowItemCount == 0) {
            hide();
            return;
        }

        if (startItemCount != afterShowItemCount) {
            Rectangle newBounds = adjustPopupAndGetBounds(false);
            list.setSize(newBounds.width, newBounds.height);
            pack();

            Point newLoc = comboBox.getLocationOnScreen();
            setLocation(newLoc.x + newBounds.x, newLoc.y + newBounds.y);
        }
        // end hack

        list.requestFocusInWindow();
    }

    @Override
    @SuppressWarnings("serial") // local class
    protected JList<Object> createList() {
        ListCellRenderer<Object> renderer = null;
        AquaComboBoxUI ui = AquaUtils.getUI(comboBox, AquaComboBoxUI.class);
        if (ui != null) {
            renderer = ui.getListCellRenderer();
        }
        return new AquaPopupMenuList(comboBox.getModel(), renderer);
    }

    protected static class AquaPopupMenuList extends JList<Object> {
        private boolean isRendererConfigured;

        public AquaPopupMenuList(ListModel<Object> dataModel, @Nullable ListCellRenderer<Object> renderer) {
            super(dataModel);
            setOpaque(false);
            isRendererConfigured = renderer != null;
            if (renderer != null) {
                super.setCellRenderer(renderer);
            }
        }

        @Override
        public void setCellRenderer(ListCellRenderer<? super Object> cellRenderer) {
            if (!isRendererConfigured) {
                super.setCellRenderer(cellRenderer);
            }
        }

        @Override
        public void processMouseEvent(MouseEvent e) {
            if (e.isMetaDown()) {
                e = new MouseEvent((Component)e.getSource(), e.getID(), e.getWhen(),
                  e.getModifiers() ^ InputEvent.META_MASK, e.getX(), e.getY(), e.getXOnScreen(), e.getYOnScreen(),
                  e.getClickCount(), e.isPopupTrigger(), MouseEvent.NOBUTTON);
            }
            super.processMouseEvent(e);
        }
    }

    protected Rectangle adjustPopupAndGetBounds(boolean forceConfiguration) {
        PopupDisplayType actualDisplayType = computePopupDisplayType();
        if (currentDisplayType != actualDisplayType) {
            currentDisplayType = actualDisplayType;
            updateContents(true);
        } else if (forceConfiguration) {
            updateContents(true);
        }

        Dimension popupSize = getBestPopupSizeForRowCount(comboBox.getMaximumRowCount());
        Rectangle popupBounds = computePopupBounds(0, comboBox.getBounds().height, popupSize.width, popupSize.height);

        Dimension realPopupSize = popupBounds.getSize();
        if (currentDisplayType == PopupDisplayType.EDITABLE_SCROLL) {
            scroller.setMaximumSize(realPopupSize);
            scroller.setPreferredSize(realPopupSize);
            scroller.setMinimumSize(realPopupSize);
            list.setMaximumSize(null);
            list.setPreferredSize(null);
            list.setMinimumSize(null);
        } else {
            list.setMaximumSize(realPopupSize);
            list.setPreferredSize(realPopupSize);
            list.setMinimumSize(realPopupSize);
        }

        list.invalidate();

        initializeListSelection();

        int selectedIndex = comboBox.getSelectedIndex();
        if (selectedIndex >= 0 && currentDisplayType == PopupDisplayType.EDITABLE_SCROLL) {
            Rectangle cellBounds = list.getCellBounds(selectedIndex, selectedIndex);
            scroller.getViewport().setViewPosition(cellBounds.getLocation());
        }

        //list.ensureIndexIsVisible(list.getSelectedIndex());
        return popupBounds;
    }

    protected void initializeListSelection() {
        AquaComboBoxUI ui = AquaUtils.getUI(comboBox, AquaComboBoxUI.class);
        if (ui != null) {
            if (ui.updateListSelectionFromEditor()) {
                return;
            }
        }

        int selectedIndex = comboBox.getSelectedIndex();
        if (selectedIndex == -1 || getComboBoxType() == AquaComboBoxType.PULL_DOWN_MENU_BUTTON) {
            list.clearSelection();
        } else {
            list.setSelectedIndex(selectedIndex);
        }
    }

    @Override
    protected Rectangle computePopupBounds(int xx, int yy, int pw, int ph) {
        // When this method is called, the X and Y parameters are not interesting.
        // The width and height represent the preferred size of the popup.

        // The preferred location of the popup is based on the display type. If there is not enough room below the
        // combo box, we will locate the pop up at a higher position. This method does not attempt to fit the pop up
        // horizontally on the screen. That adjustment is performed by AquaPopupMenuUI, which supports external
        // scrolling, except for editable combo box menus, which support internal scrolling.

        // Get the screen location of the combo box
        // Retain this location because our result must be relative to the combo box
        Point p = AquaUtils.getScreenLocation(comboBox);

        // Get the available bounds of the screen where the popup should appear.
        Rectangle scrBounds = AquaUtils.getScreenBounds(p, comboBox);

        if (currentDisplayType == PopupDisplayType.EDITABLE_SCROLL) {
            pw += 15;
        }

        Insets comboBoxInsets = comboBox.getInsets();
        Dimension comboBoxSize = comboBox.getSize();
        int minWidth = comboBoxSize.width - (comboBoxInsets.left + comboBoxInsets.right);
        pw = Math.max(minWidth, pw);
        int x = p.x + comboBoxInsets.left;
        if (!AquaUtils.isLeftToRight(comboBox)) {
            x = p.x + comboBoxSize.width - comboBoxInsets.right - pw;
        }

        AquaComboBoxType type = getComboBoxType();

        int yOffset = getNominalPopupYOffset();
        int y = p.y + yOffset;

        // For a pop up menu, the goal is for the menu item label to exactly overlay the combo box button label, at
        // least in the case where our default renderer is used.

        int labelYOffset = 0;
        if (type == AquaComboBoxType.POP_UP_MENU_BUTTON) {
            AquaComboBoxUI ui = AquaUtils.getUI(comboBox, AquaComboBoxUI.class);
            if (ui != null) {
                Point labelOffset = ui.getPopupButtonLabelOffset();
                if (labelOffset != null) {
                    x += labelOffset.x;
                    labelYOffset = labelOffset.y;
                }
            }
        }

        y += labelYOffset;

        putClientProperty(AquaPopupMenuUI.POP_UP_SELECTED_REGION, null);
        putClientProperty(AquaPopupMenuUI.POP_UP_SELECTED_REGION_LOCATION, null);

        int popupHeight = 0;    // 0 => fit to content
        int roomBelow = scrBounds.y + scrBounds.height - p.y;
        if (roomBelow < 100) {
            // Not enough room below the combo box, so instead display above, with a small gap.
            int gap = 4;
            int ybottom = p.y - gap;
            y = Math.max(scrBounds.y, ybottom - ph);
            popupHeight = ybottom - y;
        }

        // If the combo box is a pop up menu button, make sure the selected item is visible and try to position it
        // so that the selected menu item label is over the combo box button label.
        if (list != null && type == AquaComboBoxType.POP_UP_MENU_BUTTON) {
            int selectedIndex = comboBox.getSelectedIndex();
            if (selectedIndex >= 0) {
                Rectangle cellBounds = list.getCellBounds(selectedIndex, selectedIndex);
                cellBounds = new Rectangle(cellBounds.x, cellBounds.y, cellBounds.width, cellBounds.height+2);
                // For scrolling purposes, only the Y coordinate matters.
                Point regionScreenLocation = new Point(x, p.y + labelYOffset);
                putClientProperty(AquaPopupMenuUI.POP_UP_SELECTED_REGION, cellBounds);
                putClientProperty(AquaPopupMenuUI.POP_UP_SELECTED_REGION_LOCATION, regionScreenLocation);
            }
        }

        if (list != null && type == AquaComboBoxType.PULL_DOWN_MENU_BUTTON) {
            list.clearSelection();
        }

        // Unfortunately, the path by which the pop up location is transmitted from the popup to the popup menu UI is
        // interrupted by code in JPopupMenu that thinks it knows more than we do about where popup menus should appear
        // on the screen. The following is a workaround. It also allows us to force the popup height to be different
        // than the popup content height.

        putClientProperty(AquaPopupMenuUI.POP_UP_SCREEN_BOUNDS, new Rectangle(x, y, 0, popupHeight));

        return new Rectangle(x - p.x, y - p.y, pw, ph);
    }

    /**
     * Return the nominal Y offset of the popup relative to the top of the combo box.
     * The nominal offset may be replaced if there is not enough room on the screen.
     */
    protected int getNominalPopupYOffset() {

        AquaComboBoxUI ui = AquaUtils.getUI(comboBox, AquaComboBoxUI.class);
        if (ui != null) {
            return ui.getNominalPopupYOffset();
        } else {
            return comboBox.getHeight() + 2;
        }
    }

    @Override
    public void startArrowScroll() {
        if (list != null) {
            list.clearSelection();
        }
    }

    @Override
    public void stopArrowScroll() {
        if (list != null) {
            Point p = MouseInfo.getPointerInfo().getLocation();
            if (p != null) {
                SwingUtilities.convertPointFromScreen(p, list);
                int index = list.locationToIndex(p);
                if (index >= 0 && list.getSelectedIndex() != index) {
                    list.setSelectedIndex(index);
                }
            }
        }
    }

    @Override
    public void updateSelection(MouseEvent e) {
        updateListBoxSelectionForEvent(e, false);
    }

    @Override
    protected void updateListBoxSelectionForEvent(MouseEvent e, boolean shouldScroll) {
        // On an editable combo box, Aqua tracks the mouse when it is dragged, not when it is moved.
        AquaComboBoxType type = getComboBoxType();
        if (type != AquaComboBoxType.EDITABLE_COMBO_BOX || e.getID() == MouseEvent.MOUSE_DRAGGED) {
            super.updateListBoxSelectionForEvent(e, shouldScroll);
        }
    }

    @Override
    protected MouseListener createMouseListener() {
        return new AquaComboBoxPopupMouseListener();
    }

    protected class AquaComboBoxPopupMouseListener extends MouseAdapter implements MouseListener {

        // These methods copied from BasicComboPopup.Handler

        @Override
        public void mousePressed(MouseEvent e) {
            if (e.getSource() == list) {
                return;
            }

            if (!SwingUtilities.isLeftMouseButton(e) || !comboBox.isEnabled()) {
                return;
            }

            if (comboBox.isEditable()) {
                Component comp = comboBox.getEditor().getEditorComponent();
                if (shouldFocusEditor(comp)) {
                    comp.requestFocus();
                }
            } else if (comboBox.isRequestFocusEnabled()) {
                comboBox.requestFocus();
            }
            togglePopup();
        }

        protected boolean shouldFocusEditor(@NotNull Component c) {
            if (c instanceof JComponent) {
                JComponent jc = (JComponent) c;
                if (!jc.isRequestFocusEnabled()) {
                    return false;
                }
            }
            if (isTextured()) {
                return false;
            }
            return true;
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            if (e.getSource() == list) {
                if (list.getModel().getSize() > 0) {
                    // JList mouse listener
                    if (comboBox.getSelectedIndex() == list.getSelectedIndex()) {
                        comboBox.getEditor().setItem(list.getSelectedValue());
                    }
                    comboBox.setSelectedIndex(list.getSelectedIndex());
                }
                comboBox.setPopupVisible(false);
                // workaround for cancelling an edited item (bug 4530953)
                if (comboBox.isEditable() && comboBox.getEditor() != null) {
                    comboBox.configureEditor(comboBox.getEditor(),
                                             comboBox.getSelectedItem());
                }
                return;
            }
            // JComboBox mouse listener
            Component source = (Component)e.getSource();
            Dimension size = source.getSize();
            Rectangle bounds = new Rectangle( 0, 0, size.width, size.height);
            if ( !bounds.contains( e.getPoint() ) ) {
                MouseEvent newEvent = convertMouseEvent( e );
                Point location = newEvent.getPoint();
                Rectangle r = new Rectangle();
                list.computeVisibleRect( r );
                if ( r.contains( location ) ) {
                    if (comboBox.getSelectedIndex() == list.getSelectedIndex()) {
                        comboBox.getEditor().setItem(list.getSelectedValue());
                    }
                    comboBox.setSelectedIndex(list.getSelectedIndex());
                }
                comboBox.setPopupVisible(false);
            }
            hasEntered = false;
            stopAutoScrolling();
        }
    }

    protected boolean isTextured() {
        AquaComboBoxUI ui = AquaUtils.getUI(comboBox, AquaComboBoxUI.class);
        return ui != null && ui.isTextured;
    }
}
