/*
 * Changes Copyright (c) 2015-2020 Alan Snyder.
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
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.*;
import java.text.AttributedCharacterIterator.Attribute;
import java.text.Format.Field;
import java.util.Calendar;
import java.util.Map;
import javax.swing.*;
import javax.swing.JSpinner.DefaultEditor;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.SpinnerUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.text.InternationalFormatter;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.Configuration;
import org.violetlib.jnr.aqua.SpinnerArrowsConfiguration;
import org.violetlib.jnr.aqua.SpinnerArrowsLayoutConfiguration;

/**
 * This is originally derived from BasicSpinnerUI, but they made everything private
 * so we can't subclass!
 */
public class AquaSpinnerUI extends SpinnerUI implements AquaComponentUI {
    private static final RecyclableSingleton<? extends PropertyChangeListener> propertyChangeListener = new RecyclableSingletonFromDefaultConstructor<PropertyChangeHandler>(PropertyChangeHandler.class);
    static PropertyChangeListener getPropertyChangeListener() {
        return propertyChangeListener.get();
    }

    private static final RecyclableSingleton<ArrowButtonHandler> nextButtonHandler = new RecyclableSingleton<ArrowButtonHandler>() {
        @Override
        protected ArrowButtonHandler getInstance() {
            return new ArrowButtonHandler("increment", true);
        }
    };
    static ArrowButtonHandler getNextButtonHandler() {
        return nextButtonHandler.get();
    }
    private static final RecyclableSingleton<ArrowButtonHandler> previousButtonHandler = new RecyclableSingleton<ArrowButtonHandler>() {
        @Override
        protected ArrowButtonHandler getInstance() {
            return new ArrowButtonHandler("decrement", false);
        }
    };
    static ArrowButtonHandler getPreviousButtonHandler() {
        return previousButtonHandler.get();
    }

    protected JSpinner spinner;
    protected SpinPainter spinPainter;
    protected final AquaUIPainter painter = AquaPainting.create();
    protected final static int SEPARATION = 3;

    public static ComponentUI createUI(JComponent c) {
        return new AquaSpinnerUI();
    }

    private void maybeAdd(Component c, String s) {
        if (c != null) {
            spinner.add(c, s);
        }
    }

    public void installUI(JComponent c) {
        this.spinner = (JSpinner)c;
        installDefaults();
        installListeners();
        TransparentButton next = createNextButton();
        TransparentButton prev = createPreviousButton();
        spinPainter = new SpinPainter(next, prev);

        maybeAdd(next, "Next");
        maybeAdd(prev, "Previous");
        maybeAdd(createEditor(), "Editor");
        maybeAdd(spinPainter, "Painter");

        updateEnabledState();
        installKeyboardActions();
        LookAndFeel.installProperty(spinner, "opaque", false);
    }

    public void uninstallUI(JComponent c) {
        uninstallDefaults();
        uninstallListeners();
        spinner = null;
        c.removeAll();
    }

    protected void installDefaults() {
        spinner.setLayout(createLayout());
        LookAndFeel.installBorder(spinner, "Spinner.border");
        AquaUtils.installFont(spinner, "Spinner.font");
    }

    protected void uninstallDefaults() {
        spinner.setLayout(null);
    }

    protected void installListeners() {
        spinner.addPropertyChangeListener(getPropertyChangeListener());
    }

    protected void uninstallListeners() {
        spinner.removePropertyChangeListener(getPropertyChangeListener());
    }

    protected LayoutManager createLayout() {
        return new SpinnerLayout();
    }

    protected PropertyChangeListener createPropertyChangeListener() {
        return new PropertyChangeHandler();
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    protected TransparentButton createPreviousButton() {
        TransparentButton b = new TransparentButton();
        b.addActionListener(getPreviousButtonHandler());
        b.addMouseListener(getPreviousButtonHandler());
        b.setInheritsPopupMenu(true);
        return b;
    }

    protected TransparentButton createNextButton() {
        TransparentButton b = new TransparentButton();
        b.addActionListener(getNextButtonHandler());
        b.addMouseListener(getNextButtonHandler());
        b.setInheritsPopupMenu(true);
        return b;
    }

    /**
     * {@inheritDoc}
     */
    public int getBaseline(JComponent c, int width, int height) {
        super.getBaseline(c, width, height);
        JComponent editor = spinner.getEditor();
        Insets insets = spinner.getInsets();
        width = width - insets.left - insets.right;
        height = height - insets.top - insets.bottom;
        if (width >= 0 && height >= 0) {
            int baseline = editor.getBaseline(width, height);
            if (baseline >= 0) {
                return insets.top + baseline;
            }
        }
        return -1;
    }

    /**
     * {@inheritDoc}
     */
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(
            JComponent c) {
        super.getBaselineResizeBehavior(c);
        return spinner.getEditor().getBaselineResizeBehavior();
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    class TransparentButton extends JButton implements SwingConstants {
        boolean interceptRepaints = false;

        public TransparentButton() {
            super();
            setUI(new BasicButtonUI()); // avoid becoming focusable when Full Keyboard Access is enabled
            setFocusable(false);
            // only intercept repaints if we are after this has been initialized
            // otherwise we can't talk to our containing class
            interceptRepaints = true;
        }

        public void paint(Graphics g) {}

        public void repaint() {
            // only intercept repaints if we are after this has been initialized
            // otherwise we can't talk to our containing class
            if (interceptRepaints) {
                if (spinPainter == null) return;
                spinPainter.repaint();
            }
            super.repaint();
        }
    }

    protected JComponent createEditor() {
        JComponent editor = spinner.getEditor();
        fixupEditor(editor);
        return editor;
    }

    protected void replaceEditor(JComponent oldEditor, JComponent newEditor) {
        spinner.remove(oldEditor);
        fixupEditor(newEditor);
        spinner.add(newEditor, "Editor");
    }

    protected void fixupEditor(JComponent editor) {
        if (!(editor instanceof DefaultEditor)) return;

        editor.setOpaque(false);
        editor.setInheritsPopupMenu(true);

        if (editor.getFont() instanceof UIResource) {
            editor.setFont(spinner.getFont());
        }

        JFormattedTextField editorTextField = ((DefaultEditor)editor).getTextField();
        if (editorTextField.getFont() instanceof UIResource) {
            editorTextField.setFont(spinner.getFont());
        }
        InputMap spinnerInputMap = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        InputMap editorInputMap = editorTextField.getInputMap();
        KeyStroke[] keys = spinnerInputMap.keys();
        for (KeyStroke k : keys) {
            editorInputMap.put(k, spinnerInputMap.get(k));
        }
    }

    void updateEnabledState() {
        updateEnabledState(spinner, spinner.isEnabled());
    }

    private void updateEnabledState(Container c, boolean enabled) {
        for (int counter = c.getComponentCount() - 1; counter >= 0; counter--) {
            Component child = c.getComponent(counter);

            child.setEnabled(enabled);
            if (child instanceof Container) {
                updateEnabledState((Container)child, enabled);
            }
        }
    }

    private void installKeyboardActions() {
        InputMap iMap = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        SwingUtilities.replaceUIInputMap(spinner, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, iMap);
        SwingUtilities.replaceUIActionMap(spinner, getActionMap());
    }

    private InputMap getInputMap(int condition) {
        if (condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) {
            return (InputMap)UIManager.get("Spinner.ancestorInputMap");
        }
        return null;
    }

    private ActionMap getActionMap() {
        ActionMap map = (ActionMap)UIManager.get("Spinner.actionMap");

        if (map == null) {
            map = createActionMap();
            if (map != null) {
                UIManager.getLookAndFeelDefaults().put("Spinner.actionMap", map);
            }
        }
        return map;
    }

    private ActionMap createActionMap() {
        ActionMap map = new ActionMapUIResource();
        map.put("increment", getNextButtonHandler());
        map.put("decrement", getPreviousButtonHandler());
        return map;
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    private static class ArrowButtonHandler extends AbstractAction implements MouseListener {
        private final javax.swing.Timer autoRepeatTimer;
        private final boolean isNext;
        private JSpinner activeSpinner = null;

        ArrowButtonHandler(String name, boolean isNext) {
            super(name);
            this.isNext = isNext;
            autoRepeatTimer = new javax.swing.Timer(60, this);
            autoRepeatTimer.setInitialDelay(300);
        }

        private JSpinner eventToSpinner(AWTEvent e) {
            Object src = e.getSource();
            while ((src instanceof Component) && !(src instanceof JSpinner)) {
                src = ((Component)src).getParent();
            }
            return (src instanceof JSpinner) ? (JSpinner)src : null;
        }

        public void actionPerformed(ActionEvent e) {
            if (!(e.getSource() instanceof javax.swing.Timer)) {
                // Most likely resulting from being in ActionMap.
                activeSpinner = eventToSpinner(e);
            }

            if (activeSpinner != null) {
                try {
                    int calendarField = getCalendarField(activeSpinner);
                    activeSpinner.commitEdit();
                    if (calendarField != -1) {
                        ((SpinnerDateModel) activeSpinner.getModel()).setCalendarField(calendarField);
                    }
                    Object value = (isNext) ? activeSpinner.getNextValue() : activeSpinner.getPreviousValue();
                    if (value != null) {
                        activeSpinner.setValue(value);
                        select(activeSpinner);
                    }
                } catch (IllegalArgumentException | ParseException iae) {
                    UIManager.getLookAndFeel().provideErrorFeedback(activeSpinner);
                }
            }
        }

        /**
         * If the spinner's editor is a DateEditor, this selects the field
         * associated with the value that is being incremented.
         */
        private void select(JSpinner spinnerComponent) {
            JComponent editor = spinnerComponent.getEditor();
            if (!(editor instanceof JSpinner.DateEditor)) return;

            JSpinner.DateEditor dateEditor = (JSpinner.DateEditor)editor;
            JFormattedTextField ftf = dateEditor.getTextField();
            Format format = dateEditor.getFormat();
            Object value;
            if (format == null || (value = spinnerComponent.getValue()) == null) return;

            SpinnerDateModel model = dateEditor.getModel();
            DateFormat.Field field = DateFormat.Field.ofCalendarField(model.getCalendarField());
            if (field == null) return;

            try {
                AttributedCharacterIterator iterator = format.formatToCharacterIterator(value);
                if (!select(ftf, iterator, field) && field == DateFormat.Field.HOUR0) {
                    select(ftf, iterator, DateFormat.Field.HOUR1);
                }
            } catch (IllegalArgumentException iae) {}
        }

        /**
         * Selects the passed in field, returning true if it is found,
         * false otherwise.
         */
        private boolean select(JFormattedTextField ftf, AttributedCharacterIterator iterator, DateFormat.Field field) {
            int max = ftf.getDocument().getLength();

            iterator.first();
            do {
                Map<Attribute,Object> attrs = iterator.getAttributes();
                if (attrs == null || !attrs.containsKey(field)) continue;

                int start = iterator.getRunStart(field);
                int end = iterator.getRunLimit(field);
                if (start != -1 && end != -1 && start <= max && end <= max) {
                    ftf.select(start, end);
                }

                return true;
            } while (iterator.next() != CharacterIterator.DONE);
            return false;
        }

        /**
         * Returns the calendarField under the start of the selection, or
         * -1 if there is no valid calendar field under the selection (or
         * the spinner isn't editing dates.
         */
        private int getCalendarField(JSpinner spinnerComponent) {
            JComponent editor = spinnerComponent.getEditor();
            if (!(editor instanceof JSpinner.DateEditor)) return -1;

            JSpinner.DateEditor dateEditor = (JSpinner.DateEditor)editor;
            JFormattedTextField ftf = dateEditor.getTextField();
            int start = ftf.getSelectionStart();
            JFormattedTextField.AbstractFormatter formatter = ftf.getFormatter();
            if (!(formatter instanceof InternationalFormatter)) return -1;

            Format.Field[] fields = ((InternationalFormatter)formatter).getFields(start);
            for (Field element : fields) {
                if (!(element instanceof DateFormat.Field)) continue;
                int calendarField;

                if (element == DateFormat.Field.HOUR1) {
                    calendarField = Calendar.HOUR;
                } else {
                    calendarField = ((DateFormat.Field)element).getCalendarField();
                }

                if (calendarField != -1) {
                    return calendarField;
                }
            }
            return -1;
        }

        public void mousePressed(MouseEvent e) {
            if (!SwingUtilities.isLeftMouseButton(e) || !e.getComponent().isEnabled()) {
                return;
            }
            activeSpinner = eventToSpinner(e);
            if (activeSpinner != null) {
                activeSpinner.repaint();
                autoRepeatTimer.start();
                focusSpinnerIfNecessary();
            }
        }

        public void mouseReleased(MouseEvent e) {
            autoRepeatTimer.stop();
            if (activeSpinner != null) {
                activeSpinner.repaint();
                activeSpinner = null;
            }
        }

        public void mouseClicked(MouseEvent e) {}
        public void mouseEntered(MouseEvent e) {}
        public void mouseExited(MouseEvent e) {}

        /**
         * Requests focus on a child of the spinner if the spinner doesn't have focus.
         */
        private void focusSpinnerIfNecessary() {
            assert activeSpinner != null;
            Component fo = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
            if (!activeSpinner.isRequestFocusEnabled() || (fo != null && (SwingUtilities.isDescendingFrom(fo, activeSpinner)))) return;
            Container root = activeSpinner;

            if (!root.isFocusCycleRoot()) {
                root = root.getFocusCycleRootAncestor();
            }

            if (root == null) return;
            FocusTraversalPolicy ftp = root.getFocusTraversalPolicy();
            Component child = ftp.getComponentAfter(root, activeSpinner);

            if (child != null && SwingUtilities.isDescendingFrom(child, activeSpinner)) {
                child.requestFocus();
            }
        }
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    class SpinPainter extends JComponent {

        ButtonModel fTopModel;
        ButtonModel fBottomModel;

        public SpinPainter(AbstractButton top, AbstractButton bottom) {
            if (top != null) {
                fTopModel = top.getModel();
            }

            if (bottom != null) {
                fBottomModel = bottom.getModel();
            }

            setUI(new SpinnerPainterUI());
        }

        // This method is invoked by reflection
        public ComponentUI getUI() {
            return ui;
        }

        public void paint(Graphics g) {
            if (spinner.isOpaque()) {
                g.setColor(spinner.getBackground());
                g.fillRect(0, 0, getWidth(), getHeight());
            }

            Rectangle bounds = getBounds();
            AppearanceManager.ensureAppearance(spinner);
            AquaUtils.configure(painter, spinner, bounds.width, bounds.height);
            Configuration cg = getConfiguration();
            painter.getPainter(cg).paint(g, 0, 0);
        }

        protected Configuration getConfiguration() {
            Size size = AquaUtilControlSize.getUserSizeFrom(spinner);
            State state;
            boolean isPressedTop = false;

            if (isEnabled()) {
                if (fTopModel != null && fTopModel.isPressed()) {
                    state = State.PRESSED;
                    isPressedTop = true;
                } else if (fBottomModel != null && fBottomModel.isPressed()) {
                    state = State.PRESSED;
                } else {
                    state = State.ACTIVE;
                }
            } else {
                state = State.DISABLED;
            }

            boolean isFocused = false;
            return new SpinnerArrowsConfiguration(size, state, isFocused, isPressedTop);
        }

        public Dimension getPreferredSize() {
            Size size = AquaUtilControlSize.getUserSizeFrom(spinner);
            SpinnerArrowsLayoutConfiguration g = new SpinnerArrowsLayoutConfiguration(size);
            LayoutInfo layoutInfo = painter.getLayoutInfo().getLayoutInfo(g);
            int width = (int) layoutInfo.getFixedVisualWidth();
            int height = (int) layoutInfo.getFixedVisualHeight();
            if (width == 0) {
                width = 14;
            }
            if (height == 0) {
                height = 23;
            }

            return new Dimension(width, height);
        }
    }

    class SpinnerPainterUI extends ComponentUI implements FocusRingOutlineProvider {
        @Override
        public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
            Size size = AquaUtilControlSize.getUserSizeFrom(spinner);
            SpinnerArrowsLayoutConfiguration g = new SpinnerArrowsLayoutConfiguration(size);
            Rectangle bounds = c.getBounds();
            AppearanceManager.ensureAppearance(c);
            AquaUtils.configure(painter, c, bounds.width, bounds.height);
            return painter.getOutline(g);
        }

        @Override
        public void installUI(JComponent c) {
            super.installUI(c);

            // On Yosemite, the spinner control is a single control containing two buttons. The spinner control is
            // focusable in Full Keyboard Access mode. The individual buttons are not focusable.

            AquaFullKeyboardFocusableHandler.addListener(c);
        }

        @Override
        public void uninstallUI(JComponent c) {
            AquaFullKeyboardFocusableHandler.removeListener(c);
            super.uninstallUI(c);
        }
    }

    /**
     * A simple layout manager for the editor and the next/previous buttons.
     * See the AquaSpinnerUI javadoc for more information about exactly
     * how the components are arranged.
     */
    static class SpinnerLayout implements LayoutManager {
        private Component nextButton = null;
        private Component previousButton = null;
        private Component editor = null;
        private Component painter = null;

        public void addLayoutComponent(String name, Component c) {
            if ("Next".equals(name)) {
                nextButton = c;
            } else if ("Previous".equals(name)) {
                previousButton = c;
            } else if ("Editor".equals(name)) {
                editor = c;
            } else if ("Painter".equals(name)) {
                painter = c;
            }
        }

        public void removeLayoutComponent(Component c) {
            if (c == nextButton) {
                c = null;
            } else if (c == previousButton) {
                previousButton = null;
            } else if (c == editor) {
                editor = null;
            } else if (c == painter) {
                painter = null;
            }
        }

        private Dimension preferredSize(Component c) {
            return (c == null) ? new Dimension(0, 0) : c.getPreferredSize();
        }

        public Dimension preferredLayoutSize(Container parent) {
//            Dimension nextD = preferredSize(nextButton);
//            Dimension previousD = preferredSize(previousButton);
            Dimension editorD = preferredSize(editor);
            Dimension painterD = preferredSize(painter);

            /* Force the editors height to be a multiple of 2
             */
            editorD.height = ((editorD.height + 1) / 2) * 2;

            Dimension size = new Dimension(editorD.width, Math.max(painterD.height, editorD.height));
            size.width += painterD.width; //Math.max(nextD.width, previousD.width);
            Insets insets = parent.getInsets();
            size.width += insets.left + insets.right + SEPARATION;
            size.height += insets.top + insets.bottom;
            return size;
        }

        public Dimension minimumLayoutSize(Container parent) {
            return preferredLayoutSize(parent);
        }

        private void setBounds(Component c, int x, int y, int width, int height) {
            if (c != null) {
                c.setBounds(x, y, width, height);
            }
        }

        public void layoutContainer(Container parent) {
            Insets insets = parent.getInsets();
            int availWidth = parent.getWidth() - (insets.left + insets.right);
            int availHeight = parent.getHeight() - (insets.top + insets.bottom);

            Dimension painterD = preferredSize(painter);
//            Dimension nextD = preferredSize(nextButton);
//            Dimension previousD = preferredSize(previousButton);
            int nextHeight = availHeight / 2;
            int previousHeight = availHeight - nextHeight;
            int buttonsWidth = painterD.width; //Math.max(nextD.width, previousD.width);
            int editorWidth = availWidth - buttonsWidth - SEPARATION;

            /* Deal with the spinners componentOrientation property.
             */
            int editorX, buttonsX;
            if (parent.getComponentOrientation().isLeftToRight()) {
                editorX = insets.left;
                buttonsX = editorX + editorWidth + SEPARATION;
            } else {
                buttonsX = insets.left;
                editorX = buttonsX + buttonsWidth + SEPARATION;
            }

            int previousY = insets.top + nextHeight;
            int painterTop = previousY - (painterD.height / 2);
            setBounds(editor, editorX, insets.top, editorWidth, availHeight);
            setBounds(nextButton, buttonsX, insets.top, buttonsWidth, nextHeight);
            setBounds(previousButton, buttonsX, previousY, buttonsWidth, previousHeight);
            setBounds(painter, buttonsX, painterTop, buttonsWidth, painterD.height);
        }
    }

    /**
     * Detect JSpinner property changes we're interested in and delegate.  Subclasses
     * shouldn't need to replace the default propertyChangeListener (although they
     * can by overriding createPropertyChangeListener) since all of the interesting
     * property changes are delegated to protected methods.
     */
    static class PropertyChangeHandler implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent e) {
            String propertyName = e.getPropertyName();
            JSpinner spinner = (JSpinner)(e.getSource());
            SpinnerUI spinnerUI = spinner.getUI();

            if (spinnerUI instanceof AquaSpinnerUI) {
                AquaSpinnerUI ui = (AquaSpinnerUI)spinnerUI;

                if ("editor".equals(propertyName)) {
                    JComponent oldEditor = (JComponent)e.getOldValue();
                    JComponent newEditor = (JComponent)e.getNewValue();
                    ui.replaceEditor(oldEditor, newEditor);
                    ui.updateEnabledState();
                } else if ("enabled".equals(propertyName)) {
                    ui.updateEnabledState();
                } else if (JComponent.TOOL_TIP_TEXT_KEY.equals(propertyName)) {
                    ui.updateToolTipTextForChildren(spinner);
                } else if ("font".equals(propertyName)) {
                    JComponent editor = spinner.getEditor();
                    if (editor != null && editor instanceof JSpinner.DefaultEditor) {
                        JTextField tf =
                                ((JSpinner.DefaultEditor) editor).getTextField();
                        if (tf != null) {
                            if (tf.getFont() instanceof UIResource) {
                                tf.setFont(spinner.getFont());
                            }
                        }
                    }
                } else if (AquaUtilControlSize.CLIENT_PROPERTY_KEY.equals(propertyName)) {
                    JComponent editor = spinner.getEditor();
                    if (editor != null && editor instanceof JSpinner.DefaultEditor) {
                        JTextField tf =
                                ((JSpinner.DefaultEditor) editor).getTextField();
                        if (tf != null) {
                            tf.putClientProperty(propertyName, e.getNewValue());
                        }
                    }
                }
            }
        }
    }

    // Synchronizes the ToolTip text for the components within the spinner
    // to be the same value as the spinner ToolTip text.
    void updateToolTipTextForChildren(JComponent spinnerComponent) {
        String toolTipText = spinnerComponent.getToolTipText();
        Component[] children = spinnerComponent.getComponents();
        for (Component element : children) {
            if (element instanceof JSpinner.DefaultEditor) {
                JTextField tf = ((JSpinner.DefaultEditor)element).getTextField();
                if (tf != null) {
                    tf.setToolTipText(toolTipText);
                }
            } else if (element instanceof JComponent) {
                ((JComponent)element).setToolTipText(toolTipText);
            }
        }
    }
}
