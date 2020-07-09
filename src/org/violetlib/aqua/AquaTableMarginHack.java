package org.violetlib.aqua;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import javax.swing.*;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Support access via reflection to an extended JTable that supports side margins.
 * The extended JTable must support:
 * <ul>
 *     <li>
 *         A {@code getSideMargins} method that takes no arguments and returns the margins (not null).
 *     </li>
 *     <li>
 *         A {@code setSideMargins} method that takes one argument whose type matches the return type of {@code getSideMargins}.
 *     </li>
 *     <li>
 *         A nested class representing default margins that conforms to the return type of {@code getSideMargins} and to
 *         {@link javax.swing.plaf.UIResource} and has a constructor that takes two integer parameters representing the
 *         leading margin width and the trailing margin width.
 *     </li>
 * </ul>
 */
public class AquaTableMarginHack {

    public static class TableMargins {
        public final int leadingMargin;
        public final int trailingMargin;

        public TableMargins(int leadingMargin, int trailingMargin) {
            this.leadingMargin = leadingMargin;
            this.trailingMargin = trailingMargin;
        }
    }

    public interface ExtendedTable {
        /**
         * If the current side margins are a UI default, replace them with the specified margins as a UI default.
         * @param leadingMargin The leading margin width.
         * @param trailingMargin The trailing margin width.
         */
        void installDefaultMargins(int leadingMargin, int trailingMargin);
    }

    public static @Nullable ExtendedTable getExtendedTable(@NotNull JTable table) {
        Class<?> c = table.getClass();
        String className = c.getName();
        if (className.startsWith("javax.")) {
            return null;
        }
        Method getMarginsMethod = null;
        Method setMarginsMethod = null;
        Class<?> marginsResultClass = null;
        Class<?> marginsParameterClass = null;

        {
            Method[] methods = c.getMethods();
            for (Method m : methods) {
                String name = m.getName();
                if (name.equals("getSideMargins") && m.getParameterCount() == 0) {
                    getMarginsMethod = m;
                    marginsResultClass = m.getReturnType();
                } else if (name.equals("setSideMargins") && m.getParameterCount() == 1) {
                    marginsParameterClass = m.getParameterTypes()[0];
                    setMarginsMethod = m;
                }
                if (getMarginsMethod != null && setMarginsMethod != null) {
                    break;
                }
            }
        }

        if (getMarginsMethod == null || setMarginsMethod == null || marginsResultClass != marginsParameterClass) {
            return null;
        }

        Class<?> marginsUIResourceClass = null;
        {
            Class<?>[] classes = c.getClasses();
            for (Class<?> mc : classes) {
                if (UIResource.class.isAssignableFrom(mc) && marginsResultClass.isAssignableFrom(mc)) {
                    marginsUIResourceClass = mc;
                    break;
                }
            }
        }

        if (marginsUIResourceClass == null) {
            return null;
        }

        Field leadingMarginField = null;
        Field trailingMarginField = null;
        Constructor<?> marginConstructor = null;

        {
            Field[] fields = marginsResultClass.getFields();
            for (Field f : fields) {
                String name = f.getName();
                if (name.equals("leadingMargin") && f.getType() == Integer.TYPE) {
                    leadingMarginField = f;
                } else if (name.equals("trailingMargin") && f.getType() == Integer.TYPE) {
                    trailingMarginField = f;
                }
                if (leadingMarginField != null && trailingMarginField != null) {
                    break;
                }
            }
        }

//        if (leadingMarginField == null || trailingMarginField == null) {
//            return null;
//        }

        try {
            marginConstructor = marginsUIResourceClass.getConstructor(Integer.TYPE, Integer.TYPE);
        } catch (NoSuchMethodException e) {
            return null;
        }

        return new MyExtendedTable(table, getMarginsMethod, setMarginsMethod, marginConstructor);
    }

    private static class MyExtendedTable
            implements ExtendedTable
    {
        private final @NotNull JTable table;
        private final @NotNull Method getMarginsMethod;
        private final @NotNull Method setMarginsMethod;
        private final @NotNull Constructor<?> marginsConstructor;

        public MyExtendedTable(@NotNull JTable table,
                               @NotNull Method getMarginsMethod,
                               @NotNull Method setMarginsMethod,
                               @NotNull Constructor<?> marginsConstructor) {
            this.table = table;
            this.getMarginsMethod = getMarginsMethod;
            this.setMarginsMethod = setMarginsMethod;
            this.marginsConstructor = marginsConstructor;
        }

        @Override
        public void installDefaultMargins(int leadingMargin, int trailingMargin) {
            Object margins;

            try {
                margins = getMarginsMethod.invoke(table);
            } catch (Exception ex) {
                AquaUtils.logError("Unable to get SideMargins", ex);
                return;
            }

            if (margins instanceof UIResource) {
                try {
                    margins = marginsConstructor.newInstance(leadingMargin, trailingMargin);
                } catch (Exception ex) {
                    AquaUtils.logError("Unable to create SideMargins instance", ex);
                    return;
                }
                try {
                    setMarginsMethod.invoke(table, margins);
                } catch (Exception ex) {
                    AquaUtils.logError("Unable to install SideMargins", ex);
                }
            }
        }
    }
}
