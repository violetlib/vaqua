/*
 * Copyright (c) 2018 Alan Snyder.
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
import java.awt.geom.AffineTransform;
import java.text.AttributedCharacterIterator.Attribute;
import java.util.Map;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.UIResource;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;

@SuppressWarnings("serial") // JDK implementation class
public class AquaFonts {
    private static final String MAC_DEFAULT_FONT_NAME = getDefaultFontName();

    private static final RecyclableSingleton<FontUIResource> lucida9Pt = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.PLAIN, 9);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida10Pt = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.PLAIN, 10);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida11Pt = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.PLAIN, 11);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida11PtBold = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.BOLD, 11);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida12Pt = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.PLAIN, 12);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida12PtBold = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.BOLD, 12);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida13Pt = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.PLAIN, 13);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida14Pt = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.PLAIN, 14);
        }
    };

    private static final RecyclableSingleton<FontUIResource> lucida13PtBold = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.BOLD, 13);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida14PtBold = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.BOLD, 14);
        }
    };
    private static final RecyclableSingleton<FontUIResource> lucida18Pt = new RecyclableSingleton<FontUIResource>() {
        @Override
        protected FontUIResource getInstance() {
            return new DerivedUIResourceFont(MAC_DEFAULT_FONT_NAME, Font.PLAIN, 18);
        }
    };

    protected static FontUIResource getMiniControlTextFont() {
        return lucida9Pt.get();
    }

    protected static FontUIResource getSmallControlTextFont() {
        return lucida11Pt.get();
    }

    public static FontUIResource getControlTextFont() {
        return lucida13Pt.get();
    }

    public static FontUIResource getControlTextSmallFont() {
        return lucida11Pt.get();
    }

    public static FontUIResource getIconButtonFont() {
        return lucida12Pt.get();
    }

    public static FontUIResource getIconButtonSmallFont() {
        return lucida11Pt.get();
    }

    public static FontUIResource getControlTextMiniFont() {
        return lucida9Pt.get();
    }

    public static FontUIResource getMenuFont() {
        return lucida14Pt.get();
    }

    public static Font getDockIconFont() {
        return lucida14PtBold.get();
    }

    public static FontUIResource getAlertHeaderFont() {
        return lucida13PtBold.get();
    }

    public static FontUIResource getAlertMessageFont() {
        return lucida11Pt.get();
    }

    public static FontUIResource getViewFont() {
        return lucida12Pt.get();
    }

    public static FontUIResource getSideBarFont() {
        return lucida13Pt.get();
    }

    public static FontUIResource getSideBarSelectionFont() {
        return lucida13Pt.get();
    }

    public static FontUIResource getSideBarCategoryFont() {
        return lucida11PtBold.get();
    }

    public static FontUIResource getSideBarCategorySelectionFont() {
        return lucida11PtBold.get();
    }

    public static FontUIResource getPreviewLabelFont() {
        return lucida12PtBold.get();
    }

    public static FontUIResource getPreviewValueFont() {
        return lucida12Pt.get();
    }

    public static FontUIResource getPreviewNameFont() {
        return lucida18Pt.get();
    }

    public static FontUIResource getPreviewTypeSizeFont() {
        return lucida12PtBold.get();
    }

    public static FontUIResource getRecessedButtonFont() {
        return lucida13PtBold.get();
    }

    public static FontUIResource getInlineButtonFont() {
        return lucida11PtBold.get();
    }

    /**
     * All fonts derived from this type will also be of this type, and not a plain java.awt.Font
     */
    static class DerivedUIResourceFont extends FontUIResource implements UIResource {
        public DerivedUIResourceFont(Font font) {
            super(font);
        }

        public DerivedUIResourceFont(String name, int style, int size) {
            super(name, style, size);
        }

        @Override
        public Font deriveFont(AffineTransform trans) {
            return new DerivedUIResourceFont(super.deriveFont(trans));
        }

        @Override
        public Font deriveFont(float derivedSize) {
            return new DerivedUIResourceFont(super.deriveFont(derivedSize));
        }

        @Override
        public Font deriveFont(int derivedStyle) {
            return new DerivedUIResourceFont(super.deriveFont(derivedStyle));
        }

        @Override
        public Font deriveFont(int derivedStyle, AffineTransform trans) {
            return new DerivedUIResourceFont(super.deriveFont(derivedStyle, trans));
        }

        @Override
        public Font deriveFont(int derivedStyle, float derivedSize) {
            return new DerivedUIResourceFont(super.deriveFont(derivedStyle, derivedSize));
        }

        @Override
        public Font deriveFont(Map<? extends Attribute, ?> attributes) {
            return new DerivedUIResourceFont(super.deriveFont(attributes));
        }
    }

    private static String getDefaultFontName() {
        return "Helvetica Neue";
    }

    private static boolean isFontValid(String familyName, int style, String face) {
        Font f = new Font(familyName, style, 13);
        return f.getFontName().equals(face);
    }
}
