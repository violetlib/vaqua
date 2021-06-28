/*
 * Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.util.StringTokenizer;

import org.jetbrains.annotations.NotNull;

/**
 * Basic utilities that do not involve native code.
 */

public class Utils {

    private static final int javaVersion = obtainJavaVersion();

    private Utils() {
    }

    public static void logError(@NotNull String message) {
        System.err.println(message);
    }

    public static void logError(@NotNull String message, @NotNull Throwable th) {
        System.err.println(message + ": " + th);
    }

    public static void logDebug(@NotNull String message) {
        System.err.println(message);
    }

    public static int getJavaVersion() {
        return javaVersion;
    }

    private static int obtainJavaVersion()
    {
        String s = System.getProperty("java.version");
        if (s.startsWith("1.")) {
            s = s.substring(2);
        }
        int version = 0;
        int tokenCount = 0;
        StringTokenizer st = new StringTokenizer(s, "._");
        try {
            while (st.hasMoreTokens()) {
                String token = st.nextToken();
                int pos = token.indexOf("-");
                if (pos > 0) {
                    token = token.substring(0, pos);
                }
                int n = Integer.parseInt(token);
                ++tokenCount;
                int limit = tokenCount < 3 ? 100 : 1000;
                if (n < 0 || n >= limit) {
                    return 0;
                }
                version = version * limit + n;
                if (tokenCount == 3) {
                    return version;
                }
            }
        } catch (NumberFormatException ex) {
            return 0;
        }

        while (tokenCount < 3) {
            ++tokenCount;
            int limit = tokenCount < 3 ? 100 : 1000;
            version = version * limit;
        }
        return version;
    }
}
