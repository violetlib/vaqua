/*
 * Copyright (c) 2018-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

// Support for mapping a native window to a Java window.

#import "JavaWindowAccess.h"

static JNF_CLASS_CACHE(jc_CPlatformWindow, "sun/lwawt/macosx/CPlatformWindow");
static JNF_MEMBER_CACHE(jf_target, jc_CPlatformWindow, "target", "Ljava/awt/Window;");

// Map a native window to its Java window, if possible.
jobject getJavaWindow(JNIEnv *env, NSWindow *w)
{
    NSObject *delegate = [w delegate];

    if ([delegate respondsToSelector: @selector(javaPlatformWindow)]) {
        JNFWeakJObjectWrapper *jPlatformWindowWrapper = [delegate javaPlatformWindow];
        if (jPlatformWindowWrapper) {
            jobject jPlatformWindow = [jPlatformWindowWrapper jObjectWithEnv:env];
            if (jPlatformWindow) {
                return JNFGetObjectField(env, jPlatformWindow, jf_target);
            }
        }
    }

    return NULL;
}

// Map a native window to its Java platform window, if possible.
jobject getJavaPlatformWindow(JNIEnv *env, NSWindow *w)
{
    NSObject *delegate = [w delegate];

    if ([delegate respondsToSelector: @selector(javaPlatformWindow)]) {
        JNFWeakJObjectWrapper *jPlatformWindowWrapper = [delegate javaPlatformWindow];
        if (jPlatformWindowWrapper) {
            return [jPlatformWindowWrapper jObjectWithEnv:env];
        }
    }

    return NULL;
}
