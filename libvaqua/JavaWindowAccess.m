/*
 * Copyright (c) 2018-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

// Support for mapping a native window to a Java window.

#import "JavaWindowAccess.h"
#import "AquaNativeSupport.h"

static JNF_CLASS_CACHE(jc_CPlatformWindow, "sun/lwawt/macosx/CPlatformWindow");
static JNF_MEMBER_CACHE(jf_target, jc_CPlatformWindow, "target", "Ljava/awt/Window;");

// Map a native window to its Java window, if possible.
jobject getJavaWindow(JNIEnv *env, NSWindow *w)
{
    jobject jPlatformWindow = getJavaPlatformWindow(env, w);
    if (jPlatformWindow) {
        return JNFGetObjectField(env, jPlatformWindow, jf_target);
    }
    return NULL;
}

// Map a native window to its Java platform window, if possible.
jobject getJavaPlatformWindow(JNIEnv *env, NSWindow *w)
{
    NSObject *delegate = [w delegate];

    if ([delegate respondsToSelector: @selector(javaPlatformWindow)]) {
        long javaVersion = getJavaVersion();
        if (javaVersion >= 1700000
            || (javaVersion >= 1500004 && javaVersion < 1600000)
            || (javaVersion >= 1300008 && javaVersion < 1400000)
            || (javaVersion >= 1100012 && javaVersion < 1300000)
           ) {
            return (*env)->NewLocalRef(env, [delegate javaPlatformWindow]);
        } else {
            JNFWeakJObjectWrapper *jPlatformWindowWrapper = [delegate javaPlatformWindow];
            if (jPlatformWindowWrapper) {
                return [jPlatformWindowWrapper jObjectWithEnv:env];
            }
        }
    }

    return NULL;
}
