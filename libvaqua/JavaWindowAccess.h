/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

// Support for mapping a native window to a Java window.

#import <Cocoa/Cocoa.h>
#include "jni.h"

jobject getJavaWindow(JNIEnv *env, NSWindow *w);
