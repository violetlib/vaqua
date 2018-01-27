/*
 * @(#)KeyWindowPatch.m
 *
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#include "jni.h"

#import <Cocoa/Cocoa.h>

#import "AquaWrappedWindowDelegate.h"
#import "CMenuItemCategory.h"
#import <JavaNativeFoundation.h>

void ensureWindowDelegateWrapper(NSWindow *w);

/*
 * Class:     org_violetlib_aqua_KeyWindowPatch
 * Method:    nativeIsPatchNeeded
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_KeyWindowPatch_nativeIsPatchNeeded
  (JNIEnv *env, jclass cl, jlong wptr)
{
    jint result = 0;
    
    JNF_COCOA_ENTER(env);

    NSWindow *w = (NSWindow *) wptr;
    id delegate = [w delegate];
    result = ![delegate respondsToSelector: @selector(windowDidResignMain:)];

    JNF_COCOA_EXIT(env);

    return result;
}

/*
 * Class:     org_violetlib_aqua_KeyWindowPatch
 * Method:    nativeEnsureWindowDelegateInstalled
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_KeyWindowPatch_nativeEnsureWindowDelegateInstalled
  (JNIEnv *env, jclass cl, jlong wptr)
{
    JNF_COCOA_ENTER(env);

    void (^block)() = ^(){
        NSWindow *w = (NSWindow *) wptr;
        ensureWindowDelegateWrapper(w);
    };

    if ([NSThread isMainThread]) {
        block();
    } else {
        // Not waiting because there might be a deadlock situation
        [JNFRunLoop performOnMainThreadWaiting:NO withBlock:block];
    }

    JNF_COCOA_EXIT(env);
}

// Ensure that our wrapper window delegate is installed.

void ensureWindowDelegateWrapper(NSWindow *w)
{
	id delegate = [w delegate];
    if (![delegate isKindOfClass: [AquaWrappedWindowDelegate class]]) {
    	NSLog(@"Installing window delegate: %@ %@", [w title], delegate);
		delegate = [[AquaWrappedWindowDelegate alloc] initWithObject: delegate];
		[w setDelegate: delegate];
	}
}
