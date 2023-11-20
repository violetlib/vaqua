/*
 * @(#)KeyWindowPatch.m
 *
 * Copyright (c) 2018-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>

#include "jnix.h"
#import "KeyWindowPatch.h"
#import "AquaWrappedWindowDelegate.h"
#import "CMenuItemCategory.h"
#import "CMenuBarCategory.h"

void ensureWindowDelegateWrapper(NSWindow *w);

/*
 * Class:     org_violetlib_aqua_KeyWindowPatch
 * Method:    nativeEnsureWindowDelegateInstalled
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_KeyWindowPatch_nativeEnsureWindowDelegateInstalled
  (JNIEnv *env, jclass cl, jlong wptr)
{
    COCOA_ENTER();

    void (^block)() = ^(){
        NSWindow *w = (NSWindow *) wptr;
        ensureWindowDelegateWrapper(w);
    };

    // avoid deadlock if invoked on a thread other than the main thread by not waiting
    APPKIT_EXEC_NOW_OR_LATER(block);

    COCOA_EXIT();
}

// Ensure that our wrapper window delegate is installed.

void ensureWindowDelegateWrapper(NSWindow *w)
{
    id delegate = [w delegate];
    if (![delegate isKindOfClass: [AquaWrappedWindowDelegate class]]) {
#ifdef DEBUG_PATCH
        NSLog(@"Installing window delegate: %@ %@", [w title], delegate);
#endif
        delegate = [[AquaWrappedWindowDelegate alloc] initWithObject: delegate];
        [w setDelegate: delegate];
    }
}
