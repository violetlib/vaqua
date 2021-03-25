/*
 * Copyright (c) 2018-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#ifndef __AQUA_NATIVE_SUPPORT__
#define __AQUA_NATIVE_SUPPORT__

void deliverWindowChangedAppearance(NSWindow *window, NSAppearance *appearance);
void runOnMainThread(void (^block)());
long getJavaVersion();

#endif /* __AQUA_NATIVE_SUPPORT__ */
