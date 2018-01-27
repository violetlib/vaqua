/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>

@interface AquaWrappedWindowDelegate : NSProxy <NSWindowDelegate>
  - (instancetype)initWithObject:(id)object;
@end
