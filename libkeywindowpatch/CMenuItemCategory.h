/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>

// The following are minimal definitions to allow our altered methods to access the instance variables it needs.

@interface CMenuComponent : NSObject {
@public
    jobject fPeer;
}
@end

@interface CMenuItem : CMenuComponent
{
    BOOL fIsCheckbox;
}
@end

@interface CMenuItem (CMenuItemCategory)
- (void)handleAction:(NSMenuItem *)sender;
@end

@interface ApplicationDelegate { }
@end
