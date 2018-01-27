/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>
#import <JavaNativeFoundation/JavaNativeFoundation.h>

@interface CMenu : NSObject { }
@end

@interface CMenuBar : NSObject {
@public
    NSMutableArray *fMenuList;
    BOOL fModallyDisabled;
    CMenu *fHelpMenu;
}
@end

@interface CMenuBar (CMenuBarCategory)
+ (BOOL) isActiveMenuBar:(CMenuBar *)inMenuBar;
+ (void) activate:(CMenuBar *)menubar modallyDisabled:(BOOL)modallyDisabled;
- (void) deactivate;
- (void) javaAddMenu: (CMenu *)theMenu;
- (void) javaAddMenu: (CMenu *)theMenu atIndex:(jint)index;
- (void) javaDeleteMenu: (jint)index;
@end
