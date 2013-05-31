//
// Created by Stefan on 5/31/13.
// Copyright (c) 2013 Stefan van den Oord. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import "TennisGame3.h"
#import "TennisGame.h"


@implementation TennisGame


- (id)initWithPlayer1:(NSString *)player1 player2:(NSString *)player2 {
    self = [super init];
    if (self) {
        self.player1 = player1;
        self.player2 = player2;
    }

    return self;
}

- (void)wonPoint:(NSString *)player {}
- (NSString *)score { return nil; }
@end