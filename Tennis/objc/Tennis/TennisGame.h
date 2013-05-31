//
// Created by Stefan on 5/31/13.
// Copyright (c) 2013 Stefan van den Oord. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import <Foundation/Foundation.h>


@interface TennisGame : NSObject {
    int score1;
    int score2;
}

@property(nonatomic, copy) NSString *player1;
@property(nonatomic, copy) NSString *player2;

- (id)initWithPlayer1:(NSString *)player1 player2:(NSString *)player2;
- (void)wonPoint:(NSString *)playerName;
- (NSString *)score;

@end