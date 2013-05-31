//
// Created by Stefan on 5/31/13.
// Copyright (c) 2013 Stefan van den Oord. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import "TennisGame3.h"


@interface TennisGame3 ()
@end

@implementation TennisGame3 {
    int p1;
    int p2;
    NSString *p1N;
    NSString *p2N;
}

- (id)initWithPlayer1:(NSString *)p1N player2:(NSString *)p2N {
    self = [super initWithPlayer1:p1N player2:p2N];
    if (self) {
        self->p1N = p1N;
        self->p2N = p2N;
        p1 = 0;
        p2 = 0;
    }
    return self;
}

- (NSString *)score {
    NSString *s;
    if (p1 < 4 && p2 < 4) {
        NSArray* p = @[@"Love", @"Fifteen", @"Thirty", @"Forty"];
        s = p[p1];
        return (p1 == p2) ? [NSString stringWithFormat:@"%@-All",s] : [NSString stringWithFormat:@"%@-%@",s,p[p2]];
    } else {
        if (p1 == p2)
            return @"Deuce";
        s = p1 > p2 ? p1N : p2N;
        return ((p1-p2)*(p1-p2) == 1) ? [NSString stringWithFormat:@"Advantage %@",s] : [NSString stringWithFormat:@"Win for %@",s];
    }
}

- (void)wonPoint:(NSString *)playerName {
    if ([playerName isEqualToString:@"player1"])
        p1 += 1;
    else
        p2 += 1;
}


@end