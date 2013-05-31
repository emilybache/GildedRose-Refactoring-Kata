//
// Created by Stefan on 5/31/13.
// Copyright (c) 2013 Stefan van den Oord. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import "TennisGame2.h"


@implementation TennisGame2 {
    int P1point;
    NSString *P1res;
    int P2point;
    NSString *P2res;
}

- (NSString *)score {
    NSString *score = @"";
    if (P1point == P2point && P1point < 4)
    {
        if (P1point==0)
            score = @"Love";
        if (P1point==1)
            score = @"Fifteen";
        if (P1point==2)
            score = @"Thirty";
        if (P1point==3)
            score = @"Forty";
        score = [NSString stringWithFormat:@"%@-All", score];
    }
    if (P1point==P2point && P1point>3)
        score = @"Deuce";

    if (P1point > 0 && P2point==0)
    {
        if (P1point==1)
            P1res = @"Fifteen";
        if (P1point==2)
            P1res = @"Thirty";
        if (P1point==3)
            P1res = @"Forty";

        P2res = @"Love";
        score = [NSString stringWithFormat:@"%@-%@", P1res, P2res];
    }
    if (P2point > 0 && P1point==0)
    {
        if (P2point==1)
            P2res = @"Fifteen";
        if (P2point==2)
            P2res = @"Thirty";
        if (P2point==3)
            P2res = @"Forty";

        P1res = @"Love";
        score = [NSString stringWithFormat:@"%@-%@", P1res, P2res];
    }

    if (P1point>P2point && P1point < 4)
    {
        if (P1point==2)
            P1res=@"Thirty";
        if (P1point==3)
            P1res=@"Forty";
        if (P2point==1)
            P2res=@"Fifteen";
        if (P2point==2)
            P2res=@"Thirty";
        score = [NSString stringWithFormat:@"%@-%@", P1res, P2res];
    }
    if (P2point>P1point && P2point < 4)
    {
        if (P2point==2)
            P2res=@"Thirty";
        if (P2point==3)
            P2res=@"Forty";
        if (P1point==1)
            P1res=@"Fifteen";
        if (P1point==2)
            P1res=@"Thirty";
        score = [NSString stringWithFormat:@"%@-%@", P1res, P2res];
    }

    if (P1point > P2point && P2point >= 3)
    {
        score = @"Advantage player1";
    }

    if (P2point > P1point && P1point >= 3)
    {
        score = @"Advantage player2";
    }

    if (P1point>=4 && P2point>=0 && (P1point-P2point)>=2)
    {
        score = @"Win for player1";
    }
    if (P2point>=4 && P1point>=0 && (P2point-P1point)>=2)
    {
        score = @"Win for player2";
    }
    return score;
}

-(void)setP1Score:(int)number {

    for (int i = 0; i < number; i++)
    {
        [self P1Score];
    }

}

- (void)setP2Score:(int)number {

    for (int i = 0; i < number; i++)
    {
        [self P2Score];
    }

}

- (void)P1Score {
    P1point++;
}

- (void)P2Score {
    P2point++;
}

- (void)wonPoint:(NSString *)playerName {
    if ([playerName isEqualToString:@"player1"])
        [self P1Score];
    else
        [self P2Score];
}

@end