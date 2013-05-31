//
// Created by Stefan on 5/31/13.
// Copyright (c) 2013 Stefan van den Oord. All rights reserved.
//
// To change the template use AppCode | Preferences | File Templates.
//


#import "TennisGame1.h"


@implementation TennisGame1

- (void)wonPoint:(NSString *)playerName {
    if ([playerName isEqualToString:@"player1"])
        score1 += 1;
    else
        score2 += 1;
}

- (NSString *)score {
    NSString *score = @"";
    int tempScore=0;
    if (score1 == score2)
    {
        switch (score1)
        {
            case 0:
                score = @"Love-All";
                break;
            case 1:
                score = @"Fifteen-All";
                break;
            case 2:
                score = @"Thirty-All";
                break;
            case 3:
                score = @"Forty-All";
                break;
            default:
                score = @"Deuce";
                break;

        }
    }
    else if (score1>=4 || score2>=4)
    {
        int minusResult = score1-score2;
        if (minusResult==1) score = @"Advantage player1";
        else if (minusResult ==-1) score = @"Advantage player2";
        else if (minusResult>=2) score = @"Win for player1";
        else score = @"Win for player2";
    }
    else
    {
        for (int i=1; i<3; i++)
        {
            if (i==1) tempScore = score1;
            else { score = [NSString stringWithFormat:@"%@-", score]; tempScore = score2; }
            switch(tempScore)
            {
                case 0:
                    score = [NSString stringWithFormat:@"%@Love", score];
                    break;
                case 1:
                    score = [NSString stringWithFormat:@"%@Fifteen", score];
                    break;
                case 2:
                    score = [NSString stringWithFormat:@"%@Thirty", score];
                    break;
                case 3:
                    score = [NSString stringWithFormat:@"%@Forty", score];
                    break;
            }
        }
    }
    return score;
}


@end