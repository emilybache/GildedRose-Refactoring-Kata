//
//  TennisTests.m
//  TennisTests
//
//  Created by Stefan on 05/31/13.
//  Copyright (c) 2013 Stefan van den Oord. All rights reserved.
//

#import <SenTestingKit/SenTestingKit.h>
#import "TennisTests.h"
#import "TennisGame1.h"
#import "TennisGame2.h"
#import "TennisGame3.h"

@interface TennisTests()
+ (NSArray*)parameters;
@end

@implementation TennisTests (Parametrized)
+ (id)defaultTestSuite {
    SenTestSuite *testSuite = [[SenTestSuite alloc] initWithName:NSStringFromClass(self)];

    NSArray *allScores = [self parameters];
    for (NSArray *scores in allScores) {
        [self addTestWithScores:scores toTestSuite:testSuite];
    }

    return testSuite;
}
+ (void)addTestWithScores:(NSArray *)scores toTestSuite:(SenTestSuite *)testSuite {
    NSArray *testInvocations = [self testInvocations];
    for (NSInvocation *testInvocation in testInvocations) {

        // Create a new instance of our test case for each method found using the given set of parameters.
        SenTestCase *test = [[TennisTests alloc] initWithInvocation:testInvocation
                                                             scores:scores];

        // Add the new test instance to the suite. The OCUnit framework eventually executes the entire test suite.
        [testSuite addTest:test];
    }
}
@end

@implementation TennisTests {
    int player1Score;
    int player2Score;
    NSString *expectedScore;
}

+ (NSArray*)parameters {
    return @[
            @[ @0, @0, @"Love-All"],
            @[ @1, @1, @"Fifteen-All"],
            @[ @2, @2, @"Thirty-All"],
            @[ @3, @3, @"Forty-All"],
            @[ @4, @4, @"Deuce"],

            @[ @1, @0, @"Fifteen-Love"],
            @[ @0, @1, @"Love-Fifteen"],
            @[ @2, @0, @"Thirty-Love"],
            @[ @0, @2, @"Love-Thirty"],
            @[ @3, @0, @"Forty-Love"],
            @[ @0, @3, @"Love-Forty"],
            @[ @4, @0, @"Win for player1"],
            @[ @0, @4, @"Win for player2"],

            @[ @2, @1, @"Thirty-Fifteen"],
            @[ @1, @2, @"Fifteen-Thirty"],
            @[ @3, @1, @"Forty-Fifteen"],
            @[ @1, @3, @"Fifteen-Forty"],
            @[ @4, @1, @"Win for player1"],
            @[ @1, @4, @"Win for player2"],

            @[ @3, @2, @"Forty-Thirty"],
            @[ @2, @3, @"Thirty-Forty"],
            @[ @4, @2, @"Win for player1"],
            @[ @2, @4, @"Win for player2"],

            @[ @4, @3, @"Advantage player1"],
            @[ @3, @4, @"Advantage player2"],
            @[ @5, @4, @"Advantage player1"],
            @[ @4, @5, @"Advantage player2"],
            @[ @15, @14, @"Advantage player1"],
            @[ @14, @15, @"Advantage player2"],

            @[ @6, @4, @"Win for player1"],
            @[ @4, @6, @"Win for player2"],
            @[ @16, @14, @"Win for player1"],
            @[ @14, @16, @"Win for player2"]
    ];
}

- (id)initWithInvocation:(NSInvocation *)invocation scores:(NSArray *)scores {
    self = [super initWithInvocation:invocation];
    if (self) {
        player1Score = [scores[0] intValue];
        player2Score = [scores[1] intValue];
        expectedScore = scores[2];
    }
    return self;
}

- (NSString *)name {
    return [[super name] stringByReplacingOccurrencesOfString:@"]" withString:[NSString stringWithFormat:@" (%d,%d,%@)]", player1Score, player2Score, expectedScore]];
}

- (void)checkAllScoresForGame:(TennisGame *)game {
    int highestScore = MAX(player1Score, player2Score);
    for (int i = 0; i < highestScore; i++) {
        if (i < player1Score)
            [game wonPoint:@"player1"];
        if (i < player2Score)
            [game wonPoint:@"player2"];
    }
    STAssertEqualObjects([game score], expectedScore, @"");
}

- (void)testAllScoresTennisGame1 {
    TennisGame1 * game = [[TennisGame1 alloc] initWithPlayer1:@"player1" player2:@"player2"];
    [self checkAllScoresForGame:game];
}

- (void)testAllScoresTennisGame2 {
    TennisGame2 * game = [[TennisGame2 alloc] initWithPlayer1:@"player1" player2:@"player2"];
    [self checkAllScoresForGame:game];
}

- (void)testAllScoresTennisGame3 {
    TennisGame3 * game = [[TennisGame3 alloc] initWithPlayer1:@"player1" player2:@"player2"];
    [self checkAllScoresForGame:game];
}

@end
