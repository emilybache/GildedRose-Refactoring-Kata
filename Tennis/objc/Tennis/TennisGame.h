#import <Foundation/Foundation.h>

@interface TennisGame : NSObject

- (id)initWithPlayer1:(NSString *)player1 player2:(NSString *)player2;
- (void)wonPoint:(NSString *)playerName;
- (NSString *)score;

@end