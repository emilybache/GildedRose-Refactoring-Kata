#include <gtest/gtest.h>

const std::string tennis_score(int p1Score, int p2Score) {
	return "foo";
}

TEST(TennisTest, LoveAll_0_0) {
  EXPECT_EQ("Love-All", tennis_score(0, 0));
}


TEST(TennisTest, FifteenAll_1_1) {
  EXPECT_EQ("Fifteen-All", tennis_score(1, 1));
}


TEST(TennisTest, ThirtyAll_2_2) {
  EXPECT_EQ("Thirty-All", tennis_score(2, 2));
}


TEST(TennisTest, FortyAll_3_3) {
  EXPECT_EQ("Forty-All", tennis_score(3, 3));
}


TEST(TennisTest, Deuce_4_4) {
  EXPECT_EQ("Deuce", tennis_score(4, 4));
}


TEST(TennisTest, FifteenLove_1_0) {
  EXPECT_EQ("Fifteen-Love", tennis_score(1, 0));
}


TEST(TennisTest, LoveFifteen_0_1) {
  EXPECT_EQ("Love-Fifteen", tennis_score(0, 1));
}


TEST(TennisTest, ThirtyLove_2_0) {
  EXPECT_EQ("Thirty-Love", tennis_score(2, 0));
}


TEST(TennisTest, LoveThirty_0_2) {
  EXPECT_EQ("Love-Thirty", tennis_score(0, 2));
}


TEST(TennisTest, FortyLove_3_0) {
  EXPECT_EQ("Forty-Love", tennis_score(3, 0));
}


TEST(TennisTest, LoveForty_0_3) {
  EXPECT_EQ("Love-Forty", tennis_score(0, 3));
}


TEST(TennisTest, Winforplayer1_4_0) {
  EXPECT_EQ("Win for player1", tennis_score(4, 0));
}


TEST(TennisTest, Winforplayer2_0_4) {
  EXPECT_EQ("Win for player2", tennis_score(0, 4));
}


TEST(TennisTest, ThirtyFifteen_2_1) {
  EXPECT_EQ("Thirty-Fifteen", tennis_score(2, 1));
}


TEST(TennisTest, FifteenThirty_1_2) {
  EXPECT_EQ("Fifteen-Thirty", tennis_score(1, 2));
}


TEST(TennisTest, FortyFifteen_3_1) {
  EXPECT_EQ("Forty-Fifteen", tennis_score(3, 1));
}


TEST(TennisTest, FifteenForty_1_3) {
  EXPECT_EQ("Fifteen-Forty", tennis_score(1, 3));
}


TEST(TennisTest, Winforplayer1_4_1) {
  EXPECT_EQ("Win for player1", tennis_score(4, 1));
}


TEST(TennisTest, Winforplayer2_1_4) {
  EXPECT_EQ("Win for player2", tennis_score(1, 4));
}


TEST(TennisTest, FortyThirty_3_2) {
  EXPECT_EQ("Forty-Thirty", tennis_score(3, 2));
}


TEST(TennisTest, ThirtyForty_2_3) {
  EXPECT_EQ("Thirty-Forty", tennis_score(2, 3));
}


TEST(TennisTest, Winforplayer1_4_2) {
  EXPECT_EQ("Win for player1", tennis_score(4, 2));
}


TEST(TennisTest, Winforplayer2_2_4) {
  EXPECT_EQ("Win for player2", tennis_score(2, 4));
}


TEST(TennisTest, Advantageplayer1_4_3) {
  EXPECT_EQ("Advantage player1", tennis_score(4, 3));
}


TEST(TennisTest, Advantageplayer2_3_4) {
  EXPECT_EQ("Advantage player2", tennis_score(3, 4));
}


TEST(TennisTest, Advantageplayer1_5_4) {
  EXPECT_EQ("Advantage player1", tennis_score(5, 4));
}


TEST(TennisTest, Advantageplayer2_4_5) {
  EXPECT_EQ("Advantage player2", tennis_score(4, 5));
}


TEST(TennisTest, Advantageplayer1_15_14) {
  EXPECT_EQ("Advantage player1", tennis_score(15, 14));
}


TEST(TennisTest, Advantageplayer2_14_15) {
  EXPECT_EQ("Advantage player2", tennis_score(14, 15));
}


TEST(TennisTest, Winforplayer1_6_4) {
  EXPECT_EQ("Win for player1", tennis_score(6, 4));
}


TEST(TennisTest, Winforplayer2_4_6) {
  EXPECT_EQ("Win for player2", tennis_score(4, 6));
}


TEST(TennisTest, Winforplayer1_16_14) {
  EXPECT_EQ("Win for player1", tennis_score(16, 14));
}


TEST(TennisTest, Winforplayer2_14_16) {
  EXPECT_EQ("Win for player2", tennis_score(14, 16));
}


