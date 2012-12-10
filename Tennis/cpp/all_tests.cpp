#include "all_tests.hpp"

/* change this to the version of tennis you want to work on */
#include "tennis1.cc"

#include <cassert>

void test_LoveAll_0_0()
{
    assert("Love-All" == tennis_score(0, 0));
}

void test_FifteenAll_1_1()
{
    assert("Fifteen-All" == tennis_score(1, 1));
}

void test_ThirtyAll_2_2()
{
    assert("Thirty-All" == tennis_score(2, 2));
}

void test_Deuce_3_3()
{
    assert("Deuce" == tennis_score(3, 3));
}

void test_Deuce_4_4()
{
    assert("Deuce" == tennis_score(4, 4));
}

void test_FifteenLove_1_0()
{
    assert("Fifteen-Love" == tennis_score(1, 0));
}

void test_LoveFifteen_0_1()
{
    assert("Love-Fifteen" == tennis_score(0, 1));
}

void test_ThirtyLove_2_0()
{
    assert("Thirty-Love" == tennis_score(2, 0));
}

void test_LoveThirty_0_2()
{
    assert("Love-Thirty" == tennis_score(0, 2));
}

void test_FortyLove_3_0()
{
    assert("Forty-Love" == tennis_score(3, 0));
}

void test_LoveForty_0_3()
{
    assert("Love-Forty" == tennis_score(0, 3));
}

void test_Winforplayer1_4_0()
{
    assert("Win for player1" == tennis_score(4, 0));
}

void test_Winforplayer2_0_4()
{
    assert("Win for player2" == tennis_score(0, 4));
}

void test_ThirtyFifteen_2_1()
{
    assert("Thirty-Fifteen" == tennis_score(2, 1));
}

void test_FifteenThirty_1_2()
{
    assert("Fifteen-Thirty" == tennis_score(1, 2));
}

void test_FortyFifteen_3_1()
{
    assert("Forty-Fifteen" == tennis_score(3, 1));
}

void test_FifteenForty_1_3()
{
    assert("Fifteen-Forty" == tennis_score(1, 3));
}

void test_Winforplayer1_4_1()
{
    assert("Win for player1" == tennis_score(4, 1));
}

void test_Winforplayer2_1_4()
{
    assert("Win for player2" == tennis_score(1, 4));
}

void test_FortyThirty_3_2()
{
    assert("Forty-Thirty" == tennis_score(3, 2));
}

void test_ThirtyForty_2_3()
{
    assert("Thirty-Forty" == tennis_score(2, 3));
}

void test_Winforplayer1_4_2()
{
    assert("Win for player1" == tennis_score(4, 2));
}

void test_Winforplayer2_2_4()
{
    assert("Win for player2" == tennis_score(2, 4));
}

void test_Advantageplayer1_4_3()
{
    assert("Advantage player1" == tennis_score(4, 3));
}

void test_Advantageplayer2_3_4()
{
    assert("Advantage player2" == tennis_score(3, 4));
}

void test_Advantageplayer1_5_4()
{
    assert("Advantage player1" == tennis_score(5, 4));
}

void test_Advantageplayer2_4_5()
{
    assert("Advantage player2" == tennis_score(4, 5));
}

void test_Advantageplayer1_15_14()
{
    assert("Advantage player1" == tennis_score(15, 14));
}

void test_Advantageplayer2_14_15()
{
    assert("Advantage player2" == tennis_score(14, 15));
}

void test_Winforplayer1_6_4()
{
    assert("Win for player1" == tennis_score(6, 4));
}

void test_Winforplayer2_4_6()
{
    assert("Win for player2" == tennis_score(4, 6));
}

void test_Winforplayer1_16_14()
{
    assert("Win for player1" == tennis_score(16, 14));
}

void test_Winforplayer2_14_16()
{
    assert("Win for player2" == tennis_score(14, 16));
}