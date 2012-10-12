#include "yahtzee.hpp"
#include <cassert>
#include <iostream>

static void Chance_scores_sum_of_all_dice(void)
{
    int expected = 15;
    int actual = Yahtzee().Chance(2,3,4,5,1);
    assert(expected == actual);
    assert(16 == Yahtzee().Chance(3,3,4,5,1));
}

static int * ints(int a, int b, int c, int d, int e)
{
    int * r = new int[5];
    r[0] = a;
    r[1] = b;
    r[2] = c;
    r[3] = d;
    r[4] = e;
    return r;
}

static void Yahtzee_scores_50(void) 
{
    int expected = 50;
    int actual = Yahtzee().yahtzee(ints(4,4,4,4,4));
    assert(expected == actual);
    assert(50 == Yahtzee().yahtzee(ints(6,6,6,6,6)));
    assert(0 == Yahtzee().yahtzee(ints(6,6,6,6,3)));
}

static void Test_1s() 
{
    assert(Yahtzee().Ones(1,2,3,4,5) == 1);
    assert(2 == Yahtzee().Ones(1,2,1,4,5));
    assert(0 == Yahtzee().Ones(6,2,2,4,5));
    assert(4 == Yahtzee().Ones(1,2,1,1,1));
}

static void test_2s() 
{
    assert(4 == Yahtzee().Twos(1,2,3,2,6));
    assert(10 == Yahtzee().Twos(2,2,2,2,2));
}

static void test_threes() 
{
    assert(6 == Yahtzee().Threes(1,2,3,2,3));
    assert(12 == Yahtzee().Threes(2,3,3,3,3));
}

static void fours_test() 
{
    assert(12 == (new Yahtzee(4,4,4,5,5))->Fours());
    assert(8 == (new Yahtzee(4,4,5,5,5))->Fours());
    assert(4 == (*new Yahtzee(4,5,5,5,5)).Fours());
}

static void fives() {
    assert(10 == (new Yahtzee(4,4,4,5,5))->Fives());
    assert(15 == Yahtzee(4,4,5,5,5).Fives());
    assert(20 == Yahtzee(4,5,5,5,5).Fives());
}

static void sixes_test() 
{
    assert(0 == Yahtzee(4,4,4,5,5).sixes());
    assert(6 == Yahtzee(4,4,6,5,5).sixes());
    assert(18 == Yahtzee(6,5,6,6,5).sixes());
}

static void one_pair() 
{
    assert(6 == Yahtzee().ScorePair(3,4,3,5,6));
    assert(10 == Yahtzee().ScorePair(5,3,3,3,5));
    assert(12 == Yahtzee().ScorePair(5,3,6,6,5));
}

static void two_Pair() 
{
    assert(16 == Yahtzee().TwoPair(3,3,5,4,5));
    assert(0 == Yahtzee().TwoPair(3,3,5,5,5));
}

static void three_of_a_kind() 
{
    assert(9 == Yahtzee().ThreeOfAKind(3,3,3,4,5));
    assert(15 == Yahtzee().ThreeOfAKind(5,3,5,4,5));
    assert(0 == Yahtzee::ThreeOfAKind(3,3,3,3,5));
}

static void four_of_a_knd() 
{
    assert(12 == Yahtzee::FourOfAKind(3,3,3,3,5));
    assert(20 == Yahtzee::FourOfAKind(5,5,5,4,5));
    assert(0  == Yahtzee::FourOfAKind(3,3,3,3,3));
}

static void smallStraight() 
{
    assert(15 == Yahtzee::SmallStraight(1,2,3,4,5));
    assert(15 == Yahtzee::SmallStraight(2,3,4,5,1));
    assert(0 == Yahtzee().SmallStraight(1,2,2,4,5));
}

static void largeStraight() 
{
    assert(20 == Yahtzee::LargeStraight(6,2,3,4,5));
    assert(20 == Yahtzee().LargeStraight(2,3,4,5,6));
    assert(0== Yahtzee::LargeStraight(1,2,2,4,5));
}


static void fullHouse() 
{
    assert(18 == Yahtzee().FullHouse(6,2,2,2,6));
    assert(0 == Yahtzee().FullHouse(2,3,4,5,6));
}

typedef void test();

static test * tests[ ] =
{
    Chance_scores_sum_of_all_dice,
    Yahtzee_scores_50,
    Test_1s,
    test_2s,
    test_threes,
    fours_test,
    fives,
    sixes_test,
    one_pair,
    two_Pair,
    three_of_a_kind,
    four_of_a_knd,
    smallStraight,
    largeStraight,
    fullHouse,
    static_cast<test*>(0),
};

int main()
{
    size_t at = 0;
    while (tests[at])
    {
        tests[at++]();
        std::cout << '.';
    }
    std::cout << std::endl << at << " tests passed" << std::endl;
    return 0;
}