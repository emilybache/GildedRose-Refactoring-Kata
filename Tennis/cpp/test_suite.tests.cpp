#include "all_tests.hpp"
#include <cassert>
#include <iostream>

typedef void test();

static test * tests[ ] =
{
    test_LoveAll_0_0,
    test_FifteenAll_1_1,
    test_ThirtyAll_2_2,
    test_Deuce_3_3,
    test_Deuce_4_4,
    test_FifteenLove_1_0,
    test_LoveFifteen_0_1,
    test_ThirtyLove_2_0,
    test_LoveThirty_0_2,
    test_FortyLove_3_0,
    test_LoveForty_0_3,
    test_Winforplayer1_4_0,
    test_Winforplayer2_0_4,
    test_ThirtyFifteen_2_1,
    test_FifteenThirty_1_2,
    test_FortyFifteen_3_1,
    test_FifteenForty_1_3,
    test_Winforplayer1_4_1,
    test_Winforplayer2_1_4,
    test_FortyThirty_3_2,
    test_ThirtyForty_2_3,
    test_Winforplayer1_4_2,
    test_Winforplayer2_2_4,
    test_Advantageplayer1_4_3,
    test_Advantageplayer2_3_4,
    test_Advantageplayer1_5_4,
    test_Advantageplayer2_4_5,
    test_Advantageplayer1_15_14,
    test_Advantageplayer2_14_15,
    test_Winforplayer1_6_4,
    test_Winforplayer2_4_6,
    test_Winforplayer1_16_14,
    test_Winforplayer2_14_16,
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
