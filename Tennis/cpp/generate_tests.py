

test_cases = [dict(p1Points=0, p2Points=0, score="Love-All"),
         dict(p1Points=1, p2Points=1, score="Fifteen-All"),
         dict(p1Points=2, p2Points=2, score="Thirty-All"),
         dict(p1Points=3, p2Points=3, score="Deuce"),
         dict(p1Points=4, p2Points=4, score="Deuce"),

         dict(p1Points=1, p2Points=0, score="Fifteen-Love"),
         dict(p1Points=0, p2Points=1, score="Love-Fifteen"),
         dict(p1Points=2, p2Points=0, score="Thirty-Love"),
         dict(p1Points=0, p2Points=2, score="Love-Thirty"),
         dict(p1Points=3, p2Points=0, score="Forty-Love"),
         dict(p1Points=0, p2Points=3, score="Love-Forty"),
         dict(p1Points=4, p2Points=0, score="Win for player1"),
         dict(p1Points=0, p2Points=4, score="Win for player2"),

         dict(p1Points=2, p2Points=1, score="Thirty-Fifteen"),
         dict(p1Points=1, p2Points=2, score="Fifteen-Thirty"),
         dict(p1Points=3, p2Points=1, score="Forty-Fifteen"),
         dict(p1Points=1, p2Points=3, score="Fifteen-Forty"),
         dict(p1Points=4, p2Points=1, score="Win for player1"),
         dict(p1Points=1, p2Points=4, score="Win for player2"),

         dict(p1Points=3, p2Points=2, score="Forty-Thirty"),
         dict(p1Points=2, p2Points=3, score="Thirty-Forty"),
         dict(p1Points=4, p2Points=2, score="Win for player1"),
         dict(p1Points=2, p2Points=4, score="Win for player2"),

         dict(p1Points=4, p2Points=3, score="Advantage player1"),
         dict(p1Points=3, p2Points=4, score="Advantage player2"),
         dict(p1Points=5, p2Points=4, score="Advantage player1"),
         dict(p1Points=4, p2Points=5, score="Advantage player2"),
         dict(p1Points=15, p2Points=14, score="Advantage player1"),
         dict(p1Points=14, p2Points=15, score="Advantage player2"),

         dict(p1Points=6, p2Points=4, score="Win for player1"),
         dict(p1Points=4, p2Points=6, score="Win for player2"),
         dict(p1Points=16, p2Points=14, score="Win for player1"),
         dict(p1Points=14, p2Points=16, score="Win for player2"),
        ]

def create_testcase_dicts():
    testcase_dicts = []
    for test in test_cases:
        cleaned = test["score"]
        cleaned = cleaned.replace("-", "")
        cleaned = cleaned.replace(" ", "")
        test["cleaned"] = cleaned
        test["testcase_name"] = "%(cleaned)s_%(p1Points)s_%(p2Points)s" % test
        testcase_dicts.append(test)
    return testcase_dicts

testcase_dicts = create_testcase_dicts()

# This is not currently being used
gtest_template = """\
TEST(TennisTest, %(testcase_name)s) {
  EXPECT_EQ("%(score)s", tennis_score(%(p1Points)s, %(p2Points)s));
}
"""

template = """\
void test_%(testcase_name)s()
{
    assert("%(score)s" == tennis_score(%(p1Points)s, %(p2Points)s));
}
"""
# test cases for all_tests.cpp
for test in testcase_dicts:
    print template % test

# test_suite.tests.cpp
for test in testcase_dicts:
    print "    test_%(testcase_name)s," % test

# all_tests.hpp
for test in testcase_dicts:
    print "void test_%(testcase_name)s();" % test