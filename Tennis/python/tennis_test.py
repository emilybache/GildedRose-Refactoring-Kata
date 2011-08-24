
from tennis import TennisGame

# test support code
def params(funcarglist):
    def wrapper(function):
        function.funcarglist = funcarglist
        return function
    return wrapper

def pytest_generate_tests(metafunc):
    for funcargs in getattr(metafunc.function, 'funcarglist', ()):
        metafunc.addcall(funcargs=funcargs)

# actual test code
class TestTennis:

    @params([dict(p1Points=0, p2Points=0, score="Love-All"),
             dict(p1Points=1, p2Points=1, score="Fifteen-All"),
             dict(p1Points=2, p2Points=2, score="Thirty-All"),
             dict(p1Points=3, p2Points=3, score="Forty-All"),
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
            ])
    def test_get_score(self, p1Points, p2Points, score):
        game = TennisGame("player1", "player2")
        for i in range(p1Points):
            game.won_point("player1")
        for i in range(p2Points):
            game.won_point("player2")
        assert score == game.score()

