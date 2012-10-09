import pytest
from tennis import TennisGame

from tennis_unittest import test_cases

class TestTennis:

    @pytest.mark.parametrize('p1Points p2Points score p1Name p2Name'.split(), test_cases)
    def test_get_score(self, p1Points, p2Points, score, p1Name, p2Name):
        game = TennisGame(p1Name, p2Name)
        for i in range(p1Points):
            game.won_point(p1Name)
        for i in range(p2Points):
            game.won_point(p2Name)
        assert score == game.score()

