# -*- coding: utf-8 -*-

import pytest
from tennis import TennisGame

from tennis_unittest import test_cases, play_game

class TestTennis:

    @pytest.mark.parametrize('p1Points p2Points score p1Name p2Name'.split(), test_cases)
    def test_get_score(self, p1Points, p2Points, score, p1Name, p2Name):
        game = play_game(p1Points, p2Points, p1Name, p2Name)
        assert score == game.score()
