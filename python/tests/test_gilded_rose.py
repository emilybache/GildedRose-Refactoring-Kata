# -*- coding: utf-8 -*-
import sys
import unittest
from pathlib import Path

_EXERCISE_ROOT = Path(__file__).resolve().parent.parent
if str(_EXERCISE_ROOT) not in sys.path:
    sys.path.insert(0, str(_EXERCISE_ROOT))

from gilded_rose import Item, GildedRose


class GildedRoseTest(unittest.TestCase):
    def test_normal_item_at_zero_quality_stays_zero(self):
        items = [Item("foo", 0, 0)]
        gilded_rose = GildedRose(items)
        gilded_rose.update_quality()
        self.assertEqual(0, items[0].quality)

        
if __name__ == '__main__':
    unittest.main()
