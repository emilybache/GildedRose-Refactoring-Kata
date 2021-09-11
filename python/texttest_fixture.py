# -*- coding: utf-8 -*-
from __future__ import print_function

import constants
import gilded_rose

if __name__ == "__main__":
    print("OMGHAI!")
    items = [
        gilded_rose.Item(name="+5 Dexterity Vest", sell_in=10, quality=20),
        gilded_rose.Item(name=constants.AGED_BRIE, sell_in=2, quality=0),
        gilded_rose.Item(name="Elixir of the Mongoose", sell_in=5, quality=7),
        gilded_rose.Item(name=constants.SULFURAS, sell_in=0, quality=80),
        gilded_rose.Item(name=constants.SULFURAS, sell_in=-1, quality=80),
        gilded_rose.Item(name=constants.BACKSTAGE, sell_in=15, quality=20),
        gilded_rose.Item(name=constants.BACKSTAGE, sell_in=10, quality=49),
        gilded_rose.Item(name=constants.BACKSTAGE, sell_in=5, quality=49),
        gilded_rose.Item(name="Conjured Mana Cake", sell_in=3, quality=6),  # <-- :O
    ]

    days = 2
    import sys

    if len(sys.argv) > 1:
        days = int(sys.argv[1]) + 1
    for day in range(days):
        print("-------- day %s --------" % day)
        print("name, sellIn, quality")
        for item in items:
            print(item)
        print("")
        gilded_rose.GildedRose(items).update_quality()
