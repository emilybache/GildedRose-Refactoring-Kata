"""
    texttest_fixture.py
    Simulates item updates for the Gilded Rose.
    Author: Mohammed Mohideen M Z
"""
from gilded_rose import Item, GildedRose
import sys

def main():
    print("OMGHAI!")
    items = [
        Item(name="+5 Dexterity Vest", sell_in=10, quality=20),
        Item(name="Aged Brie", sell_in=2, quality=0),
        Item(name="Elixir of the Mongoose", sell_in=5, quality=7),
        Item(name="Sulfuras, Hand of Ragnaros", sell_in=0, quality=80),
        Item(name="Sulfuras, Hand of Ragnaros", sell_in=-1, quality=80),
        Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20),
        Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=49),
        Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49),
        Item(name="Conjured Mana Cake", sell_in=3, quality=6),  # <-- :O
    ]
    days = int(sys.argv[1]) + 1 if len(sys.argv) > 1 else 2

    app = GildedRose(items)

    for day in range(days):
        print("-------- day {} --------".format(day))
        print("name, sellIn, quality")
        for item in items:
            print(item)
        print("")
        app.update_quality()

if __name__ == "__main__":
    main()
