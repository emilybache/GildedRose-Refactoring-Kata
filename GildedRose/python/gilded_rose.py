
def update_quality(items):
    for item in items:
        if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert":
            if item.quality > 0:
                if item.name != "Sulfuras, Hand of Ragnaros":
                    item.quality = item.quality - 1;
        else:
            if item.quality < 50:
                item.quality = item.quality + 1;
                if item.name == "Backstage passes to a TAFKAL80ETC concert":
                    if item.sell_in < 11:
                        if item.quality < 50:
                            item.quality = item.quality + 1;
                    if item.sell_in < 6:
                        if item.quality < 50:
                            item.quality = item.quality + 1;
        if item.name != "Sulfuras, Hand of Ragnaros":
            item.sell_in = item.sell_in - 1;
        if item.sell_in < 0:
            if item.name != "Aged Brie":
                if item.name != "Backstage passes to a TAFKAL80ETC concert":
                    if item.quality > 0:
                        if item.name != "Sulfuras, Hand of Ragnaros":
                            item.quality = item.quality - 1;
                else:
                    item.quality = item.quality - item.quality;
            else:
                if item.quality < 50:
                    item.quality = item.quality + 1;
    return items

class Item:
    def __init__(self, name, sell_in, quality):
        self.name = name
        self.sell_in = sell_in
        self.quality = quality
        
    def __repr__(self):
        return "%s, %s, %s" % (self.name, self.sell_in, self.quality)
        
if __name__ == "__main__":
    print ("OMGHAI!")
    items = [
             Item(name="+5 Dexterity Vest", sell_in=10, quality=20),
             Item(name="Aged Brie", sell_in=2, quality=0),
             Item(name="Elixir of the Mongoose", sell_in=5, quality=7),
             Item(name="Sulfuras, Hand of Ragnaros", sell_in=0, quality=80),
             Item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20),
             Item(name="Conjured Mana Cake", sell_in=3, quality=6),
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
        update_quality(items)