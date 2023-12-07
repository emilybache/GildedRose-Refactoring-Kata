import items

proc updateQuality*(items: var seq[Item]) =
  for i in 0 ..< items.len:
    if items[i].name != "Aged Brie" and items[i].name != "Backstage passes to a TAFKAL80ETC concert":
      if items[i].quality > 0:
        if items[i].name != "Sulfuras, Hand of Ragnaros":
          items[i].quality = items[i].quality - 1
    else:
      if items[i].quality < 50:
        items[i].quality = items[i].quality + 1

        if items[i].name == "Backstage passes to a TAFKAL80ETC concert":
          if items[i].sellIn < 11:
            if items[i].quality < 50:
              items[i].quality = items[i].quality + 1
          
          if items[i].sellIn < 6:
            if items[i].quality < 50:
              items[i].quality = items[i].quality + 1
    
    if items[i].name != "Sulfuras, Hand of Ragnaros":
      items[i].sellIn = items[i].sellIn - 1
    
    if items[i].sellIn < 0:
      if items[i].name != "Aged Brie":
        if items[i].name != "Backstage passes to a TAFKAL80ETC concert":
          if items[i].quality > 0:
            if items[i].name != "Sulfuras, Hand of Ragnaros":
              items[i].quality = items[i].quality - 1
        else:
          items[i].quality = items[i].quality - items[i].quality
      else:
        if items[i].quality < 50:
          items[i].quality = items[i].quality + 1
