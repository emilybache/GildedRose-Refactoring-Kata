package main

type Item struct {
	name            string
	sellIn, quality int
}

func min(x int, y int) int {
	if x == y {
		return x
	}
	if x < y {
		return x
	}
	return y
}

func UpdateQualityBrie(item *Item) {
	defer func() {
		item.sellIn--
	}()

	if item.quality == 50 {
		return
	}

	if item.sellIn > 0 {
		item.quality++
	}
	if item.sellIn <= 0 {
		item.quality = min(item.quality+2, 50)
	}

}

func UpdateQualityBackstagePasses(item *Item) {
	defer func() {
		item.sellIn--
	}()

	if item.sellIn <= 0 {
		item.quality = 0
	}

	if item.quality == 50 {
		return
	}
	if item.quality == 49 && item.sellIn <= 0 {
		item.quality = 50
	}
	if item.sellIn > 0 {
		item.quality++
	}
	if item.sellIn <= 0 {
		item.quality += 2
	}

}

func UpdateQuality(items []*Item) {
	for i := 0; i < len(items); i++ {

		if items[i].name == "Sulfuras, Hand of Ragnaros" {
			continue
		}

		if items[i].name == "Aged Brie" {
			UpdateQualityBrie(items[i])
			continue
		}

		if items[i].name != "Backstage passes to a TAFKAL80ETC concert" {
			if items[i].quality > 0 {
				items[i].quality = items[i].quality - 1
			}
		} else {
			if items[i].quality < 50 {
				items[i].quality = items[i].quality + 1
				if items[i].name == "Backstage passes to a TAFKAL80ETC concert" {
					if items[i].sellIn < 11 {
						if items[i].quality < 50 {
							items[i].quality = items[i].quality + 1
						}
					}
					if items[i].sellIn < 6 {
						if items[i].quality < 50 {
							items[i].quality = items[i].quality + 1
						}
					}
				}
			}
		}

		items[i].sellIn = items[i].sellIn - 1

		if items[i].sellIn < 0 {
			if items[i].name != "Aged Brie" {
				if items[i].name != "Backstage passes to a TAFKAL80ETC concert" {
					if items[i].quality > 0 {
						items[i].quality = items[i].quality - 1
					}
				} else {
					items[i].quality = items[i].quality - items[i].quality
				}
			} else {
				if items[i].quality < 50 {
					items[i].quality = items[i].quality + 1
				}
			}
		}
	}

}
