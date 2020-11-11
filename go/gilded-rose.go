package main

type Item struct {
	name            string
	sellIn, quality int
}
func ReduceSulfuraConjureAndOthersQuality(item *Item) {
	if item.quality > 0 {
		if item.name != "Sulfuras, Hand of Ragnaros" {
			if item.name == "Conjured Mana Cake" {
				item.quality = item.quality - 2
			} else {
				item.quality = item.quality - 1
			}
		}
	}
}

func ExtraBackstageOperations(item *Item){
	if item.name == "Backstage passes to a TAFKAL80ETC concert" {
		if item.sellIn < 11 {
			if item.quality < 50 {
				item.quality = item.quality + 1
			}
		}
		if item.sellIn < 6 {
			if item.quality < 50 {
				item.quality = item.quality + 1
			}
		}
	}
}

func UpdateQuality(items []*Item) {
	for i := 0; i < len(items); i++ {

		if items[i].name != "Aged Brie" && items[i].name != "Backstage passes to a TAFKAL80ETC concert" {
			ReduceSulfuraConjureAndOthersQuality(items[i])
		} else {
			if items[i].quality < 50 {
				items[i].quality = items[i].quality + 1
				ExtraBackstageOperations(items[i])
			}
		}

		if items[i].name != "Sulfuras, Hand of Ragnaros" {
			items[i].sellIn = items[i].sellIn - 1
		}

		if items[i].sellIn < 0 {
			if items[i].name != "Aged Brie" {
				if items[i].name != "Backstage passes to a TAFKAL80ETC concert" {
					ReduceSulfuraConjureAndOthersQuality(items[i])
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
