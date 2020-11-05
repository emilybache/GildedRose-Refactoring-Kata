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

func max(x int, y int) int {
	if x == y {
		return x
	}
	if x > y {
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
		return
	}

	if item.sellIn <= 5 {
		item.quality = min(item.quality+3, 50)
		return
	}

	if item.sellIn <= 10 {
		item.quality = min(item.quality+2, 50)
		return
	}

	item.quality++

}

func UpdateQualityStandard(item *Item) {
	defer func() {
		item.sellIn--
	}()

	if item.sellIn <= 0 {
		item.quality = max(item.quality-2, 0)
		return
	}

	item.quality = max(item.quality-1, 0)

}

func UpdateQualityConjured(item *Item) {
	defer func() {
		item.sellIn--
	}()

	if item.sellIn <= 0 {
		item.quality = max(item.quality-4, 0)
		return
	}

	item.quality = max(item.quality-2, 0)

}

func UpdateQuality(items []*Item) {
	for i := 0; i < len(items); i++ {

		switch name := items[i].name; name {
		case "Sulfuras, Hand of Ragnaros":
		case "Aged Brie":
			UpdateQualityBrie(items[i])
		case "Backstage passes to a TAFKAL80ETC concert":
			UpdateQualityBackstagePasses(items[i])
		case "Conjured Mana Cake":
			UpdateQualityConjured(items[i])
		default:
			UpdateQualityStandard(items[i])
		}
	}

}
