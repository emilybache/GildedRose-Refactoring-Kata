package main

import "fmt"

type Item struct {
	name            string
	sellIn, quality int
}

var items = []Item{
	Item{"+5 Dexterity Vest", 10, 20},
	Item{"Aged Brie", 2, 0},
	Item{"Elixir of the Mongoose", 5, 7},
	Item{"Sulfuras, Hand of Ragnaros", 0, 80},
	Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20},
	Item{"Conjured Mana Cake", 3, 6},
}

type Updatable interface {
	Update()
}

type UpdatableItem func(item *Item) Updatable

type AgedBrieItem struct {
	*Item
}

func (item *AgedBrieItem) Update() {
	item.updateSellIn(-1)
	if item.sellIn < 0 {
		item.updateQuality(+2)
	} else {
		item.updateQuality(+1)
	}
	item.limitQualityToMax(50)
}

type SulfurasItem struct {
	*Item
}

func (item *SulfurasItem) Update() {
}

type BackstagePassesItem struct {
	*Item
}

func (item *BackstagePassesItem) Update() {
	item.updateSellIn(-1)
	sellIn := item.sellIn
	switch {
	case sellIn >= 10:
		item.updateQuality(+1)
	case sellIn < 10 && sellIn >= 5:
		item.updateQuality(+2)
	case sellIn < 15 && sellIn >= 0:
		item.updateQuality(+3)
	case sellIn < 0:
		item.limitQualityToMax(0)
	}
	item.limitQualityToMax(50)
}

type RegularItem struct {
	*Item
}

func (item *RegularItem) Update() {
	item.updateSellIn(-1)
	if item.sellIn < 0 {
		item.updateQuality(-2)
	} else {
		item.updateQuality(-1)
	}
	item.limitQualityToMin(0)
}

type ConjuredItem struct {
	*Item
}

func (item *ConjuredItem) Update() {
	item.updateSellIn(-1)
	if item.sellIn < 0 {
		item.updateQuality(-4)
	} else {
		item.updateQuality(-2)
	}
	item.limitQualityToMin(0)
}

func UpdatableItemFactory(createClosure map[string]UpdatableItem, item *Item) Updatable {
	create, exists := createClosure[item.name]
	if exists {
		return create(item)
	}
	return &RegularItem{
		Item: item,
	}
}

func (item *Item) updateSellIn(updatedBy int) {
	item.sellIn += updatedBy
}

func (item *Item) updateQuality(updatedBy int) {
	item.quality += updatedBy
}

func (item *Item) limitQualityToMax(max int) {
	if item.quality > max {
		item.quality = max
	}
}

func (item *Item) limitQualityToMin(min int) {
	if item.quality < min {
		item.quality = min
	}
}

func GildedRose(items []Item) {
	creationMap := map[string]UpdatableItem{
		"Aged Brie": func(item *Item) Updatable {
			return &AgedBrieItem{
				Item: item,
			}
		},
		"Sulfuras, Hand of Ragnaros": func(item *Item) Updatable {
			return &SulfurasItem{
				Item: item,
			}
		},
		"Backstage passes to a TAFKAL80ETC concert": func(item *Item) Updatable {
			return &BackstagePassesItem{
				Item: item,
			}
		},
		"Conjured Mana Cake": func(item *Item) Updatable {
			return &ConjuredItem{
				Item: item,
			}
		},
	}

	for i := 0; i < len(items); i++ {
		updatableItem := UpdatableItemFactory(creationMap, &items[i])
		updatableItem.Update()
	}
}

func main() {
	fmt.Println("# Before updating")
	// fmt.Println(items)
	GildedRose(items)
}

