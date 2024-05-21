package gildedrose

import (
	"github.com/emilybache/gildedrose-refactoring-kata/gildedrose/domain"
)

const (
	AgedBrie            = "Aged Brie"
	BackstagePasses     = "Backstage passes to a TAFKAL80ETC concert"
	Sulfuras            = "Sulfuras, Hand of Ragnaros"
	Conjured            = "Conjured Mana Cake"
	MaxQuality          = 50
	MinQuality          = 0
	SulfurasQuality     = 80
	Expired             = 0
	Backstage10Days     = 10
	Backstage5Days      = 5
	NormalDegradeRate   = 1
	ConjuredDegradeRate = 2
)

func UpdateQuality(items []*domain.Item) {
	for _, item := range items {
		updateItem(item)
		validateQuality(item)
	}
}
func validateQuality(item *domain.Item) {
	if item.Name == Sulfuras {
		if item.Quality != SulfurasQuality {
			item.Quality = SulfurasQuality
		}
		return
	}
	if item.Quality > MaxQuality || item.Quality < MinQuality {
		item.Quality = MinQuality
	}
}
func updateItem(item *domain.Item) {
	switch item.Name {
	case AgedBrie:
		incrementQuality(item)
	case BackstagePasses:
		updateBackstagePasses(item)
	case Sulfuras:
		// Sulfuras does not change in quality or sell-in
	case Conjured:
		degradeQuality(item, ConjuredDegradeRate)
	default:
		degradeQuality(item, NormalDegradeRate)
	}

	if item.Name != Sulfuras {
		item.SellIn--
	}

	if item.SellIn < Expired {
		handleExpiredItem(item)
	}
}

func incrementQuality(item *domain.Item) {
	if item.Quality < MaxQuality {
		item.Quality++
	}
}
func updateBackstagePasses(item *domain.Item) {
	incrementQuality(item)
	if item.SellIn <= Backstage10Days && item.Quality < MaxQuality {
		item.Quality++
	}
	if item.SellIn <= Backstage5Days && item.Quality < MaxQuality {
		item.Quality++
	}
}

func degradeQuality(item *domain.Item, degradeRate int) {
	if item.Quality > MinQuality {
		item.Quality -= degradeRate
	}
}

func handleExpiredItem(item *domain.Item) {
	switch item.Name {
	case AgedBrie:
		incrementQuality(item)
	case BackstagePasses:
		item.Quality = MinQuality
	case Conjured:
		degradeQuality(item, ConjuredDegradeRate)
	case Sulfuras:
		// Sulfuras does not change in quality or sell-in
	default:
		degradeQuality(item, NormalDegradeRate)
	}
}
