package main

// Item represents an item for sale at the inn
type Item struct {
	name            string
	sellIn, quality int
}

// simple func to return a min of 2 integers
// useful in this context to stop items value going over 50
func min(x int, y int) int {
	if x == y {
		return x
	}
	if x < y {
		return x
	}
	return y
}

// simple func to return a max of 2 integers
// useful in this context to stop items value going under 0
func max(x int, y int) int {
	if x == y {
		return x
	}
	if x > y {
		return x
	}
	return y
}

// UpdateQualityBrie handles updates for aged brie
func UpdateQualityBrie(item *Item) {
	// always decrement sellInn
	defer func() {
		item.sellIn--
	}()

	// quality cannot go above 50
	if item.quality == 50 {
		return
	}

	// increment when prior to sellIn
	if item.sellIn > 0 {
		item.quality++
	}
	// increment by 2 if past sellInn date
	if item.sellIn <= 0 {
		item.quality = min(item.quality+2, 50)
	}

}

// UpdateQualityBackstagePasses handles updates for backstage passes
func UpdateQualityBackstagePasses(item *Item) {
	// always decrement sellInn
	defer func() {
		item.sellIn--
	}()

	// if past sellInn date then item is worthless
	if item.sellIn <= 0 {
		item.quality = 0
		return
	}

	// increment by 3 under with under 5 days to go
	if item.sellIn <= 5 {
		item.quality = min(item.quality+3, 50)
		return
	}

	// increment by 2 under with under 10 days to go
	if item.sellIn <= 10 {
		item.quality = min(item.quality+2, 50)
		return
	}

	// increment by 1 under normal conditions
	item.quality++

}

// UpdateQualityStandard handles updates for standard items
func UpdateQualityStandard(item *Item) {
	// always decrement sellInn
	defer func() {
		item.sellIn--
	}()

	// if past sellInn date then decrement by 2
	if item.sellIn <= 0 {
		item.quality = max(item.quality-2, 0)
		return
	}

	// decrement by 1
	item.quality = max(item.quality-1, 0)

}

// UpdateQualityConjured handles updates for conjured items
func UpdateQualityConjured(item *Item) {
	// always decrement sellInn
	defer func() {
		item.sellIn--
	}()

	// if past sellInn date then decrement by 4
	if item.sellIn <= 0 {
		item.quality = max(item.quality-4, 0)
		return
	}

	// decrement by 2
	item.quality = max(item.quality-2, 0)

}

// UpdateQuality handles the update of a slice of items. The individual items are handled
// by separate functions based on the name of the item.
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
