package gilded_rose

import "core:fmt"
import "core:os"
import "core:strconv"

Item :: struct {
	name:    string,
	sell_in: i32,
	quality: i32,
}

update_quality :: proc(inventory: []Item) {
	for &item in inventory {
		if item.name != "Aged Brie" && item.name != "Backstage passes to a TAFKAL80ETC concert" {
			if item.quality > 0 {
				if item.name != "Sulfuras, Hand of Ragnaros" {
					item.quality = item.quality - 1
				}
			}
		} else {
			if item.quality < 50 {
				item.quality = item.quality + 1
				if item.name == "Backstage passes to a TAFKAL80ETC concert" {
					if item.sell_in < 11 {
						if item.quality < 50 {
							item.quality = item.quality + 1
						}
					}
					if item.sell_in < 6 {
						if item.quality < 50 {
							item.quality = item.quality + 1
						}
					}
				}
			}
		}
		if item.name != "Sulfuras, Hand of Ragnaros" {
			item.sell_in = item.sell_in - 1
		}
		if item.sell_in < 0 {
			if item.name != "Aged Brie" {
				if item.name != "Backstage passes to a TAFKAL80ETC concert" {
					if item.quality > 0 {
						if item.name != "Sulfuras, Hand of Ragnaros" {
							item.quality = item.quality - 1
						}
					}
				} else {
					item.quality = item.quality - item.quality
				}
			} else {
				if item.quality < 50 {
					item.quality = item.quality + 1
				}
			}
		}
	}
}

main :: proc() {
	days := 2
	if len(os.args) >= 2 {
		if val, ok := strconv.parse_int(os.args[1]); ok && val > 0 {
			days = val
		} else {
			fmt.eprintf("Please enter a number greater than 0\n")
			return
		}
	}

	items := []Item {
		{"+5 Dexterity Vest", 10, 20},
		{"Aged Brie", 2, 0},
		{"Elixir of the Mongoose", 5, 7},
		{"Sulfuras, Hand of Ragnaros", 0, 80},
		{"Sulfuras, Hand of Ragnaros", -1, 80},
		{"Backstage passes to a TAFKAL80ETC concert", 15, 20},
		{"Backstage passes to a TAFKAL80ETC concert", 10, 49},
		{"Backstage passes to a TAFKAL80ETC concert", 5, 49},
		// This Conjured item does not work properly yet
		{"Conjured Mana Cake", 3, 6},
	}

	fmt.println("OMGHAI!")

	for day in 0 ..= days {
		day_line := fmt.tprint("-------- day", day, "--------")
		fmt.println(day_line)
		fmt.println("name, sellIn, quality")
		for item in items {
			item_line := fmt.tprint(item.name, item.sell_in, item.quality, sep = ", ")
			fmt.println(item_line)
		}
		fmt.println()
		update_quality(items)
	}
}
