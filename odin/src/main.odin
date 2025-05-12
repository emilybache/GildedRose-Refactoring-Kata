package main

import GildedRose "./core"
import "core:fmt"
import "core:os"
import "core:slice"
import "core:strconv"

main :: proc() {
	days := 2
	if num_of_days, arg_ok := slice.get(os.args, 1); arg_ok {
		if val, ok := strconv.parse_int(num_of_days); ok && val > 0 {
			days = val
		} else {
			fmt.eprintf("Please enter a number greater than 0\n")
			return
		}
	}

	items := []GildedRose.Item {
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
		GildedRose.update_quality(items)
	}
}
