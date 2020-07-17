package main

import (
	"fmt"
	"io"
	"os"
	"testing"
)

var tests = []struct {
	startItem    Item
	expectedItem Item
	days         int
}{
	{Item{"normal", 10, 20}, Item{"normal", 10, 20}, 0},
	{Item{"normal", 10, 20}, Item{"normal", 0, 10}, 10},
	{Item{"normal", 10, 20}, Item{"normal", -1, 8}, 11},
	{Item{"normal", 10, 20}, Item{"normal", -20, 0}, 30},
	{Item{"Aged Brie", 2, 0}, Item{"Aged Brie", 2, 0}, 0},
	{Item{"Aged Brie", 2, 0}, Item{"Aged Brie", 0, 2}, 2},
	{Item{"Aged Brie", 2, 0}, Item{"Aged Brie", -23, 48}, 25},
	{Item{"Aged Brie", 2, 0}, Item{"Aged Brie", -24, 50}, 26},
	{Item{"Aged Brie", 2, 0}, Item{"Aged Brie", -25, 50}, 27},
	{Item{"Aged Brie", 2, 0}, Item{"Aged Brie", -28, 50}, 30},
	{Item{"Sulfuras, Hand of Ragnaros", 0, 80}, Item{"Sulfuras, Hand of Ragnaros", 0, 80}, 0},
	{Item{"Sulfuras, Hand of Ragnaros", 0, 80}, Item{"Sulfuras, Hand of Ragnaros", 0, 80}, 15},
	{Item{"Sulfuras, Hand of Ragnaros", 0, 80}, Item{"Sulfuras, Hand of Ragnaros", 0, 80}, 30},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, 0},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 14, 21}, 1},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 13, 22}, 2},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 10, 25}, 5},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 9, 27}, 6},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 8, 29}, 7},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 5, 35}, 10},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 4, 38}, 11},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 3, 41}, 12},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", 0, 50}, 15},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", -1, 0}, 16},
	{Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20}, Item{"Backstage passes to a TAFKAL80ETC concert", -10, 0}, 25},
	{Item{"Conjured Mana Cake", 3, 6}, Item{"Conjured Mana Cake", 3, 6}, 0},
	{Item{"Conjured Mana Cake", 3, 6}, Item{"Conjured Mana Cake", 2, 4}, 1},
	{Item{"Conjured Mana Cake", 3, 6}, Item{"Conjured Mana Cake", 1, 2}, 2},
	{Item{"Conjured Mana Cake", 3, 6}, Item{"Conjured Mana Cake", 0, 0}, 3},
	{Item{"Conjured Mana Cake", 3, 6}, Item{"Conjured Mana Cake", -1, 0}, 4},
	{Item{"Conjured Mana Cake", 3, 6}, Item{"Conjured Mana Cake", -2, 0}, 5},
	{Item{"Conjured Mana Cake", 3, 6}, Item{"Conjured Mana Cake", -7, 0}, 10},
}

func TestGildedRose(t *testing.T) {
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	defer func() {
		os.Stdout = oldStdout
		w.Close()
		if t.Failed() {
			fmt.Println("=== Start Captured Stdout ===")
			io.Copy(os.Stdout, r)
			fmt.Println("=== End Captured Stdout ===")
		}
	}()
	os.Stdout = w
	for tn, test := range tests {
		items = []Item{test.startItem}
		for i := 0; i < test.days; i++ {
			main()
		}
		if items[0] != test.expectedItem {
			t.Logf("Test Number[%d]: Expected starting item %+v to equal %+v after %d days: Got %v", tn, test.startItem, test.expectedItem, test.days, items[0])
			t.Fail()
		}
	}
}

