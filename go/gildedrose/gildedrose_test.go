package gildedrose_test

import (
	"testing"

	"github.com/emilybache/gildedrose-refactoring-kata/gildedrose"
)

func Test_Foo(t *testing.T) {
	// Arrange
	var items = []*gildedrose.Item{
		{"foo", 0, 0},
	}
	expected := []*gildedrose.Item{
		{"foo", -1, 0},
	}

	// Act
	gildedrose.UpdateQuality(items)

	// Assert
	if items[0].Name != expected[0].Name {
		t.Errorf("Name: Expected %s but got %s ", expected[0].Name, items[0].Name)
	}
	if items[0].SellIn != expected[0].SellIn {
		t.Errorf("SellIn: Expected %d but got %d ", expected[0].SellIn, items[0].SellIn)
	}
	if items[0].Quality != expected[0].Quality {
		t.Errorf("Quality: Expected %d but got %d ", expected[0].Quality, items[0].Quality)
	}
}

func Test_oneDay(t *testing.T) {
	// Arrange
	var items = []*gildedrose.Item{
		{"+5 Dexterity Vest", 10, 20},
		{"Aged Brie", 2, 0},
		{"Elixir of the Mongoose", 5, 7},
		{"Sulfuras, Hand of Ragnaros", 0, 80},
		{"Sulfuras, Hand of Ragnaros", -1, 80},
		{"Backstage passes to a TAFKAL80ETC concert", 15, 20},
		{"Backstage passes to a TAFKAL80ETC concert", 10, 49},
		{"Backstage passes to a TAFKAL80ETC concert", 5, 49},
		{"Conjured Mana Cake", 3, 6},
	}

	var expected = []*gildedrose.Item{
		{"+5 Dexterity Vest", 9, 19},
		{"Aged Brie", 1, 1},
		{"Elixir of the Mongoose", 4, 6},
		{"Sulfuras, Hand of Ragnaros", 0, 80},
		{"Sulfuras, Hand of Ragnaros", -1, 80},
		{"Backstage passes to a TAFKAL80ETC concert", 14, 21},
		{"Backstage passes to a TAFKAL80ETC concert", 9, 50},
		{"Backstage passes to a TAFKAL80ETC concert", 4, 50},
		{"Conjured Mana Cake", 2, 5}, // <-- :O
	}

	// Act
	gildedrose.UpdateQuality(items)

	// Assert
	for i, item := range expected {
		if items[i].Name != item.Name {
			t.Errorf("Name: Expected %s but got %s ", item.Name, items[i].Name)
		}
		if items[i].SellIn != item.SellIn {
			t.Errorf("%s - SellIn: Expected %d but got %d ", item.Name, item.SellIn, items[i].SellIn)
		}
		if items[i].Quality != item.Quality {
			t.Errorf("%s - Quality: Expected %d but got %d ", item.Name, item.Quality, items[i].Quality)
		}
	}

}

func Test_twoDays(t *testing.T) {
	// Arrange
	var items = []*gildedrose.Item{
		{"+5 Dexterity Vest", 10, 20},
		{"Aged Brie", 2, 0},
		{"Elixir of the Mongoose", 5, 7},
		{"Sulfuras, Hand of Ragnaros", 0, 80},
		{"Sulfuras, Hand of Ragnaros", -1, 80},
		{"Backstage passes to a TAFKAL80ETC concert", 15, 20},
		{"Backstage passes to a TAFKAL80ETC concert", 10, 49},
		{"Backstage passes to a TAFKAL80ETC concert", 5, 49},
		{"Conjured Mana Cake", 3, 6},
	}

	var expected = []*gildedrose.Item{
		{"+5 Dexterity Vest", 8, 18},
		{"Aged Brie", 0, 2},
		{"Elixir of the Mongoose", 3, 5},
		{"Sulfuras, Hand of Ragnaros", 0, 80},
		{"Sulfuras, Hand of Ragnaros", -1, 80},
		{"Backstage passes to a TAFKAL80ETC concert", 13, 22},
		{"Backstage passes to a TAFKAL80ETC concert", 8, 50},
		{"Backstage passes to a TAFKAL80ETC concert", 3, 50},
		{"Conjured Mana Cake", 1, 4}, // <-- :O
	}

	// Act
	gildedrose.UpdateQuality(items)
	gildedrose.UpdateQuality(items)

	// Assert
	for i, item := range expected {
		if items[i].Name != item.Name {
			t.Errorf("Name: Expected %s but got %s ", item.Name, items[i].Name)
		}
		if items[i].SellIn != item.SellIn {
			t.Errorf("%s - SellIn: Expected %d but got %d ", item.Name, item.SellIn, items[i].SellIn)
		}
		if items[i].Quality != item.Quality {
			t.Errorf("%s - Quality: Expected %d but got %d ", item.Name, item.Quality, items[i].Quality)
		}
	}
}
func Test_fiveDays(t *testing.T) {
	// Arrange
	var items = []*gildedrose.Item{
		{"+5 Dexterity Vest", 10, 20},
		{"Aged Brie", 2, 0},
		{"Elixir of the Mongoose", 5, 7},
		{"Sulfuras, Hand of Ragnaros", 0, 80},
		{"Sulfuras, Hand of Ragnaros", -1, 80},
		{"Backstage passes to a TAFKAL80ETC concert", 15, 20},
		{"Backstage passes to a TAFKAL80ETC concert", 10, 49},
		{"Backstage passes to a TAFKAL80ETC concert", 5, 49},
		{"Conjured Mana Cake", 3, 6},
	}

	var expected = []*gildedrose.Item{
		{"+5 Dexterity Vest", 0, 10},
		{"Aged Brie", -8, 18},
		{"Elixir of the Mongoose", -5, 0},
		{"Sulfuras, Hand of Ragnaros", 0, 80},
		{"Sulfuras, Hand of Ragnaros", -1, 80},
		{"Backstage passes to a TAFKAL80ETC concert", 5, 35},
		{"Backstage passes to a TAFKAL80ETC concert", 0, 50},
		{"Backstage passes to a TAFKAL80ETC concert", -5, 0},
		{"Conjured Mana Cake", -7, 0}, // <-- :O
	}

	// Act
	for day := 1; day <= 10; day++ {
		gildedrose.UpdateQuality(items)
	}

	// Assert
	for i, item := range expected {
		if items[i].Name != item.Name {
			t.Errorf("Name: Expected %s but got %s ", item.Name, items[i].Name)
		}
		if items[i].SellIn != item.SellIn {
			t.Errorf("%s - SellIn: Expected %d but got %d ", item.Name, item.SellIn, items[i].SellIn)
		}
		if items[i].Quality != item.Quality {
			t.Errorf("%s - Quality: Expected %d but got %d ", item.Name, item.Quality, items[i].Quality)
		}
	}
}
