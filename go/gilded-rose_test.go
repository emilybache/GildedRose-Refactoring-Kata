package main

import "testing"

type Expected [2]int

type ItemTest struct {
	*Item
	Expected
}

func GetItemTestsComponents(itemTests []ItemTest) ([]*Item, []Expected) {
	var items []*Item
	var expected []Expected

	for _, itemTest := range itemTests {
		items = append(items, itemTest.Item)
		expected = append(expected, itemTest.Expected)
	}

	return items, expected
}

func Test_Items(t *testing.T) {
	var itemTests = []ItemTest{
		{&Item{"Aged Brie", 0, 0}, Expected{-1, 2}},
		{&Item{"Aged Brie", 5, 50}, Expected{4, 50}},
		{&Item{"Backstage passes to a TAFKAL80ETC concert", 5, 49}, Expected{4, 50}},
		{&Item{"Backstage passes to a TAFKAL80ETC concert", 0, 49}, Expected{-1, 0}},
		{&Item{"Backstage passes to a TAFKAL80ETC concert", 4, 4}, Expected{3, 7}},
		{&Item{"Backstage passes to a TAFKAL80ETC concert", 9, 4}, Expected{8, 6}},
		{&Item{"Sulfuras, Hand of Ragnaros", 0, 80}, Expected{0, 80}},
		{&Item{"Conjured Mana Cake", 3, 6}, Expected{2, 4}},
		{&Item{"Conjured Mana Cake", 0, 8}, Expected{-1, 4}},
		{&Item{"Conjured Mana Cake", 3, 0}, Expected{2, 0}},
		{&Item{"Acme Dynamite", 3, 6}, Expected{2, 5}},
		{&Item{"Acme Dynamite", 0, 6}, Expected{-1, 4}},
		{&Item{"Acme Dynamite", 0, 0}, Expected{-1, 0}},
	}

	items, expected := GetItemTestsComponents(itemTests)

	UpdateQuality(items)

	for i, item := range items {
		actual := [2]int{item.sellIn, item.quality}
		if actual != expected[i] {
			t.Errorf("%s: Expected %+v but got %+v", item.name, expected[i], actual)
		}
	}

}
