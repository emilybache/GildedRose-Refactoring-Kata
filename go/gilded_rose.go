package main

import "fmt"

type GildedRose struct {
	Items []Item
}

type Item struct {
	Name            string
	SellIn, Quality int
}

func main() {
	rose := GildedRose{
		[]Item{
			Item{"+5 Dexterity Vest", 10, 20},
			Item{"Aged Brie", 2, 0},
			Item{"Elixir of the Mongoose", 5, 7},
			Item{"Sulfuras, Hand of Ragnaros", 0, 80},
			Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20},
			Item{"Conjured Mana Cake", 3, 6},
		},
	}
	rose.UpdateQuality()
	fmt.Print(rose.String())
}

func (gr *GildedRose) UpdateQuality() {

	for i := 0; i < len(gr.Items); i++ {
		item := &gr.Items[i]

		if item.Name != "Aged Brie" && item.Name != "Backstage passes to a TAFKAL80ETC concert" {
			if item.Quality > 0 {
				if item.Name != "Sulfuras, Hand of Ragnaros" {
					item.Quality = item.Quality - 1
				}
			}
		} else {
			if item.Quality < 50 {
				item.Quality = item.Quality + 1
				if item.Name == "Backstage passes to a TAFKAL80ETC concert" {
					if item.SellIn < 11 {
						if item.Quality < 50 {
							item.Quality = item.Quality + 1
						}
					}
					if item.SellIn < 6 {
						if item.Quality < 50 {
							item.Quality = item.Quality + 1
						}
					}
				}
			}
		}

		if item.Name != "Sulfuras, Hand of Ragnaros" {
			item.SellIn = item.SellIn - 1
		}

		if item.SellIn < 0 {
			if item.Name != "Aged Brie" {
				if item.Name != "Backstage passes to a TAFKAL80ETC concert" {
					if item.Quality > 0 {
						if item.Name != "Sulfuras, Hand of Ragnaros" {
							item.Quality = item.Quality - 1
						}
					}
				} else {
					item.Quality = item.Quality - item.Quality
				}
			} else {
				if item.Quality < 50 {
					item.Quality = item.Quality + 1
				}
			}
		}
	}

}

func (gr *GildedRose) String() string {
	var str string
	str += "=== The Gilded Rose ===\n"
	for i, item := range gr.Items {
		str += fmt.Sprintf("%d) %s (quality %d, sell in %d)\n", i+1, item.Name, item.Quality, item.SellIn)
	}
	return str
}
