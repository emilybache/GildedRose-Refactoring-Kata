package main

import (
	"fmt"
	"testing"
	"strconv"
)
func Print_Items(items []*Item){
	fmt.Println("name, sellIn, quality")
	for i := 0; i < len(items); i++ {
		fmt.Println(items[i])
	}
	fmt.Println("")
}
func Test_Foo(t *testing.T) {
	var items = []*Item{
		&Item{"+5 Dexterity Vest", 10, 20},
		&Item{"Aged Brie", 2, 0},
		&Item{"Elixir of the Mongoose", 5, 7},
		&Item{"Sulfuras, Hand of Ragnaros", 0, 80},
		&Item{"Sulfuras, Hand of Ragnaros", -1, 80},
		&Item{"Backstage passes to a TAFKAL80ETC concert", 15, 20},
		&Item{"Backstage passes to a TAFKAL80ETC concert", 10, 49},
		&Item{"Backstage passes to a TAFKAL80ETC concert", 5, 49},
		&Item{"Conjured Mana Cake", 15, 40}, // <-- :O
		&Item{"Conjured Mana Cake", 3, 46}, // <-- :O
	}

	days := 12

	for day := 0; day < days; day++ {
		fmt.Printf("-------- day %d --------\n", day)
		Print_Items(items)
		UpdateQuality(items)
	}
	fmt.Printf("-------- LAST day %d --------\n", days)
	Print_Items(items)

	//+5 Dexterity Vest
	if items[0].name != "+5 Dexterity Vest" {
		t.Errorf("Name: Expected %s but got %s ", "+5 Dexterity Vest", items[0].name)
	}
	if items[0].sellIn != -2 {
		t.Errorf("Sellin: Expected %s but got %s ", "-2", strconv.Itoa(items[0].sellIn))
	}
	if items[0].quality != 6 {
		t.Errorf("Quality: Expected %s but got %s ", "6", strconv.Itoa(items[0].quality))
	}

	//Aged Brie -10 22}
	if items[1].name != "Aged Brie" {
		t.Errorf("Name: Expected %s but got %s ", "Aged Brie", items[1].name)
	}
	if items[1].sellIn != -10 {
		t.Errorf("Sellin: Expected %s but got %s ", "-10", strconv.Itoa(items[1].sellIn))
	}
	if items[1].quality != 22 {
		t.Errorf("Quality: Expected %s but got %s ", "22", strconv.Itoa(items[1].quality))
	}

	//Elixir of the Mongoose -7 0}
	if items[2].name != "Elixir of the Mongoose" {
		t.Errorf("Name: Expected %s but got %s ", "Elixir of the Mongoose", items[2].name)
	}
	if items[2].sellIn != -7 {
		t.Errorf("Sellin: Expected %s but got %s ", "-7", strconv.Itoa(items[2].sellIn))
	}
	if items[2].quality != 0 {
		t.Errorf("Quality: Expected %s but got %s ", "0", strconv.Itoa(items[2].quality))
	}

	//Sulfuras, Hand of Ragnaros 0 80}
	if items[3].name != "Sulfuras, Hand of Ragnaros" {
		t.Errorf("Name: Expected %s but got %s ", "Sulfuras, Hand of Ragnaros", items[3].name)
	}
	if items[3].sellIn != 0 {
		t.Errorf("Sellin: Expected %s but got %s ", "0", strconv.Itoa(items[3].sellIn))
	}
	if items[3].quality != 80 {
		t.Errorf("Quality: Expected %s but got %s ", "80", strconv.Itoa(items[3].quality))
	}

	//Sulfuras, Hand of Ragnaros -1 80}
	if items[4].name != "Sulfuras, Hand of Ragnaros" {
		t.Errorf("Name: Expected %s but got %s ", "Sulfuras, Hand of Ragnaros", items[4].name)
	}
	if items[4].sellIn != -1 {
		t.Errorf("Sellin: Expected %s but got %s ", "-1", strconv.Itoa(items[4].sellIn))
	}
	if items[4].quality != 80 {
		t.Errorf("Quality: Expected %s but got %s ", "80", strconv.Itoa(items[4].quality))
	}

	//Backstage passes to a TAFKAL80ETC concert 3 41}
	if items[5].name != "Backstage passes to a TAFKAL80ETC concert" {
		t.Errorf("Name: Expected %s but got %s ", "Backstage passes to a TAFKAL80ETC concert", items[5].name)
	}
	if items[5].sellIn != 3 {
		t.Errorf("Sellin: Expected %s but got %s ", "3", strconv.Itoa(items[5].sellIn))
	}
	if items[5].quality != 41 {
		t.Errorf("Quality: Expected %s but got %s ", "41", strconv.Itoa(items[5].quality))
	}

	//Backstage passes to a TAFKAL80ETC concert -2 0}
	if items[6].name != "Backstage passes to a TAFKAL80ETC concert" {
		t.Errorf("Name: Expected %s but got %s ", "Backstage passes to a TAFKAL80ETC concert", items[6].name)
	}
	if items[6].sellIn != -2 {
		t.Errorf("Sellin: Expected %s but got %s ", "-2", strconv.Itoa(items[6].sellIn))
	}
	if items[6].quality != 0 {
		t.Errorf("Quality: Expected %s but got %s ", "0", strconv.Itoa(items[6].quality))
	}

	//Backstage passes to a TAFKAL80ETC concert -7 0}
	if items[7].name != "Backstage passes to a TAFKAL80ETC concert" {
		t.Errorf("Name: Expected %s but got %s ", "Backstage passes to a TAFKAL80ETC concert", items[7].name)
	}
	if items[7].sellIn != -7 {
		t.Errorf("Sellin: Expected %s but got %s ", "-7", strconv.Itoa(items[7].sellIn))
	}
	if items[7].quality != 0 {
		t.Errorf("Quality: Expected %s but got %s ", "0", strconv.Itoa(items[7].quality))
	}

	//Conjured Mana Cake 15 40}
    //expected behaviour sellin=15-12, quality=40-2*12=16
	if items[8].name != "Conjured Mana Cake" {
		t.Errorf("Name: Expected %s but got %s ", "Conjured Mana Cake", items[8].name)
	}
	if items[8].sellIn != 3 {
		t.Errorf("Sellin: Expected %s but got %s ", "3", strconv.Itoa(items[8].sellIn))
	}
	if items[8].quality != 16 {
		t.Errorf("Quality: Expected %s but got %s ", "16", strconv.Itoa(items[8].quality))
	}

	//Conjured Mana Cake 3 46}
    //expected behaviour sellin=3-12=-9, quality=46-(3*2 + 9*4)=4
	if items[9].name != "Conjured Mana Cake" {
		t.Errorf("Name: Expected %s but got %s ", "Conjured Mana Cake", items[9].name)
	}
	if items[9].sellIn != -9 {
		t.Errorf("Sellin: Expected %s but got %s ", "-9", strconv.Itoa(items[9].sellIn))
	}
	if items[9].quality != 4 {
		t.Errorf("Quality: Expected %s but got %s ", "4", strconv.Itoa(items[9].quality))
	}

}
