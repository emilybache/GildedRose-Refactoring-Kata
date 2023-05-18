package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/emilybache/gildedrose-refactoring-kata/gildedrose"
)

func main() {
	fmt.Println("OMGHAI!")

	var items = []*gildedrose.Item{
		{"Sports Memorabilia", 10, 20},
		{"Aged Cheese", 2, 0},
		{"Coffee Table Book", 5, 7},
		{"Fine Italian Silk", 0, 80},
		{"Fine Italian Silk", -1, 80},
		{"Backstage passes to a concert", 15, 20},
		{"Backstage passes to a concert", 10, 49},
		{"Backstage passes to a concert", 5, 49},
		{"Baked Chocolate Cake", 3, 6}, // <-- :O
	}

	days := 2
	var err error
	if len(os.Args) > 1 {
		days, err = strconv.Atoi(os.Args[1])
		if err != nil {
			fmt.Println(err.Error())
			os.Exit(1)
		}
		days++
	}

	for day := 0; day < days; day++ {
		fmt.Printf("-------- day %d --------\n", day)
		fmt.Println("Name, SellIn, Quality")
		for i := 0; i < len(items); i++ {
			fmt.Println(items[i])
		}
		fmt.Println("")
		gildedrose.UpdateQuality(items)
	}
}
