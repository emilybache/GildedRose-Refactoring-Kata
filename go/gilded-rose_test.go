package main

import ("testing" 
        "fmt")

func Test_Foo(t *testing.T) {
	var items = []*Item{
		&Item{"Conjured Mana Cake", 1, 10},
	}

	days := 2
	for day := 0; day < days; day++ {
	    UpdateQuality(items)
		fmt.Println(items[0])
	}

	if items[0].name != "Conjured Mana Cake" {
		t.Errorf("Name: Expected %s but got %s ", "Conjured Mana Cake", items[0].name)
	}
	if items[0].quality != 4 {
		t.Errorf("Quality:  Expected %s but got %d","4",items[0].quality)
	}
}
