package main

import "testing"

func Test_Foo(t *testing.T) {
	var items = []*Item{
		&Item{"fixme", 1, 10},
	}

	UpdateQuality(items)

	if items[0].name != "fixme" {
		t.Errorf("Name: Expected %s but got %s ", "fixme", items[0].name)
	}
}
