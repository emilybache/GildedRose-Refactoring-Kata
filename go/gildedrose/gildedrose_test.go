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
		{"foo", -1, -1},
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
