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
		t.Errorf("Name: Expected %s but got %s ", "fixme", items[0].Name)
	}
}
