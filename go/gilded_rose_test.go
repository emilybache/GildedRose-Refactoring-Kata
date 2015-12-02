package main

import "testing"

func Test_GildedRose_UpdateQuality_DoesNotChangeItemName(t *testing.T) {
	rose := GildedRose{[]Item{Item{"Foo", 0, 0}}}
	expectedName := "Fixme"
	rose.UpdateQuality()
	if rose.Items[0].Name != expectedName {
		t.Errorf("Expected Item name to be %#v (got %#v)", expectedName, rose.Items[0].Name)
	}
}
