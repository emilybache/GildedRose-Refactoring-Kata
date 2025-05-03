package test_gilded_rose

import GildedRose "../src/core"
import "core:testing"

@(test)
update_quality_test :: proc(t: ^testing.T) {
	items := []GildedRose.Item{{"food", 0, 0}}
	GildedRose.update_quality(items)
	testing.expect(t, "fixme" == items[0].name, "Fix this test")
}
