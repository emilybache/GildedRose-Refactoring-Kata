package gildedrose_test

import (
	"github.com/emilybache/gildedrose-refactoring-kata/gildedrose"
	"github.com/emilybache/gildedrose-refactoring-kata/gildedrose/domain"
	"github.com/stretchr/testify/require"
	"testing"
)

func Test_UpdateQuality(t *testing.T) {

	type TestConfig struct {
		name     string
		testData []*domain.Item
		expected domain.Item
	}

	for _, tc := range []TestConfig{
		{
			name:     "Quality of an item is never less than 0",
			testData: []*domain.Item{{Name: "Normal Item", SellIn: 10, Quality: 0}},
			expected: domain.Item{Name: "Normal Item", SellIn: 9, Quality: 0},
		},
		{
			name:     "Quality of an item degrade twice as sellIn date passed",
			testData: []*domain.Item{{Name: "Normal Item", SellIn: 0, Quality: 10}},
			expected: domain.Item{Name: "Normal Item", SellIn: -1, Quality: 8},
		},
		{
			name:     "Quality of an item degrade once as sellIn date not passed",
			testData: []*domain.Item{{Name: "Normal Item", SellIn: 10, Quality: 10}},
			expected: domain.Item{Name: "Normal Item", SellIn: 9, Quality: 9},
		},

		{
			name:     "Sell by date not passed, increase AgedBrie quality by 1",
			testData: []*domain.Item{{Name: gildedrose.AgedBrie, SellIn: 3, Quality: 40}},
			expected: domain.Item{Name: gildedrose.AgedBrie, SellIn: 2, Quality: 41},
		},
		{
			name:     "Quality of an AgedBrie is never more than 50",
			testData: []*domain.Item{{Name: gildedrose.AgedBrie, SellIn: 10, Quality: 50}},
			expected: domain.Item{Name: gildedrose.AgedBrie, SellIn: 9, Quality: 50},
		},
		{
			name:     "Quality of an item upgrade twice as sellIn date passed - AgedBrie",
			testData: []*domain.Item{{Name: gildedrose.AgedBrie, SellIn: 0, Quality: 40}},
			expected: domain.Item{Name: gildedrose.AgedBrie, SellIn: -1, Quality: 42},
		},
		{
			name:     "BackstagePasses increase in quality as SellIn approaches - 15 days",
			testData: []*domain.Item{{Name: gildedrose.BackstagePasses, SellIn: 15, Quality: 20}},
			expected: domain.Item{Name: gildedrose.BackstagePasses, SellIn: 14, Quality: 21},
		},
		{
			name:     "BackstagePasses increase in quality by 2 when SellIn <= 10",
			testData: []*domain.Item{{Name: gildedrose.BackstagePasses, SellIn: 10, Quality: 20}},
			expected: domain.Item{Name: gildedrose.BackstagePasses, SellIn: 9, Quality: 22},
		},
		{
			name:     "BackstagePasses increase in quality by 3 when SellIn <= 5",
			testData: []*domain.Item{{Name: gildedrose.BackstagePasses, SellIn: 5, Quality: 20}},
			expected: domain.Item{Name: gildedrose.BackstagePasses, SellIn: 4, Quality: 23},
		},
		{
			name:     "BackstagePasses increase in quality by 3 when SellIn <= 5, quality 49",
			testData: []*domain.Item{{Name: gildedrose.BackstagePasses, SellIn: 5, Quality: 49}},
			expected: domain.Item{Name: gildedrose.BackstagePasses, SellIn: 4, Quality: 50},
		},
		{
			name:     "BackstagePasses drop to 0 quality after concert",
			testData: []*domain.Item{{Name: gildedrose.BackstagePasses, SellIn: 0, Quality: 20}},
			expected: domain.Item{Name: gildedrose.BackstagePasses, SellIn: -1, Quality: 0},
		},
		{
			name:     "Sulfuras never decreases in quality or SellIn, SellIn positive",
			testData: []*domain.Item{{Name: gildedrose.Sulfuras, SellIn: 10, Quality: 80}},
			expected: domain.Item{Name: gildedrose.Sulfuras, SellIn: 10, Quality: 80},
		},
		{
			name:     "Sulfuras never decreases in quality or SellIn, SellIn negative",
			testData: []*domain.Item{{Name: gildedrose.Sulfuras, SellIn: -1, Quality: 80}},
			expected: domain.Item{Name: gildedrose.Sulfuras, SellIn: -1, Quality: 80},
		},
		{
			name:     "Create Sulfuras item with quality different than 80",
			testData: []*domain.Item{{Name: gildedrose.Sulfuras, SellIn: 10, Quality: 1000}},
			expected: domain.Item{Name: gildedrose.Sulfuras, SellIn: 10, Quality: 80},
		},
		{
			name:     "Conjured items degrade in quality twice as fast - before sell-by date - initial quality 6",
			testData: []*domain.Item{{Name: gildedrose.Conjured, SellIn: 3, Quality: 6}},
			expected: domain.Item{Name: gildedrose.Conjured, SellIn: 2, Quality: 4},
		},
		{
			name:     "Conjured items degrade in quality twice as fast - before sell-by date - initial quality 1",
			testData: []*domain.Item{{Name: gildedrose.Conjured, SellIn: 3, Quality: 1}},
			expected: domain.Item{Name: gildedrose.Conjured, SellIn: 2, Quality: 0},
		},
		{
			name:     "Conjured items degrade in quality twice as fast - before sell-by date - initial quality 0",
			testData: []*domain.Item{{Name: gildedrose.Conjured, SellIn: -3, Quality: 0}},
			expected: domain.Item{Name: gildedrose.Conjured, SellIn: -4, Quality: 0},
		},
		{
			name:     "Conjured items degrade in quality twice as fast - after sell-by date",
			testData: []*domain.Item{{Name: gildedrose.Conjured, SellIn: 0, Quality: 6}},
			expected: domain.Item{Name: gildedrose.Conjured, SellIn: -1, Quality: 2},
		},
		{
			name:     "Create item with quality bigger than 50",
			testData: []*domain.Item{{Name: gildedrose.Conjured, SellIn: 10, Quality: 100}},
			expected: domain.Item{Name: gildedrose.Conjured, SellIn: 9, Quality: 0},
		},
		{
			name:     "Create item with quality less than 0",
			testData: []*domain.Item{{Name: gildedrose.Conjured, SellIn: 10, Quality: -100}},
			expected: domain.Item{Name: gildedrose.Conjured, SellIn: 9, Quality: 0},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			gildedrose.UpdateQuality(tc.testData)
			require.Equal(t, tc.expected, *tc.testData[0])
		})
	}
}
