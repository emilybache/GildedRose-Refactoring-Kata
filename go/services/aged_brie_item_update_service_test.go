package services

import (
    "testing"
    "github.com/stretchr/testify/assert"

    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

// Aged Brie quality increments in 1 after each day
func TestAgedBrieItemUpdateService_QualityBeforeSellIn(t *testing.T) {
    runTestCase(t, func(
        agedBrieItemUpdateService AgedBrieItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Aged Brie", 5, 5})
        agedBrieItemUpdateService.UpdateQuality(item)
        assert.Equal(t, item.Model.Quality, 6)
    })
}

// If the sellIn date has been passed, the Aged Brie quality increments in 2
func TestAgedBrieItemUpdateService_QualityAfterSellIn0Days(t *testing.T) {
    runTestCase(t, func(
        agedBrieItemUpdateService AgedBrieItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Aged Brie", 0, 5})
        agedBrieItemUpdateService.UpdateQuality(item)
        assert.Equal(t, item.Model.Quality, 7)
    })
}

// If the sellIn date has been passed, the Aged Brie quality increments in 2
func TestAgedBrieItemUpdateService_QualityAfterSellIn4Days(t *testing.T) {
    runTestCase(t, func(
        agedBrieItemUpdateService AgedBrieItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Aged Brie", -4, 5})
        agedBrieItemUpdateService.UpdateQuality(item)
        assert.Equal(t, item.Model.Quality, 7)
    })
}
