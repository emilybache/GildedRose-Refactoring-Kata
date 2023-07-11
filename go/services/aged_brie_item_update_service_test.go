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
        assert.Equal(t, 6, item.Model.Quality)
    })
}

// If the sellIn date has been passed, the Aged Brie quality increments in 2
func TestAgedBrieItemUpdateService_QualityAfterSellIn0Days(t *testing.T) {
    runTestCase(t, func(
        agedBrieItemUpdateService AgedBrieItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Aged Brie", 0, 5})
        agedBrieItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 7, item.Model.Quality)
    })
}

// If the sellIn date has been passed, the Aged Brie quality increments in 2
func TestAgedBrieItemUpdateService_QualityAfterSellIn4Days(t *testing.T) {
    runTestCase(t, func(
        agedBrieItemUpdateService AgedBrieItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Aged Brie", -4, 5})
        agedBrieItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 7, item.Model.Quality)
    })
}

// Quality must not be greater than 50
func TestAgedBrieItemUpdateService_QualityNotHigherThan50(t *testing.T) {
    runTestCase(t, func(
        agedBrieItemUpdateService AgedBrieItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Aged Brie", -4, 50})
        agedBrieItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 50, item.Model.Quality)
    })
}

// sellIn date must decrease
func TestAgedBrieItemUpdateService_SellInIsDecreased(t *testing.T) {
    runTestCase(t, func(
        sulfurasItemUpdateService SulfurasItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Aged Brie", 5, 5})
        sulfurasItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 4, item.Model.SellIn)
    })
}
