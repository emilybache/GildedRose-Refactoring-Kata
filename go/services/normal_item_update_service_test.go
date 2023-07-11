package services

import (
    "testing"
    "github.com/stretchr/testify/assert"

    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

// Normal item quality decrements in 1 after each day
func TestNormalItemUpdateService_QualityBeforeSellIn(t *testing.T) {
    runTestCase(t, func(
        normalItemUpdateService NormalItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Random normal item", 5, 5})
        normalItemUpdateService.UpdateQuality(item)
        assert.Equal(t, item.Model.Quality, 4)
    })
}

// If the sellIn date has been passed, the Normal item quality decrements in 2
func TestNormalItemUpdateService_QualityAfterSellIn0Days(t *testing.T) {
    runTestCase(t, func(
        normalItemUpdateService NormalItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Random normal item", 0, 5})
        normalItemUpdateService.UpdateQuality(item)
        assert.Equal(t, item.Model.Quality, 3)
    })
}

// If the sellIn date has been passed, the Normal item quality decrements in 2
func TestNormalItemUpdateService_QualityAfterSellIn4Days(t *testing.T) {
    runTestCase(t, func(
        normalItemUpdateService NormalItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Random normal item", -4, 5})
        normalItemUpdateService.UpdateQuality(item)
        assert.Equal(t, item.Model.Quality, 3)
    })
}

// Quality must not be lower than 0
func TestNormalItemUpdateService_QualityNotLowerThan0(t *testing.T) {
    runTestCase(t, func(
        normalItemUpdateService NormalItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Random normal item", -4, 0})
        normalItemUpdateService.UpdateQuality(item)
        assert.Equal(t, item.Model.Quality, 0)
    })
}
