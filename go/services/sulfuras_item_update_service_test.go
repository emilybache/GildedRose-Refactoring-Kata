package services

import (
    "testing"
    "github.com/stretchr/testify/assert"

    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

// Sulfuras item quality is 80 before sellIn
func TestSulfurasItemUpdateService_QualityBeforeSellIn(t *testing.T) {
    runTestCase(t, func(
        sulfurasItemUpdateService SulfurasItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Sulfuras, Hand of Ragnaros", 5, 5})
        sulfurasItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 80, item.Model.Quality)
    })
}

// Sulfuras item sellIn is immutable
func TestSulfurasItemUpdateService_SellInIsImmutable(t *testing.T) {
    runTestCase(t, func(
        sulfurasItemUpdateService SulfurasItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Sulfuras, Hand of Ragnaros", 5, 5})
        sulfurasItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 5, item.Model.SellIn)
    })
}

// If the sellIn date has been passed, the Sulfuras item quality is still 80
func TestSulfurasItemUpdateService_QualityAfterSellIn0Days(t *testing.T) {
    runTestCase(t, func(
        sulfurasItemUpdateService SulfurasItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Sulfuras, Hand of Ragnaros", 0, 5})
        sulfurasItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 80, item.Model.Quality)
    })
}

// If the sellIn date has been passed, the Sulfuras item quality is still 80
func TestSulfurasItemUpdateService_QualityAfterSellIn4Days(t *testing.T) {
    runTestCase(t, func(
        sulfurasItemUpdateService SulfurasItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Sulfuras, Hand of Ragnaros", -4, 5})
        sulfurasItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 80, item.Model.Quality)
    })
}

// Sulfuras item negative sellIn is immutable
func TestSulfurasItemUpdateService_NegativeSellInIsImmutable(t *testing.T) {
    runTestCase(t, func(
        sulfurasItemUpdateService SulfurasItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Sulfuras, Hand of Ragnaros", -4, 5})
        sulfurasItemUpdateService.UpdateQuality(item)
        assert.Equal(t, -4, item.Model.SellIn)
    })
}
