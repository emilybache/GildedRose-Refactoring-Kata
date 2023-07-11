package services

import (
    "testing"
    "github.com/stretchr/testify/assert"

    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

// BackstagePass item quality increments +1 >10 days before sellIn
func TestBackstagePassItemUpdateService_QualityBeforeSellIn(t *testing.T) {
    runTestCase(t, func(
        backstagePassItemUpdateService BackstagePassItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Backstage passes to a TAFKAL80ETC concert", 20, 5})
        backstagePassItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 6, item.Model.Quality)
    })
}

// BackstagePass item quality increments +2 10 days before sellIn
func TestBackstagePassItemUpdateService_QualityBeforeSellIn10Days(t *testing.T) {
    runTestCase(t, func(
        backstagePassItemUpdateService BackstagePassItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Backstage passes to a TAFKAL80ETC concert", 10, 5})
        backstagePassItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 7, item.Model.Quality)
    })
}

// BackstagePass item quality increments +3 5 days before sellIn
func TestBackstagePassItemUpdateService_QualityBeforeSellIn5Days(t *testing.T) {
    runTestCase(t, func(
        backstagePassItemUpdateService BackstagePassItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Backstage passes to a TAFKAL80ETC concert", 5, 5})
        backstagePassItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 8, item.Model.Quality)
    })
}

// If the sellIn date has been passed, the BackstagePass item quality is 0
func TestBackstagePassItemUpdateService_QualityAfterSellIn0Days(t *testing.T) {
    runTestCase(t, func(
        backstagePassItemUpdateService BackstagePassItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Backstage passes to a TAFKAL80ETC concert", 0, 5})
        backstagePassItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 0, item.Model.Quality)
    })
}

// If the sellIn date has been passed, the BackstagePass item quality is still 0
func TestBackstagePassItemUpdateService_QualityAfterSellIn4Days(t *testing.T) {
    runTestCase(t, func(
        backstagePassItemUpdateService BackstagePassItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Backstage passes to a TAFKAL80ETC concert", -4, 5})
        backstagePassItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 0, item.Model.Quality)
    })
}

// Quality must not be greater than 50
func TestBackstagePassItemUpdateService_QualityNotHigherThan50(t *testing.T) {
    runTestCase(t, func(
        backstagePassItemUpdateService BackstagePassItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Backstage passes to a TAFKAL80ETC concert", 1, 50})
        backstagePassItemUpdateService.UpdateQuality(item)
        assert.Equal(t, 50, item.Model.Quality)
    })
}

// sellIn date must decrease
func TestBackstagePassItemUpdateService_SellInIsDecreased(t *testing.T) {
    runTestCase(t, func(
        backstagePassItemUpdateService BackstagePassItemUpdateService,
    ) {
        item := models.NewItem(&models.ItemModel{"Backstage passes to a TAFKAL80ETC concert", -4, 0})
        backstagePassItemUpdateService.UpdateQuality(item)
        assert.Equal(t, -5, item.Model.SellIn)
    })
}
