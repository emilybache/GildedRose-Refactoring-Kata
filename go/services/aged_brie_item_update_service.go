package services

import (
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type AgedBrieItemUpdateService struct {
    logger     lib.Logger
}

func NewAgedBrieItemUpdateService(logger lib.Logger) AgedBrieItemUpdateService {
    return AgedBrieItemUpdateService{
        logger:   logger,
    }
}

func (this AgedBrieItemUpdateService) UpdateQuality(item *models.Item) error {
    item.Mutex.Lock()
    defer item.Mutex.Unlock()

    if item.Model.Quality < 50 {
        item.Model.Quality++
    }
    if item.Model.SellIn <= 0 && item.Model.Quality < 50 {
        item.Model.Quality++
    }
    item.Model.SellIn--

    return nil
}
