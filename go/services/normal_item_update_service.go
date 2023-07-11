package services

import (
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type NormalItemUpdateService struct {
    logger     lib.Logger
}

func NewNormalItemUpdateService(logger lib.Logger) NormalItemUpdateService {
    return NormalItemUpdateService{
        logger:     logger,
    }
}

func (this NormalItemUpdateService) UpdateQuality(item *models.Item) error {
    decrement := 1

    item.Mutex.Lock()
    defer item.Mutex.Unlock()

    itemModel := item.Model

    if itemModel.SellIn <= 0 {
        decrement *= 2
    }
    if (itemModel.Quality - decrement) < 0 {
        itemModel.Quality = 0
    } else {
        itemModel.Quality -= decrement
    }
    itemModel.SellIn--

    return nil
}
