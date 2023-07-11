package services

import (
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type BackstagePassItemUpdateService struct {
    logger     lib.Logger
}

func NewBackstagePassItemUpdateService(logger lib.Logger) BackstagePassItemUpdateService {
    return BackstagePassItemUpdateService{
        logger:     logger,
    }
}

func (this BackstagePassItemUpdateService) UpdateQuality(item *models.Item) error {
    item.Mutex.Lock()
    defer item.Mutex.Unlock()

    if item.Model.SellIn <= 0 {
        item.Model.Quality = 0
    } else {
        increment := 1
        if item.Model.SellIn <= 5 {
            increment = 3
        } else if item.Model.SellIn <= 10 {
            increment = 2
        }
        if (item.Model.Quality + increment) > 50 {
            item.Model.Quality = 50
        } else {
            item.Model.Quality += increment
        }
    }
    item.Model.SellIn--

    return nil
}
