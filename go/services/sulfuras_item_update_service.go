package services

import (
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type SulfurasItemUpdateService struct {
    logger     lib.Logger
}

func NewSulfurasItemUpdateService(logger lib.Logger) SulfurasItemUpdateService {
    return SulfurasItemUpdateService{
        logger:     logger,
    }
}

func (this SulfurasItemUpdateService) UpdateQuality(item *models.Item) error {
    item.Mutex.Lock()
    defer item.Mutex.Unlock()

    item.Model.Quality = 80

    return nil
}
