package services

import (
    "github.com/emilybache/gildedrose-refactoring-kata/domains"
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type ItemUpdateServiceProvider struct {
    logger                          lib.Logger
    itemUpdateService               ItemUpdateService
}

func NewItemUpdateServiceProvider(
    logger                          lib.Logger,
    itemUpdateService               ItemUpdateService,
) domains.ItemUpdateServiceProvider {

    return ItemUpdateServiceProvider{
        logger:                          logger,
        itemUpdateService:               itemUpdateService,
    }
}

func (this ItemUpdateServiceProvider) GetUpdateService(item *models.Item) domains.ItemUpdateService {
    return this.itemUpdateService
}
