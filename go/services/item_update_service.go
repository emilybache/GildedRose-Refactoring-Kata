package services

import (
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type ItemUpdateService struct {
    logger     lib.Logger
}

func NewItemUpdateService(logger lib.Logger) ItemUpdateService {
    return ItemUpdateService{
        logger:     logger,
    }
}

func (this ItemUpdateService) UpdateQuality(item *models.Item) error {
    item.Mutex.Lock()
    defer item.Mutex.Unlock()

    itemModel := item.Model

    if itemModel.Name != "Aged Brie" && itemModel.Name != "Backstage passes to a TAFKAL80ETC concert" {
        if itemModel.Quality > 0 {
            if itemModel.Name != "Sulfuras, Hand of Ragnaros" {
                itemModel.Quality = itemModel.Quality - 1
            }
        }
    } else {
        if itemModel.Quality < 50 {
            itemModel.Quality = itemModel.Quality + 1
            if itemModel.Name == "Backstage passes to a TAFKAL80ETC concert" {
                if itemModel.SellIn < 11 {
                    if itemModel.Quality < 50 {
                        itemModel.Quality = itemModel.Quality + 1
                    }
                }
                if itemModel.SellIn < 6 {
                    if itemModel.Quality < 50 {
                        itemModel.Quality = itemModel.Quality + 1
                    }
                }
            }
        }
    }

    if itemModel.Name != "Sulfuras, Hand of Ragnaros" {
        itemModel.SellIn = itemModel.SellIn - 1
    }

    if itemModel.SellIn < 0 {
        if itemModel.Name != "Aged Brie" {
            if itemModel.Name != "Backstage passes to a TAFKAL80ETC concert" {
                if itemModel.Quality > 0 {
                    if itemModel.Name != "Sulfuras, Hand of Ragnaros" {
                        itemModel.Quality = itemModel.Quality - 1
                    }
                }
            } else {
                itemModel.Quality = itemModel.Quality - itemModel.Quality
            }
        } else {
            if itemModel.Quality < 50 {
                itemModel.Quality = itemModel.Quality + 1
            }
        }
    }

    return nil
}
