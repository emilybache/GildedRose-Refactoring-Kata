package domains

import (
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type ItemUpdateService interface {
    UpdateQuality(item *models.Item) error
}
