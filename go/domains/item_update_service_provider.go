package domains

import (
    "github.com/emilybache/gildedrose-refactoring-kata/models"
)

type ItemUpdateServiceProvider interface {
    GetUpdateService(item *models.Item) ItemUpdateService
}
