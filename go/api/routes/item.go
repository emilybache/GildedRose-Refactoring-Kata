package routes

import (
    "github.com/emilybache/gildedrose-refactoring-kata/api/controllers"
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
)

type ItemControllerRoutes struct {
    logger          lib.Logger
    handler         lib.RequestHandler
    itemController  controllers.ItemController
}

func (this ItemControllerRoutes) Setup() {
    this.logger.Info("Setting up item controller routes")
    this.itemController.Setup(this.handler.Gin)
}

func NewItemControllerRoutes(
    logger          lib.Logger,
    handler         lib.RequestHandler,
    itemController  controllers.ItemController,
) ItemControllerRoutes {
    return ItemControllerRoutes{
        logger:          logger,
        handler:         handler,
        itemController:  itemController,
    }
}
