package routes

import (
    "github.com/emilybache/gildedrose-refactoring-kata/api/controllers"
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
)

type SwaggerRoutes struct {
    logger             lib.Logger
    handler            lib.RequestHandler
    swaggerController  controllers.SwaggerController
}

func (this SwaggerRoutes) Setup() {
    this.logger.Info("Setting up swagger routes")
    this.swaggerController.Setup(this.handler.Gin)
}

func NewSwaggerRoutes(
    logger              lib.Logger,
    handler            lib.RequestHandler,
    swaggerController  controllers.SwaggerController,
) SwaggerRoutes {
    return SwaggerRoutes{
        logger:            logger,
        handler:           handler,
        swaggerController: swaggerController,
    }
}
