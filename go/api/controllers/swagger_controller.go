package controllers

import (
    docs "github.com/emilybache/gildedrose-refactoring-kata/docs"
    swaggerfiles "github.com/swaggo/files"
    ginSwagger "github.com/swaggo/gin-swagger"

    "github.com/emilybache/gildedrose-refactoring-kata/lib"

    "github.com/gin-gonic/gin"
)

type SwaggerController struct {
    logger   lib.Logger
}

func NewSwaggerController(logger lib.Logger) SwaggerController {
    return SwaggerController{
        logger:  logger,
    }
}

func (this *SwaggerController) Setup(engine *gin.Engine) {
    docs.SwaggerInfo.Title = "Gilded Rose Service"
    docs.SwaggerInfo.Description = "This service implements the Gilded Rose item update service."
    docs.SwaggerInfo.Version = "1.0"
    docs.SwaggerInfo.BasePath = "/"

    engine.GET("/swagger/*any", ginSwagger.WrapHandler(swaggerfiles.Handler))
}
