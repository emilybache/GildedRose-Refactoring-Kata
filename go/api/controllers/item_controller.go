package controllers

import (
    "net/http"

    "github.com/emilybache/gildedrose-refactoring-kata/domains"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/api/middlewares"
    "github.com/gin-gonic/gin"
)

type ItemController struct {
    logger                       lib.Logger
    contentTypeFilterMiddleware  middlewares.ContentTypeFilterMiddleware
    itemUpdateServiceProvider    domains.ItemUpdateServiceProvider
}

type UpdateQualityRequest struct {
    Days   int                    `json:"days" binding:"gte=0"`
    Items  []*models.ItemModel    `json:"items" binding:"required"`
}

func NewItemController(
    logger                       lib.Logger,
    contentTypeFilterMiddleware  middlewares.ContentTypeFilterMiddleware,
    itemUpdateServiceProvider    domains.ItemUpdateServiceProvider,
) ItemController {
    return ItemController{
        logger:                       logger,
        contentTypeFilterMiddleware:  contentTypeFilterMiddleware,
        itemUpdateServiceProvider:    itemUpdateServiceProvider,
    }
}

func (this *ItemController) Setup(engine *gin.Engine) {
    engine.GET("/status", this.getStatus)
    engine.POST("/update_quality", this.contentTypeFilterMiddleware.Handler("application/json"), this.postUpdateQuality)
    engine.NoRoute(this.invalidPath)
}

func (this *ItemController) invalidPath(ctx *gin.Context) {
    ctx.AbortWithStatus(http.StatusNotFound)
}

// getStatus godoc
// @Summary Health check (get status)
// @Schemes
// @Description Return a simple OK response
// @Tags health
// @Accept json
// @Produce json
// @Success 200 "Service is up and running"
// @Router /status [get]
func (this *ItemController) getStatus(ctx *gin.Context) {
    ctx.JSON(http.StatusOK, gin.H{
        "status": "ok",
    })
}

// postUpdateQuality godoc
// @Summary Execute Update Quality for all given items and passed days
// @Schemes
// @Description Load a full list of items that will be handled by the service and process them, returning the item list with the updated values for the given days
// @Tags items
// @Accept json
// @Param message body UpdateQualityRequest true "Items info"
// @Success 200 "Result of items with updated quality"
// @Failure 400 "Invalid data in the request"
// @Router /update_quality [post]
func (this *ItemController) postUpdateQuality(ctx *gin.Context) {

    var request UpdateQualityRequest

    if err := ctx.BindJSON(&request); err != nil {
        ctx.AbortWithStatus(http.StatusBadRequest)
        return
    }

    var items = make([]*models.Item, len(request.Items))
    for i, item := range request.Items {
        items[i] = models.NewItem(item)
    }

    for day := 0; day < request.Days; day++ {
        for _, item := range items {
            itemUpdateService := this.itemUpdateServiceProvider.GetUpdateService(item)
            itemUpdateService.UpdateQuality(item)
        }
    }

    var itemEntities = make([]*models.ItemModel, len(items))
    for i, item := range items {
        itemEntities[i] = item.Model
    }

    ctx.JSON(http.StatusOK, itemEntities)
}
