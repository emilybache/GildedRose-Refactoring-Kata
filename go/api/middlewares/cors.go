package middlewares

import (
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    cors "github.com/rs/cors/wrapper/gin"
)

type CorsMiddleware struct {
    handler lib.RequestHandler
    logger  lib.Logger
    env     lib.Env
}

func NewCorsMiddleware(handler lib.RequestHandler, logger lib.Logger, env lib.Env) CorsMiddleware {
    return CorsMiddleware{
        handler: handler,
        logger:  logger,
        env:     env,
    }
}

func (this CorsMiddleware) Setup() {
    this.logger.Info("Setting up cors middleware")

    debug := this.env.Environment == "development"
    this.handler.Gin.Use(cors.New(cors.Options{
        AllowCredentials: true,
        AllowOriginFunc:  func(origin string) bool { return true },
        AllowedHeaders:   []string{"*"},
        AllowedMethods:   []string{"GET", "POST", "PUT", "HEAD", "OPTIONS"},
        Debug:            debug,
    }))
}
