package lib

import (
    "github.com/gin-gonic/gin"
)

// RequestHandler function
type RequestHandler struct {
    Gin *gin.Engine
}

// NewRequestHandler creates a new request handler
func NewRequestHandler(logger Logger, env Env) RequestHandler {
    gin.DefaultWriter = logger.GetGinLogger()
    if env.Environment != "development" {
        gin.SetMode(gin.ReleaseMode)
    }
    engine := gin.New()
    return RequestHandler{Gin: engine}
}
