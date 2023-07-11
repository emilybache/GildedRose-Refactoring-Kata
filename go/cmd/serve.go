package cmd

import (
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/api/middlewares"
    "github.com/emilybache/gildedrose-refactoring-kata/api/routes"
    "github.com/spf13/cobra"
)

type ServeCommand struct{}

func (this *ServeCommand) Short() string {
    return "serve application"
}

func (this *ServeCommand) Setup(command *cobra.Command) {}

func (this *ServeCommand) Run() lib.CommandRunner {
    return func(
        middleware middlewares.Middlewares,
        env lib.Env,
        requestHandler lib.RequestHandler,
        route routes.Routes,
        logger lib.Logger,
    ) {
        logger.Info("Init")
        middleware.Setup()
        route.Setup()
        logger.Info("Running server")
        if env.ServerPort == "" {
            _ = requestHandler.Gin.Run()
        } else {
            _ = requestHandler.Gin.Run(":" + env.ServerPort)
        }
    }
}

func NewServeCommand() *ServeCommand {
    return &ServeCommand{}
}
