package bootstrap

import (
    "github.com/emilybache/gildedrose-refactoring-kata/cmd"
    "github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
    Use:   "gilded-rose",
    Short: "Clean architecture using gin framework",
    Long: `
This is a command runner or cli for api architecture in golang.
Using this we can use underlying dependency injection container for running scripts.
Main advantage is that, we can use same services, repositories, infrastructure present in the application itself`,
    TraverseChildren: true,
}

type App struct {
    *cobra.Command
}

func NewApp() App {
    command := App{
        Command: rootCmd,
    }
    command.AddCommand(cmd.GetSubCommands(CommonModules)...)
    return command
}

var RootApp = NewApp()
