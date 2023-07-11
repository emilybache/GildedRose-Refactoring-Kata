package bootstrap

import (
    "github.com/emilybache/gildedrose-refactoring-kata/api/controllers"
    "github.com/emilybache/gildedrose-refactoring-kata/api/middlewares"
    "github.com/emilybache/gildedrose-refactoring-kata/api/routes"
    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/services"
    "go.uber.org/fx"
)

var CommonModules = fx.Options(
    controllers.Module,
    middlewares.Module,
    routes.Module,
    lib.Module,
    services.Module,
)
