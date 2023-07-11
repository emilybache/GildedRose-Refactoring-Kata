package services

import "go.uber.org/fx"

var Module = fx.Options(
    fx.Provide(NewItemUpdateService),
    fx.Provide(NewItemUpdateServiceProvider),
)
