package services

import "go.uber.org/fx"

var Module = fx.Options(
    fx.Provide(NewNormalItemUpdateService),
    fx.Provide(NewAgedBrieItemUpdateService),
    fx.Provide(NewBackstagePassItemUpdateService),
    fx.Provide(NewSulfurasItemUpdateService),
    fx.Provide(NewItemUpdateServiceProvider),
)
