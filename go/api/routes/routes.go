package routes

import "go.uber.org/fx"

var Module = fx.Options(
    fx.Provide(NewItemControllerRoutes),
    fx.Provide(NewSwaggerRoutes),
    fx.Provide(NewRoutes),
)

type Routes []Route

type Route interface {
    Setup()
}

func NewRoutes(
    itemControllerRoutes ItemControllerRoutes,
    swaggerRoutes        SwaggerRoutes,
) Routes {
    return Routes{
        itemControllerRoutes,
        swaggerRoutes,
    }
}

func (r Routes) Setup() {
    for _, route := range r {
        route.Setup()
    }
}
