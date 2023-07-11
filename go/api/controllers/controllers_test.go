package controllers

import (
    "net/http"
    "net/http/httptest"
    "testing"

    "github.com/stretchr/testify/require"

    "go.uber.org/fx"
    "go.uber.org/fx/fxtest"

    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/services"
    "github.com/emilybache/gildedrose-refactoring-kata/api/middlewares"
)

var TestCommonModules = fx.Options(
    Module,
    lib.Module,
    services.Module,
    middlewares.Module,
)

type TestCaseRunner interface{}

func setupTestCase(handler lib.RequestHandler, logger lib.Logger, itemController ItemController, swaggerController SwaggerController) {
    logger.Info("Setup test case")
    swaggerController.Setup(handler.Gin)
    itemController.Setup(handler.Gin)
}

func runTestCase(t *testing.T, runner TestCaseRunner) {
    app := fxtest.New(t, TestCommonModules, fx.Invoke(setupTestCase), fx.Invoke(runner))

    defer app.RequireStart().RequireStop()
    require.NoError(t, app.Err())
}

func executeRequest(handler lib.RequestHandler, req *http.Request) *httptest.ResponseRecorder {
    result := httptest.NewRecorder()
    handler.Gin.ServeHTTP(result, req)
    return result
}
