package controllers

import (
    "net/http"
    "net/http/httptest"
    //"strings"
    "testing"

    "github.com/stretchr/testify/assert"
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

func setupTestCase(handler lib.RequestHandler, logger lib.Logger, itemController ItemController) {
    logger.Info("Setup test case")
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

func TestStatus(t *testing.T) {
    runTestCase(t, func(
        handler lib.RequestHandler,
    ) {
        req, _ := http.NewRequest("GET", "/status", nil)

        w := executeRequest(handler, req)
        assert.Equal(t, 200, w.Code)
        assert.Equal(t, `{"status":"ok"}`, w.Body.String())
    })
}
