package services

import (
    "testing"

    "github.com/stretchr/testify/require"

    "go.uber.org/fx"
    "go.uber.org/fx/fxtest"

    "github.com/emilybache/gildedrose-refactoring-kata/lib"
)

var TestCommonModules = fx.Options(
    Module,
    lib.Module,
)

type TestCaseRunner interface{}

func setupTestCase(logger lib.Logger) {
    logger.Info("Setup Service test case")
}

func runTestCase(t *testing.T, runner TestCaseRunner) {
    app := fxtest.New(t, TestCommonModules, fx.Invoke(setupTestCase), fx.Invoke(runner))

    defer app.RequireStart().RequireStop()
    require.NoError(t, app.Err())
}
