package controllers

import (
    "net/http"
    "testing"

    "github.com/stretchr/testify/assert"

    "github.com/emilybache/gildedrose-refactoring-kata/lib"
)

func TestSwagger(t *testing.T) {
    runTestCase(t, func(
        handler lib.RequestHandler,
    ) {
        req, _ := http.NewRequest("GET", "/swagger/index.html", nil)

        w := executeRequest(handler, req)
        assert.Equal(t, 404, w.Code)
    })
}
