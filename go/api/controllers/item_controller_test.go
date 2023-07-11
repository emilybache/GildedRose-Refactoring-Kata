package controllers

import (
    "net/http"
    "testing"
    "strings"

    "github.com/stretchr/testify/assert"

    "github.com/emilybache/gildedrose-refactoring-kata/lib"
)

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

func TestNotFound(t *testing.T) {
    runTestCase(t, func(
        handler lib.RequestHandler,
    ) {
        req, _ := http.NewRequest("GET", "/notfoundurl", nil)

        w := executeRequest(handler, req)
        assert.Equal(t, 404, w.Code)
    })
}

func TestInvalidItem(t *testing.T) {
    runTestCase(t, func(
        handler lib.RequestHandler,
    ) {
        req, _ := http.NewRequest("POST", "/update_quality", strings.NewReader(`
        { "id": 1, "people": invalid
        `))
        req.Header = map[string][]string{"Content-Type": {"application/json"}}

        w := executeRequest(handler, req)
        assert.Equal(t, 400, w.Code)
    })
}
