package main

import (
    "github.com/emilybache/gildedrose-refactoring-kata/bootstrap"
    "github.com/joho/godotenv"
)

// Application main entry point
func main() {
    _ = godotenv.Load()
    bootstrap.RootApp.Execute()
}
