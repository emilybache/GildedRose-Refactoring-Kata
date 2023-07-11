package models

import (
    "fmt"
    "sync"
)

type ItemModel struct {
    Name     string `json:"name" binding:"required"`
    SellIn   int    `json:"sellIn" binding:"required"`
    Quality  int    `json:"quality" binding:"required,gte=0"`
}

type Item struct {
    Model   *ItemModel
    Mutex   *sync.RWMutex
}

func NewItem(model *ItemModel) *Item {
    return &Item{
        Model: model,
        Mutex: &sync.RWMutex{},
    }
}

func (this Item) String() string {
    return this.Model.String()
}

func (this ItemModel) String() string {
    return fmt.Sprintf("%s, %d, %d", this.Name, this.SellIn, this.Quality)
}
