import strformat

type
  Item* = object
    name*: string
    sellIn*, quality*: int
  
proc initItem*(name: string, sellIn, quality: int): Item =
  Item(name: name, sellIn: sellIn, quality: quality)

proc `$`*(item: Item): string =
  &"{item.name}, {item.sellIn}, {item.quality}"
