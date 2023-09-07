module Item = {
  type t = {
    name: string,
    sellIn: int,
    quality: int,
  }

  let make = (~name, ~sellIn, ~quality): t => {
    name,
    sellIn,
    quality,
  }
}

let updateQuality = (items: array<Item.t>) => {
  items->Js.Array2.map(item => {
    let newItem = ref(item)

    if (
      newItem.contents.name != "Aged Brie" &&
        newItem.contents.name != "Backstage passes to a TAFKAL80ETC concert"
    ) {
      if newItem.contents.quality > 0 {
        if newItem.contents.name != "Sulfuras, Hand of Ragnaros" {
          newItem := {...newItem.contents, quality: newItem.contents.quality - 1}
        }
      }
    } else if newItem.contents.quality < 50 {
      newItem := {...newItem.contents, quality: newItem.contents.quality + 1}

      if newItem.contents.name == "Backstage passes to a TAFKAL80ETC concert" {
        if newItem.contents.sellIn < 11 {
          if newItem.contents.quality < 50 {
            newItem := {...newItem.contents, quality: newItem.contents.quality + 1}
          }
        }

        if newItem.contents.sellIn < 6 {
          if newItem.contents.quality < 50 {
            newItem := {...newItem.contents, quality: newItem.contents.quality + 1}
          }
        }
      }
    }

    if newItem.contents.name != "Sulfuras, Hand of Ragnaros" {
      newItem := {...newItem.contents, sellIn: newItem.contents.sellIn - 1}
    }

    if newItem.contents.sellIn < 0 {
      if newItem.contents.name != "Aged Brie" {
        if newItem.contents.name != "Backstage passes to a TAFKAL80ETC concert" {
          if newItem.contents.quality > 0 {
            if newItem.contents.name != "Sulfuras, Hand of Ragnaros" {
              newItem := {...newItem.contents, quality: newItem.contents.quality - 1}
            }
          }
        } else {
          newItem := {
              ...newItem.contents,
              quality: newItem.contents.quality - newItem.contents.quality,
            }
        }
      } else if newItem.contents.quality < 50 {
        newItem := {...newItem.contents, quality: newItem.contents.quality + 1}
      }
    }

    newItem.contents
  })
}
