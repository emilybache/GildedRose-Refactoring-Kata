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

    if item.name != "Aged Brie" && item.name != "Backstage passes to a TAFKAL80ETC concert" {
      if item.quality > 0 {
        if item.name != "Sulfuras, Hand of Ragnaros" {
          newItem := {...item, quality: item.quality - 1}
        }
      }
    } else if item.quality < 50 {
      newItem := {...item, quality: item.quality + 1}

      if item.name == "Backstage passes to a TAFKAL80ETC concert" {
        if item.sellIn < 11 {
          if item.quality < 50 {
            newItem := {...item, quality: item.quality + 1}
          }
        }

        if item.sellIn < 6 {
          if item.quality < 50 {
            newItem := {...item, quality: item.quality + 1}
          }
        }
      }
    }

    if item.name != "Sulfuras, Hand of Ragnaros" {
      newItem := {...item, sellIn: item.sellIn - 1}
    }

    if item.sellIn < 0 {
      if item.name != "Aged Brie" {
        if item.name != "Backstage passes to a TAFKAL80ETC concert" {
          if item.quality > 0 {
            if item.name != "Sulfuras, Hand of Ragnaros" {
              newItem := {...item, quality: item.quality - 1}
            }
          }
        } else {
          newItem := {...item, quality: 0}
        }
      } else if item.quality < 50 {
        newItem := {...item, quality: item.quality + 1}
      }
    }

    newItem.contents
  })
}
