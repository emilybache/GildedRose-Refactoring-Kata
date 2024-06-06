import cli
import gilded_rose_item.{type GildedRose, type Item, Item}
import gleam/list

pub fn update_quality(inventory: GildedRose) -> GildedRose {
  let update_quality_item = fn(item: Item) {
    let new_quality = case
      item.name != "Aged Brie"
      && item.name != "Backstage passes to a TAFKAL80ETC concert"
    {
      True -> {
        case item.quality > 0 {
          True ->
            case item.name != "Sulfuras, Hand of Ragnaros" {
              True -> item.quality - 1
              False -> item.quality
            }
          False -> item.quality
        }
      }
      False -> {
        case item.quality < 50 {
          True ->
            item.quality
            + 1
            + case item.name == "Backstage passes to a TAFKAL80ETC concert" {
              True ->
                case item.sell_in < 11 {
                  True -> {
                    case item.quality < 49 {
                      True ->
                        1
                        + case item.sell_in < 6 {
                          True ->
                            case item.quality < 48 {
                              True -> 1
                              False -> 0
                            }
                          False -> 0
                        }
                      False -> 0
                    }
                  }
                  False -> 0
                }
              False -> 0
            }
          False -> item.quality
        }
      }
    }

    let new_sell_in = case item.name != "Sulfuras, Hand of Ragnaros" {
      True -> item.sell_in - 1
      False -> item.sell_in
    }

    case new_sell_in < 0 {
      True ->
        case item.name != "Aged Brie" {
          True ->
            case item.name != "Backstage passes to a TAFKAL80ETC concert" {
              True ->
                case new_quality > 0 {
                  True ->
                    case item.name != "Sulfuras, Hand of Ragnaros" {
                      True ->
                        Item(
                          ..item,
                          sell_in: new_sell_in,
                          quality: new_quality - 1,
                        )
                      False -> Item(..item, sell_in: new_sell_in, quality: 80)
                    }
                  False ->
                    Item(..item, sell_in: new_sell_in, quality: new_quality)
                }
              False ->
                Item(
                  ..item,
                  sell_in: new_sell_in,
                  quality: new_quality - new_quality,
                )
            }
          False ->
            case new_quality < 50 {
              True ->
                Item(..item, sell_in: new_sell_in, quality: new_quality + 1)
              False -> Item(..item, sell_in: new_sell_in, quality: 50)
            }
        }
      False ->
        case item.name == "Sulfuras, Hand of Ragnaros" {
          True -> Item(..item, sell_in: new_sell_in, quality: 80)
          False ->
            case new_quality > 50 {
              True -> Item(..item, sell_in: new_sell_in, quality: 50)
              False -> Item(..item, sell_in: new_sell_in, quality: new_quality)
            }
        }
    }
  }
  list.map(inventory, update_quality_item)
}

pub fn main() {
  cli.run_cli_app(update_quality)
}
