rm(list=ls())

source('gilded_rose.R')

writeLines('OMGHAI!')

items <- list(
  item('+5 Dexterity Vest', 10, 20),
  item('Aged Brie', 2, 0),
  item('Elixir of the Mongoose', 5, 7),
  item('Sulfuras, Hand of Ragnaros', 0, 80),
  item('Sulfuras, Hand of Ragnaros', -1, 80),
  item('Backstage passes to a TAFKAL80ETC concert', 15, 20),
  item('Backstage passes to a TAFKAL80ETC concert', 10, 49),
  item('Backstage passes to a TAFKAL80ETC concert', 5, 49),
  # This Conjured item does not work properly yet
  item('Conjured Mana Cake', 3, 6)
)

days <- 2
for (day in 0:days) {
  writeLines(paste('-------- day ', day, ' --------', sep=''))
  writeLines('name, sellIn, quality')
  lapply(items, 
    function(item) {
      writeLines(as.character(item))
    }
  )
  writeLines('')
  items <- update_quality(items)
}

rm('day', 'days', 'items')
