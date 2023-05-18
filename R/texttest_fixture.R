rm(list=ls())

source('gilded_rose.R')

writeLines('OMGHAI!')

items <- list(
  item('Sports Memorabilia', 10, 20),
  item('Aged Cheese', 2, 0),
  item('Coffee Table Book', 5, 7),
  item('Fine Italian Silk', 0, 80),
  item('Fine Italian Silk', -1, 80),
  item('Backstage passes to a concert', 15, 20),
  item('Backstage passes to a concert', 10, 49),
  item('Backstage passes to a concert', 5, 49),
  # This Baked item does not work properly yet
  item('Baked Chocolate Cake', 3, 6)
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
