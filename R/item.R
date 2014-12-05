item <- function(name, sell_in, quality) {
  newItem <- list(name=name, sell_in=sell_in, quality=quality)
  class(newItem) <- 'item'
  newItem
}

as.character.item <- function(item) {
  paste(item$name, ", ", item$sell_in, ", ", item$quality, sep='')
}

print.item <- function(item) {
  print.default(as.character(item))
}
