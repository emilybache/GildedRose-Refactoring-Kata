source('gilded_rose.R')

test.foo <- function() {
  items <- list( item('foo', 0, 0) )
  items <- update_quality(items)
  checkEquals('fixme', items[[1]]$name);
}
