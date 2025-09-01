rockspec_format = "3.0"
package = "gildedrose"
version = "dev-1"
source = {
   url = "https://github.com/emilybache/GildedRose-Refactoring-Kata.git"
}
description = {
   license = "MIT"
}
dependencies = {
   "lua >= 5.1, < 5.5"
}
test_dependencies = {
   "busted >= 2.2"
}
test = {
   type = "command",
   command = "busted"
}
