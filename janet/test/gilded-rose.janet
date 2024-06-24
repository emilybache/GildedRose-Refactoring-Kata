(import ../src/main :as shop)
(use judge)

(do
  (def items [(shop/item "foo" 0 0)])
  (shop/update-quality items)
  (test ((first items) :name) "fixme"))
