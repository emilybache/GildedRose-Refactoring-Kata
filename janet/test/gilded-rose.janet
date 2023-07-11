(import ../gilded-rose/main)
(use judge)

(do
  (def items 
    [@{ :name "foo" :quality 3 :sell-in 5}
     @{ :name "bar" :quality 6 :sell-in 6}])
  (main/update-quality items)
  (def found (find |(= ($ :quality) 2) items))
  (test (found :name) "fixme"))