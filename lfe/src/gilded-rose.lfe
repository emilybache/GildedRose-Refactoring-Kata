;; Hi and welcome to team Gilded Rose. As you know, we are a small inn
;; with a prime location in a prominent city ran by a friendly
;; innkeeper named Allison. We also buy and sell only the finest goods.
;; Unfortunately, our goods are constantly degrading in (item-quality item) as they
;; approach their sell by date. We have a system in place that updates
;; our inventory for us. It was developed by a no-nonsense type named
;; Leeroy, who has moved on to new adventures. Your task is to add the
;; new feature to our system so that we can begin selling a new
;; category of items.

;; First an introduction to our system:
;; All items have a SellIn value which denotes the number of days we have to sell the item
;; All items have a Quality value which denotes how valuable the item is
;; At the end of each day our system lowers both values for every item
;; Pretty simple, right? Well this is where it gets interesting:
;; Once the sell by date has passed, Quality degrades twice as fast
;; The Quality of an item is never negative
;; "Aged Brie" actually increases in Quality the older it gets
;; The Quality of an item is never more than 50
;; "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
;; "Backstage passes", like aged brie, increases in Quality as it's
;;   SellIn value approaches; Quality increases by 2 when there are 10
;;   days or less and by 3 when there are 5 days or less but Quality
;;   drops to 0 after the concert

;; We have recently signed a supplier of conjured items. This requires an update to our system:
;; "Conjured" items degrade in Quality twice as fast as normal items
;; Feel free to make any changes to the UpdateQuality method and add
;; any new code as long as everything still works correctly. However,
;; do not alter the Item class or Items property as those belong to the
;; goblin in the corner who will insta-rage and one-shot you as he
;; doesn't believe in shared code ownership (you can make the
;; UpdateQuality method and Items property static if you like, we'll
;; cover for you).
;; Just for clarification, an item can never have its Quality increase
;; above 50, however "Sulfuras" is a legendary item and as such its
;; Quality is 80 and it never alters.

;; https://github.com/emilybache/GildedRose-Refactoring-Kata

;; LFE version by Manfred Bergmann, 2022

;;; ================================================================
;;; Code

(defmodule gilded-rose
  (export all))

(include-lib "include/gilded-rose-item.lfe")

;; update-quality

(defun update-quality (items)
  (lists:map #'update-item/1 items))

(defun update-item (item)
  (let* ((item (if (and (not (string:equal (item-name item)
                                           "Aged Brie"))
                        (not (string:equal (item-name item)
                                           "Backstage passes to a TAFKAL80ETC concert")))
                 (if (> (item-quality item) 0)
                   (if (not (string:equal (item-name item)
                                          "Sulfuras, Hand of Ragnaros"))
                     (set-item-quality item (- (item-quality item) 1))
                     item)
                   item)
                 (if (< (item-quality item) 50)
                   (set-item-quality item (+ (item-quality item) 1))
                   (if (string:equal (item-name item)
                                     "Backstage passes to a TAFKAL80ETC concert")
                     (if (< (item-sellin item) 11)
                       (if (< (item-quality item) 50)
                         (set-item-quality item (+ (item-quality item) 1))
                         item)
                       item)
                     (if (< (item-sellin item) 6)
                       (if (< (item-quality item) 50)
                         (set-item-quality item (+ (item-quality item) 1))
                         item)
                       item)))))
         (item (if (not (string:equal (item-name item)
                                      "Sulfuras, Hand of Ragnaros"))
                 (set-item-sellin item (- (item-sellin item) 1))
                 item)))
    (if (< (item-sellin item) 0)
      (if (not (string:equal (item-name item)
                             "Aged Brie"))
        (if (not (string:equal (item-name item)
                               "Backstage passes to a TAFKAL80ETC concert"))
          (if (> (item-quality item) 0)
            (if (not (string:equal (item-name item)
                                   "Sulfuras, Hand of Ragnaros"))
              (set-item-quality item (- (item-quality item) 1))
              item)
            item)
          (set-item-quality item (- (item-quality item) (item-quality item))))
        (if (< (item-quality item) 50)
          (set-item-quality item (+ (item-quality item) 1))
          item))
      item)))

;;; Example

(defun print-item (item)
  (io:format "~s ~b ~b~n" (list (item-name item) (item-sellin item) (item-quality item))))

(defun print-day (day items)
  (io:format "-------- day ~b --------~n" (list day))
  (io:format "name, sellin, quality~n")
  (lists:foreach #'print-item/1 items)
  (io:format "~n"))

(defrecord gilded-rose items)

(defun run-gilded-rose (days)
  (io:format "OMGHAI!~n")
  (let ((items (list (make-item name "+5 Dexterity Vest" sellin 10 quality 20)
                     (make-item name "Aged Brie" sellin 2 quality 0)
                     (make-item name "Elixir of the Mongoose" sellin 5 quality 7)
                     (make-item name "Sulfuras, Hand of Ragnaros" sellin 0 quality 80)
                     (make-item name "Sulfuras, Hand of Ragnaros" sellin -1 quality 80)
                     (make-item name "Backstage passes to a TAFKAL80ETC concert" sellin 15 quality 20)
                     (make-item name "Backstage passes to a TAFKAL80ETC concert" sellin 10 quality 49)
                     (make-item name "Backstage passes to a TAFKAL80ETC concert" sellin 5 quality 49)
                     ;; this conjured item does not work properly yet
                     (make-item name "Conjured Mana Cake" sellin 3 quality 6))))

    (lists:foldl (lambda (day gr)
                   (print-day day (gilded-rose-items gr))
                   (make-gilded-rose items (update-quality (gilded-rose-items gr))))
                 (make-gilded-rose items items)
                 (lists:seq 0 days))))

