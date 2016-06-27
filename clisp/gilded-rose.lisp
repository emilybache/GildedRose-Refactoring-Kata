; Hi and welcome to team Gilded Rose. As you know, we are a small inn
; with a prime location in a prominent city ran by a friendly
; innkeeper named Allison. We also buy and sell only the finest goods.
; Unfortunately, our goods are constantly degrading in quality as they
; approach their sell by date. We have a system in place that updates
; our inventory for us. It was developed by a no-nonsense type named
; Leeroy, who has moved on to new adventures. Your task is to add the
; new feature to our system so that we can begin selling a new
; category of items.
; First an introduction to our system:
; All items have a SellIn value which denotes the number of days we have to sell the item
; All items have a Quality value which denotes how valuable the item is
; At the end of each day our system lowers both values for every item
; Pretty simple, right? Well this is where it gets interesting:
; Once the sell by date has passed, Quality degrades twice as fast
; The Quality of an item is never negative
; "Aged Brie" actually increases in Quality the older it gets
; The Quality of an item is never more than 50
; "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
; "Backstage passes", like aged brie, increases in Quality as it's
;   SellIn value approaches; Quality increases by 2 when there are 10
;   days or less and by 3 when there are 5 days or less but Quality
;   drops to 0 after the concert
; We have recently signed a supplier of conjured items. This requires an update to our system:
; "Conjured" items degrade in Quality twice as fast as normal items
; Feel free to make any changes to the UpdateQuality method and add
; any new code as long as everything still works correctly. However,
; do not alter the Item class or Items property as those belong to the
; goblin in the corner who will insta-rage and one-shot you as he
; doesn't believe in shared code ownership (you can make the
; UpdateQuality method and Items property static if you like, we'll
; cover for you).
; Just for clarification, an item can never have its Quality increase
; above 50, however "Sulfuras" is a legendary item and as such its
; Quality is 80 and it never alters.

; https://github.com/emilybache/GildedRose-Refactoring-Kata

; Common Lisp version: Rainer Joswig, joswig@lisp.de, 2016

; Example from the command line:
; sbcl --script gildedrose.lisp 10

;;; ================================================================
;;; Code

(defpackage "GILDED-ROSE"
  (:use "CL"))

(in-package "GILDED-ROSE")


;;; Class ITEM

(defclass item ()
  ((name    :initarg :name    :type string)
   (sell-in :initarg :sell-in :type integer)
   (quality :initarg :quality :type integer)))

(defmethod to-string ((i item))
  (with-slots (name quality sell-in) i
    (format nil "~a, ~a, ~a" name sell-in quality)))

;;; Class GILDED-ROSE

(defclass gilded-rose ()
  ((items :initarg :items)))

(defmethod update-quality ((gr gilded-rose))
  (with-slots (items) gr
    (dotimes (i (length items))
      (with-slots (name quality sell-in)
          (elt items i)
        (if (and (not (equalp name "Aged Brie"))
                 (not (equalp name "Backstage passes to a TAFKAL80ETC concert")))
            (if (> quality 0)
                (if (not (equalp name "Sulfuras, Hand of Ragnaros"))
                    (setf quality (- quality 1))))
          (when (< quality 50)
            (setf quality (+ quality 1))
            (when (equalp name "Backstage passes to a TAFKAL80ETC concert")
              (if (< sell-in 11)
                  (if (< quality 50)
                      (setf quality (+ quality 1))))
              (if (< sell-in 6)
                  (if (< quality 50)
                      (setf quality (+ quality 1)))))))

        (if (not (equalp name "Sulfuras, Hand of Ragnaros"))
            (setf sell-in (- sell-in 1)))

        (if (< sell-in 0)
            (if (not (equalp name "Aged Brie"))
                (if (not (equalp name "Backstage passes to a TAFKAL80ETC concert"))
                    (if (> quality 0)
                        (if (not (equalp name "Sulfuras, Hand of Ragnaros"))
                            (setf quality (- quality 1))))
                  (setf quality (- quality quality)))
              (if (< quality 50)
                  (setf quality (+ quality 1)))))))))

;;; Example

(defun run-gilded-rose ()
  (write-line "OMGHAI!")
  (let* ((descriptions '(("+5 Dexterity Vest"                         10 20)
                         ("Aged Brie"                                  2  0)
                         ("Elixir of the Mongoose"                     5  7)
                         ("Sulfuras, Hand of Ragnaros"                 0 80)
                         ("Sulfuras, Hand of Ragnaros"                -1 80)
                         ("Backstage passes to a TAFKAL80ETC concert" 15 20)
                         ("Backstage passes to a TAFKAL80ETC concert" 10 49)
                         ("Backstage passes to a TAFKAL80ETC concert"  5 49)
                         ;; this conjured item does not work properly yet
                         ("Conjured Mana Cake"                         3  6)))
         (items (loop for (name sell-in quality) in descriptions
                      collect (make-instance 'item
                                             :name    name
                                             :sell-in sell-in
                                             :quality quality)))
         (app (make-instance 'gilded-rose :items items))
         (days 2))
    #+sbcl
    (if (second sb-ext:*posix-argv*)
        (setf days (parse-integer (second sb-ext:*posix-argv*))))
    #+lispworks
    (if (fourth sys:*line-arguments-list*)
        (setf days (parse-integer (fourth sys:*line-arguments-list*))))
    (dotimes (i days)
      (format t "-------- day ~a --------~%" i)
      (format t "name, sell-in, quality~%")
      (dolist (item items)
        (write-line (to-string item)))
      (terpri)
      (update-quality app))))

(run-gilded-rose)

;;; ================================================================
;;; EOF
