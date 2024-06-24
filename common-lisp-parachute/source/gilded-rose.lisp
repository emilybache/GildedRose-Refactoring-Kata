;; https://github.com/emilybache/GildedRose-Refactoring-Kata

;; Common Lisp version: Rainer Joswig, joswig@lisp.de, 2016
;; Minor modifications (independent of CL impl.): Manfred Bergmann, 2022
;; Adaption to asdf: Nico Simoski, 2023

;;; ================================================================
;;; Code

(in-package :gilded-rose)

;;; Class ITEM

(defclass item ()
  ((name    :initarg :name    :type string)
   (sell-in :initarg :sell-in :type integer)
   (quality :initarg :quality :type integer)))

(defmethod to-string ((i item))
  (with-slots (name quality sell-in) i
    (format nil "~a, ~a, ~a" name sell-in quality)))

;;; Class gilded-rose

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

(defun run-gilded-rose (days)
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
         (items (loop :for (name sell-in quality) :in descriptions
                      :collect (make-instance 'item
                                              :name name
                                              :sell-in sell-in
                                              :quality quality)))
         (app (make-instance 'gilded-rose :items items)))
    (dotimes (i days)
      (format t "-------- day ~a --------~%" i)
      (format t "name, sell-in, quality~%")
      (dolist (item items)
        (write-line (to-string item)))
      (terpri)
      (update-quality app))))

;;; ================================================================
;;; EOF
