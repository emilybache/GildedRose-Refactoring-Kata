(defun make-item (name sell-in quality)
  "Create an item with NAME, SELL-IN, and QUALITY."
  (list :name name :sell-in sell-in :quality quality))

(defun item-name (item)
  "Return the name of ITEM."
  (plist-get item :name))

(defun item-sell-in (item)
  "Return the sell-in value of ITEM."
  (plist-get item :sell-in))

(defun item-quality (item)
  "Return the quality of ITEM."
  (plist-get item :quality))

(defun update-quality (item)
  "Update the quality of the ITEM according to the Gilded Rose rules."
  (let ((quality (item-quality item))
        (sell-in (item-sell-in item)))
    (cond
     ;; Aged Brie
     ((string= (item-name item) "Aged Brie")
      (setf (nth 2 item) (min 50 (1+ quality))))
     
     ;; Backstage passes
     ((string= (item-name item) "Backstage passes")
      (cond
       ((> sell-in 10) (setf (nth 2 item) quality))
       ((and (<= sell-in 10) (> sell-in 5)) (setf (nth 2 item) (min 50 (+ quality 2))))
       ((and (<= sell-in 5) (> sell-in 0)) (setf (nth 2 item) (min 50 (+ quality 3))))
       (t (setf (nth 2 item) 0))))
     
     ;; Sulfuras
     ((string= (item-name item) "Sulfuras")
      (setf (nth 2 item) quality))

(provide 'gilded-rose)
