(defun make-item (name sell-in quality)
  "Create an item with NAME, SELL-IN, and QUALITY."
  (list :name name :sell-in sell-in :quality quality))

(defun update-quality (item)
  (let* ((quality (plist-get item :quality))
         (sell-in (plist-get item :sell-in))
         (name (plist-get item :name)))
    (cond
     ((string= name "Aged Brie")
      (setf (nth 2 item) (min 50 (1+ quality))))
     ((string= name "Backstage passes")
      (cond
       ((> sell-in 10) (setf (nth 2 item) quality))
       ((and (<= sell-in 10) (> sell-in 5)) (setf (nth 2 item) (min 50 (+ quality 2))))
       ((and (<= sell-in 5) (> sell-in 0)) (setf (nth 2 item) (min 50 (+ quality 3))))
       (t (setf (nth 2 item) 0))))
     ((string= name "Sulfuras")
      (setf (nth 2 item) quality))
     (t
      (setf (nth 2 item) (max 0 (1- quality)))))))

(provide 'gilded-rose)
