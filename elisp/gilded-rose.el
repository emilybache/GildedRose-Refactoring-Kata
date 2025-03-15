(defun make-item (name sell-in quality)
  "Create an item with NAME, SELL-IN, and QUALITY."
  (list :name name :sell-in sell-in :quality quality))

(defun update-quality (items)
  (dolist (item items)
    (if (and (not (string= (plist-get item :name) "Aged Brie"))
             (not (string= (plist-get item :name) "Backstage passes to a TAFKAL80ETC concert")))

        (when (> (plist-get item :quality) 0)
          (when (not (string= (plist-get item :name) "Sulfuras, Hand of Ragnaros"))
            (setf (plist-get item :quality) (1- (plist-get item :quality)))))

        (when (< (plist-get item :quality) 50)
          (setf (plist-get item :quality) (1+ (plist-get item :quality)))
          (when (string= (plist-get item :name) "Backstage passes to a TAFKAL80ETC concert")
            (when (< (plist-get item :sell-in) 11)
              (when (< (plist-get item :quality) 50)
                (setf (plist-get item :quality) (1+ (plist-get item :quality)))))
            (when (< (plist-get item :sell-in) 6)
              (when (< (plist-get item :quality) 50)
                (setf (plist-get item :quality) (1+ (plist-get item :quality)))))))

    (when (not (string= (plist-get item :name) "Sulfuras, Hand of Ragnaros"))
      (setf (plist-get item :sell-in) (1- (plist-get item :sell-in))))

    (when (< (plist-get item :sell-in) 0)
      (if (not (string= (plist-get item :name) "Aged Brie"))
          (if (not (string= (plist-get item :name) "Backstage passes to a TAFKAL80ETC concert"))
              (when (> (plist-get item :quality) 0)
                (when (not (string= (plist-get item :name) "Sulfuras, Hand of Ragnaros"))
                  (setf (plist-get item :quality) (1- (plist-get item :quality)))))
            (setf (plist-get item :quality) 0))
        (when (< (plist-get item :quality) 50)
          (setf (plist-get item :quality) (1+ (plist-get item :quality)))))))))

(provide 'gilded-rose)
