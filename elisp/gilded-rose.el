(defun make-item (name sell-in quality)
  "Create an item with NAME, SELL-IN, and QUALITY."
  (list :name name :sell-in sell-in :quality quality))

(defun update-quality (items)
  (dolist (item items)
    (if (string= (plist-get item :name) "Aged Brie")
        (plist-put item :quality (min 50 (1+ (plist-get item :quality))))
      
      (if (string= (plist-get item :name) "Backstage passes")
          (if (> (plist-get item :sell-in) 10)
              (plist-put item :quality (plist-get item :quality))
            
            (if (and (<= (plist-get item :sell-in) 10) (> (plist-get item :sell-in) 5))
                (plist-put item :quality (min 50 (+ (plist-get item :quality) 2)))
              
              (if (and (<= (plist-get item :sell-in) 5) (> (plist-get item :sell-in) 0))
                  (plist-put item :quality (min 50 (+ (plist-get item :quality) 3)))
                
                (plist-put item :quality 0))))
        
        (if (string= (plist-get item :name) "Sulfuras")
            (plist-put item :quality (plist-get item :quality))
          
          (plist-put item :quality (max 0 (1- (plist-get item :quality)))))))))

(provide 'gilded-rose)
