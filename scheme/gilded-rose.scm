;;; Class ITEM

(define-structure item
    (name sell-in quality))
;; defines make-item, item?, item-name, item-sell-in, item-quality, set-item-name!, set-item-sell-in!, set-item-quality!

(define (item-to-string item)
    (string-append (item-name item)
                   ", "
                   (number->string (item-sell-in item))
                   ", "
                   (number->string (item-quality item))))

;;; Class GILDED-ROSE

(define (update-quality items)
    (for-each
        (lambda (item)
            (if (and (not (string-= (item-name item) "Aged Brie"))
                     (not (string-= (item-name item) "Backstage passes to a TAFKAL80ETC concert")))
                (if (> (item-quality item) 0)
                    (if (not (string-= (item-name item) "Sulfuras, Hand of Ragnaros"))
                        (set-item-quality! item (- (item-quality item) 1))))
                (cond ((< (item-quality item) 50)
                       (set-item-quality! item (+ (item-quality item) 1))
                       (if (string-= (item-name item) "Backstage passes to a TAFKAL80ETC concert")
                            (if (< sell-in 11)
                                (if (< (item-quality item) 50)
                                    (set-item-quality! item (+ (item-quality item) 1))))
                            (if (< sell-in 6)
                                (if (< (item-quality item) 50)
                                    (set-item-quality! item (+ (item-quality item) 1))))))))

            (if (not (string-= (item-name item) "Sulfuras, Hand of Ragnaros"))
                (set-item-sell-in! item (- (item-sell-in item) 1)))

            (if (< (item-sell-in item) 0)
                (if (not (string-= (item-name item) "Aged Brie"))
                    (if (not (string-= (item-name item) "Backstage passes to a TAFKAL80ETC concert"))
                        (if (> (item-quality item) 0)
                            (if (not (string-= (item-name item) "Sulfuras, Hand of Ragnaros"))
                                (set-item-quality! item (- (item-quality item) 1))))
                        (set-item-quality! item (- (item-quality item) (item-quality item))))
                    (if (< (item-quality item) 50)
                        (set-item-quality! item (+ (item-quality item) 1))))))
      items))
