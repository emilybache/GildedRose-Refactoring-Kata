;;; Class ITEM

(define-record-type item (make-item name sell-in quality) item? name sell-in quality)
;; define-record-type ... SRFI-9
;; creates make-item, item?, item-name, item-sell-in, item-quality, item-name-set!, item-sell-in-set!, item-quality-set!

(define (item-to-string item)
    (string-append (item-name item)
                   ", "
                   (number->string (item-sell-in item))
                   ", "
                   (number->string (item-quality item))))

;;; GILDED-ROSE

(define (update-quality items)
    (for-each
        (lambda (item)
            (if (and (not (string=? (item-name item) "Aged Brie"))
                     (not (string=? (item-name item) "Backstage passes to a TAFKAL80ETC concert")))
                (if (> (item-quality item) 0)
                    (if (not (string=? (item-name item) "Sulfuras, Hand of Ragnaros"))
                        (item-quality-set! item (- (item-quality item) 1))))
                (cond ((< (item-quality item) 50)
                       (item-quality-set! item (+ (item-quality item) 1))
                       (if (string=? (item-name item) "Backstage passes to a TAFKAL80ETC concert")
                            (if (< (item-sell-in item) 11)
                                (cond ((< (item-quality item) 50)
                                       (item-quality-set! item (+ (item-quality item) 1))
                                       (if (< (item-sell-in item) 6)
                                           (if (< (item-quality item) 50)
                                               (item-quality-set! item (+ (item-quality item) 1)))))))))))

            (if (not (string=? (item-name item) "Sulfuras, Hand of Ragnaros"))
                (item-sell-in-set! item (- (item-sell-in item) 1)))

            (if (< (item-sell-in item) 0)
                (if (not (string=? (item-name item) "Aged Brie"))
                    (if (not (string=? (item-name item) "Backstage passes to a TAFKAL80ETC concert"))
                        (if (> (item-quality item) 0)
                            (if (not (string=? (item-name item) "Sulfuras, Hand of Ragnaros"))
                                (item-quality-set! item (- (item-quality item) 1))))
                        (item-quality-set! item (- (item-quality item) (item-quality item))))
                    (if (< (item-quality item) 50)
                        (item-quality-set! item (+ (item-quality item) 1))))))
        items))
